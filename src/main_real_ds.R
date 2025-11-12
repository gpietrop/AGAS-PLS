library(GA)
library(optparse)

setwd("src")

source("my_fitness_ecsi.R")
source("ga_operators_ecsi.R")
source("utils.R")

variables <- c("eta1", "eta2", "eta3", "eta4", "eta5", "eta6", "eta7") # dir_names in the other script

# default hyperparameters
option_list <- list(
  make_option(c("--modeDim"), type="integer", default=100, help="Sample size"),
  make_option(c("--popSize"), type="integer", default=200, help="Population size"),
  make_option(c("--maxiter"), type="integer", default=100, help="Maximum iterations"),
  make_option(c("--pmutation"), type="double", default=1.0, help="Mutation rate"),
  make_option(c("--pcrossover"), type="double", default=0.8, help="Crossover rate"),
  make_option(c("--seed_start"), type="integer", default=0, help="First seed for the GA"),
  make_option(c("--seed_end"), type="integer", default=100, help="Last seed for the GA")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)


# Define and create results directory
hyperparam_subdir <- paste(opt$maxiter, opt$popSize, sep = "_")
results_dir <- file.path("..", "results", hyperparam_subdir, "ecsi")
# model_subdir <- paste(opt$model, opt$modeDim, sep="_")
# subdir <- file.path(results_dir, model_subdir)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# sat_matrix <- matrix(c(
#   0, 0, 0, 0, 0, 0,  # IMAG dependencies
#   1, 0, 0, 0, 0, 0,  # EXPE dependencies
#   0, 1, 0, 0, 0, 0,  # QUAL dependencies
#   0, 1, 1, 0, 0, 0,  # VAL dependencies
#   1, 1, 1, 1, 0, 0,  # SAT dependencies
#   1, 0, 0, 0, 1, 0   # LOY dependencies
# ), nrow = 6, byrow = TRUE)

trans_matrix <- matrix(c(
    0, 0, 0, 0, 0, 0, 0,  # IMAG dependencies
    1, 0, 0, 0, 0, 0, 0,  # EXPE dependencies
    0, 1, 0, 0, 0, 0, 0,  # QUAL dependencies
    0, 1, 1, 0, 0, 0, 0,  # VAL dependencies
    1, 1, 1, 1, 0, 0, 0,  # SAT dependencies
    1, 0, 0, 0, 1, 0, 1,  # LOY dependencies
    0, 0, 0, 0, 1, 0, 0  # COMP dependencies
  ), nrow = 7, byrow = TRUE)
  

# (sat) sat_string <- create_sem_model_string_from_matrix_satisfaction(sat_matrix)
# (sat) sat_out <- csem(.data = satisfaction,.model = sat_string) 
# (sat) sat_out <- csem(.data = satisfaction,.model = sat_string)
# (sat) sat_criteria <- calculateModelSelectionCriteria(sat_out, .by_equation = FALSE)
# (sat) bic_true <- sat_criteria$BIC

dataTrans <- read.csv("ds/data_ecsi.csv", sep = ";")
trans_string <- create_sem_model_string_from_matrix_trans(trans_matrix)
# outTrans=csem(.data = dataTrans,.model = modelTrans,.PLS_modes = 'modeA',.PLS_weight_scheme_inner = 'path',.resample_method="bootstrap")
outTrans=csem(.data = dataTrans,.model = trans_string,.PLS_modes = 'modeA',.PLS_weight_scheme_inner = 'centroid')

criteriaTrans <- calculateModelSelectionCriteria(outTrans, .by_equation = FALSE)
bic_true <- criteriaTrans$BIC

# GA function
run_ga <- function(seed) {
  message("Running GA for seed: ", seed)
  best_individuals_all <<- list()
  best_individual <<- NULL
  best_fitness <<- -Inf
  
  
  # Save the specific of the run
  hyperparams <- data.frame(
    "Population Size" = opt$popSize,
    "Max Iterations" = opt$maxiter,
    "Mutation Rate" = opt$pmutation,
    "Crossover Rate" = opt$pcrossover,
    "True BIC" = bic_true
  )
  write.csv(hyperparams, file.path(results_dir, paste0(seed, "_hyperparameters.csv")), row.names = FALSE, quote = FALSE)
  
  # Time the GA execution
  ga_time <- system.time({
    
    fitness_function <- myFitnessECSI
    mutation_function <- myMutationECSI
    
    ga_control <- ga(
      type = "binary",
      nBits = 7 * 7,
      popSize = opt$popSize,
      maxiter = opt$maxiter,
      pmutation = opt$pmutation,
      pcrossover = opt$pcrossover,
      fitness = function(x) fitness_function(x),
      elitism = TRUE,
      parallel = FALSE,
      seed = seed,
      mutation = mutation_function
    )
  })
  
  # Save the computation time to a file
  write.csv(data.frame(ComputationTime = ga_time['elapsed']), file.path(results_dir, paste0(seed, "_time.csv")), row.names = FALSE)
  
  # Save tracked best individual
  if (!is.null(best_individual) && length(best_individual) != 0) {
    best_ind_df <- as.data.frame(best_individual)
    colnames(best_ind_df) <- variables
    rownames(best_ind_df) <- variables
    write.csv(best_ind_df, file.path(results_dir, paste0(seed, "_best", ".csv")), row.names = TRUE)
  }
  
  # Save GA's best fitness
  write.csv(data.frame(Fitness = - ga_control@fitnessValue), file.path(results_dir, paste0(seed, "_fitness", ".csv")), row.names = FALSE)
}

print(bic_true)

# Loop over a range of seeds
lapply(opt$seed_start:opt$seed_end, run_ga)



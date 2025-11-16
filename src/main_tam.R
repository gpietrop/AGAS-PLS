library(GA)
library(optparse)

# setwd("src")

source("my_fitness_tam.R")
source("ga_operators_tam.R")
source("utils.R")

variables <- c("eta1", "eta2", "eta3", "eta4", "eta5") # dir_names in the other script

# default hyperparameters
option_list <- list(
  make_option(c("--modeDim"), type="integer", default=100, help="Sample size"),
  make_option(c("--popSize"), type="integer", default=200, help="Population size"),
  make_option(c("--maxiter"), type="integer", default=200, help="Maximum iterations"),
  make_option(c("--pmutation"), type="double", default=1.0, help="Mutation rate"),
  make_option(c("--pcrossover"), type="double", default=0.8, help="Crossover rate"),
  make_option(c("--seed_start"), type="integer", default=1, help="First seed for the GA"),
  make_option(c("--seed_end"), type="integer", default=100, help="Last seed for the GA")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)


# Define and create results directory
hyperparam_subdir <- paste(opt$maxiter, opt$popSize, sep = "_")
results_dir <- file.path("..", "results", hyperparam_subdir, "tam")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


modelTAM='  
# strucutral model
  USEF ~ EOI
  ATT ~ EOI + USEF
  BI ~ ATT + USEF
  USE ~ BI + ATT
# outer model - composites
  EOI <~ EOU1 + EOU2 + EOU3 + EOU4 + EOU5
  USEF <~ USEF1 + USEF2 + USEF3 + USEF4 + USEF5
  BI <~ BI1 + BI2 +  BI3   
  ATT <~ ATT1 + ATT2 + ATT3 + ATT4 + ATT5
  USE <~ USE1 + USE2 + USE3 + USE4 
'

tam_matrix <- matrix(c(
    0, 0, 0, 0, 0,  # EOI dependencies
    1, 0, 0, 0, 0,  # USEF dependencies
    1, 1, 0, 0, 0,  # ATT dependencies
    0, 1, 1, 0, 0,  # BI dependencies
    0, 0, 1, 1, 0   # USE dependencies
  ), nrow = 5, byrow = TRUE)
  
dati_TAM <-  read.csv("ds/Data_TAM.txt", sep=";", stringsAsFactors=TRUE)
tam_string <- create_sem_model_string_from_matrix_tam(tam_matrix)

outTAM=csem(.data = dati_TAM,.model = modelTAM,.PLS_modes = 'modeA')
outTam=csem(.data = dati_TAM,.model = tam_string,.PLS_modes = 'modeA')

criteriaTam <- calculateModelSelectionCriteria(outTam, .by_equation = FALSE)
bic_true <- criteriaTam$BIC

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
    
    fitness_function <- myFitnessTAM
    mutation_function <- myMutationTAM
    
    ga_control <- ga(
      type = "binary",
      nBits = 5 * 5,
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



library(GA)
library(optparse)

# setwd("src")

source("my_fitness_ova.R")
source("ga_operators_ova.R")
source("utils.R")

variables <- c("eta1", "eta2", "eta3", "eta4") # dir_names in the other script

# default hyperparameters
option_list <- list(
  make_option(c("--modeDim"), type="integer", default=100, help="Sample size"),
  make_option(c("--popSize"), type="integer", default=100, help="Population size"),
  make_option(c("--maxiter"), type="integer", default=100, help="Maximum iterations"),
  make_option(c("--pmutation"), type="double", default=1.0, help="Mutation rate"),
  make_option(c("--pcrossover"), type="double", default=0.8, help="Crossover rate"),
  make_option(c("--seed_start"), type="integer", default=1, help="First seed for the GA"),
  make_option(c("--seed_end"), type="integer", default=100, help="Last seed for the GA")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)


# Define and create results directory
hyperparam_subdir <- paste(opt$maxiter, opt$popSize, sep = "_")
results_dir <- file.path("..", "results", hyperparam_subdir, "ova")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


modelOI='  
# strucutral model
  OI ~ OP
  ACJ ~ OI
  ACL ~ OI
# outer model - composites
  OP <~ org_pre1 + org_pre2 + org_pre3 + org_pre4 + org_pre5 + org_pre6 +  org_pre7 + org_pre8 
  OI <~ org_ident1 + org_ident2 + org_ident3 + org_ident4 + org_ident5 + org_ident6
  ACJ <~ ac_joy1 + ac_joy2 + ac_joy3 + ac_joy4
  ACL <~ ac_love1 + ac_love2 + ac_love3
'

ova_matrix <- matrix(c(
    0, 0, 0, 0,  # OP dependencies
    1, 0, 0, 0,  # OI dependencies
    0, 1, 0, 0,  # ACJ dependencies
    0, 1, 0, 0   # ACL dependencies
  ), nrow = 4, byrow = TRUE)
  
dati_OVA <-  read.csv("ds/Data_OIvariations.txt", sep=";", stringsAsFactors=TRUE)
ova_string <- create_sem_model_string_from_matrix_ova(ova_matrix)

outTAM=csem(.data = dati_OVA,.model = modelOI,.PLS_modes = 'modeA')
outTam=csem(.data = dati_OVA,.model = ova_string,.PLS_modes = 'modeA')

criteriaTam <- calculateModelSelectionCriteria(outTAM, .by_equation = FALSE)
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
    
    fitness_function <- myFitnessOVA
    mutation_function <- myMutationOVA
    
    ga_control <- ga(
      type = "binary",
      nBits = 4 * 4,
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



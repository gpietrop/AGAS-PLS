library(GA)
library(optparse)

setwd("src")

source("my_fitness.R")
source("ga_operators.R")
source("hyperparameters.R")
source("run_model.R")
source("utils.R")

# default hyperparameters
option_list <- list(
  make_option(c("--model"), default="str1_high", help="Model to use"),
  make_option(c("--modeDim"), type="integer", default=100, help="Sample size"),
  make_option(c("--popSize"), type="integer", default=100, help="Population size"),
  make_option(c("--maxiter"), type="integer", default=50, help="Maximum iterations"),
  make_option(c("--pmutation"), type="double", default=1.0, help="Mutation rate"),
  make_option(c("--pcrossover"), type="double", default=0.8, help="Crossover rate"),
  make_option(c("--seed_start"), type="integer", default=0, help="First seed for the GA"),
  make_option(c("--seed_end"), type="integer", default=99, help="Last seed for the GA"),
  make_option(c("--treeRows"), type="logical", default=TRUE, help="Use treeRow-specific mutation and fitness")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Select appropriate model function
model_mapping <- list(
  str1 = run_sem_model_str1,
  str2 = run_sem_model_str2,
  str3 = run_sem_model_str3,
  str4 = run_sem_model_str4
)

model_key <- substr(opt$model, 1, 4)
if (!model_key %in% names(model_mapping)) stop("Model string does not start correctly.")

run_specific_model <- model_mapping[[model_key]]
result_dir_str <- model_key

# Define and create results directory
hyperparam_subdir <- paste(opt$maxiter, opt$popSize, opt$treeRows, sep = "_")
results_dir <- file.path("..", "results_speed", hyperparam_subdir, result_dir_str)
model_subdir <- paste(opt$model, opt$modeDim, sep="_")
subdir <- file.path(results_dir, model_subdir)
dir.create(subdir, recursive = TRUE, showWarnings = FALSE)

# Define all possible eta connections (36 combinations)
etas <- paste0("eta", 1:6)
all_connections <- as.vector(outer(etas, etas, function(x, y) ifelse(x != y, paste(x, "~", y), NA)))
all_connections <- all_connections[!is.na(all_connections)]  

# Define the file for storing p-values
p_value_subdir <- file.path(results_dir, "p_values")
if (!dir.exists(p_value_subdir)) {
  dir.create(p_value_subdir, recursive = TRUE)
}
p_value_name_file = paste("p", opt$model, opt$modeDim, sep="_")
p_value_file = file.path(p_value_subdir, p_value_name_file)

if (!file.exists(p_value_file)) {
  write.table(data.frame(Connection = all_connections), file = p_value_file, row.names = FALSE, quote = FALSE, sep = "\t")
}

# GA function
run_ga <- function(seed) {
  message("Running GA for seed: ", seed)
  best_individuals_all <<- list()
  best_individual <<- NULL
  best_fitness <<- -Inf
  
  # Generate and save the dataset
  dataset_generated <- generateData(
    .model = get(opt$model),    
    .n     = opt$modeDim,             
    .return_type = "data.frame",
    .empirical = FALSE
  )
  write.csv(dataset_generated, file.path(subdir, paste0(seed, "_dataset_generated.csv")), row.names = FALSE)
  
  # get the BIC of the true model 
  bic_true = run_specific_model(dataset_generated)
  cat("The true model has BIC: ", bic_true, "\n")
  
  # Save the specific of the run
  hyperparams <- data.frame(
    "Population Size" = opt$popSize,
    "Max Iterations" = opt$maxiter,
    "Mutation Rate" = opt$pmutation,
    "Crossover Rate" = opt$pcrossover,
    "True BIC" = bic_true, 
    "TreeRows" = opt$treeRows
  )
  write.csv(hyperparams, file.path(subdir, paste0(seed, "_hyperparameters.csv")), row.names = FALSE, quote = FALSE)
  
  # Time the GA execution
  ga_time <- system.time({
    
    fitness_function <- if (opt$treeRows) myFitnessTreeRowZero else myFitness
    mutation_function <- if (opt$treeRows) myMutationTreeRowZero else myMutation
    
    ga_control <- ga(
      type = "binary",
      nBits = n_variables * n_variables,
      popSize = opt$popSize,
      maxiter = opt$maxiter,
      pmutation = opt$pmutation,
      pcrossover = opt$pcrossover,
      fitness = function(x) fitness_function(x, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated),
      elitism = TRUE,
      parallel = FALSE,
      seed = seed,
      mutation = mutation_function
    )
  })
  
  # Save the computation time to a file
  write.csv(data.frame(ComputationTime = ga_time['elapsed']), file.path(subdir, paste0(seed, "_time.csv")), row.names = FALSE)
  
  # Compute the p-values and update the p-value file
  model_string_best <- create_sem_model_string_from_matrix(best_individual, variables, measurement_model, structural_coefficients, type_of_variable)
  out_best <- csem(.data = dataset_generated,.model = model_string_best, .resample_method = "bootstrap")
  summar <- cSEM::summarize(out_best)
  p_val <- round(summar$Estimates$Path_estimates$p_value, 4)
  p_name <- summar$Estimates$Path_estimates$Name
  update_p_value_file(p_value_file, p_name, p_val)
  
  # Save tracked best individual
  if (!is.null(best_individual) && length(best_individual) != 0) {
    best_ind_df <- as.data.frame(best_individual)
    colnames(best_ind_df) <- variables
    rownames(best_ind_df) <- variables
    write.csv(best_ind_df, file.path(subdir, paste0(seed, "_best", ".csv")), row.names = TRUE)
  }
  
  # Save GA's best fitness
  write.csv(data.frame(Fitness = - ga_control@fitnessValue), file.path(subdir, paste0(seed, "_fitness", ".csv")), row.names = FALSE)
}

# Loop over a range of seeds
lapply(opt$seed_start:opt$seed_end, run_ga)


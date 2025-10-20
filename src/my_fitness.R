# Ensure the cSEM library and your model creation function are loaded
library(cSEM)
library(igraph)

# import model.R expecially: create_sem_model_string_from_matrix
source("utils.R")
source("fitness_utils.R")

best_individuals_all <<- list()
best_individual <<- NULL
best_fitness <<- -Inf

myFitness <- function(matrix_vector, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated) {
  n_variables <- length(variables)
  adj_matrix <- matrix(matrix_vector, nrow = n_variables, byrow = TRUE)
  
  # Check each column to ensure at least one non-zero entry
  if (any(colSums(adj_matrix) == 0)) {
    adj_matrix <- repair_individual_unused(adj_matrix)
  }
  
  if (!check_matrix_criteria(adj_matrix)) {
    return(-100000)  # Penalize if criteria are not met
  }
  
  # Convert the matrix to an igraph object
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE)
  
  # Check for cycles using girth, which finds the shortest cycle
  has_cycle <- has_cycle_dfs(g, adj_matrix)
  if (has_cycle) {
    return(-100000)  # The matrix has cyclic dependencies
  } 
  
  # Check if the solution is admissible 
  model_string <- create_sem_model_string_from_matrix(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable)
  out <- csem(.data = dataset_generated,.model = model_string)
  ver = verify(out)
  if (!sum(ver) == 0) {
    return(-100000)  # Penalize configurations where any construct is unused
  }
  
  # SEM fitness calculation (using the negative of AIC to make higher values more fit)
  sem_fitness <- -fitness(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated)
  if (is.na(sem_fitness)) {
    return(-100000)
  }
  
  # Store the modified individual
  if (sem_fitness > best_fitness) {
    best_individual <<- adj_matrix
    best_fitness <<- sem_fitness
    best_individuals_all <<- append(best_individuals_all, list(adj_matrix))
  }
  
  return(sem_fitness)
}


myFitnessTreeRowZero <- function(matrix_vector, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated) {
  n_variables <- length(variables)
  adj_matrix <- matrix(matrix_vector, nrow = n_variables, byrow = TRUE)
  
  # Check each column+row to ensure at least one non-zero entry
  if (!check_matrix(adj_matrix)) {
    adj_matrix <- repair_individual_unused(adj_matrix)
  }
  
  if (!check_matrix_criteriaTreeRowZero (adj_matrix)) {
    return(-100000)  # Penalize if criteria are not met
  }
  
  # Convert the matrix to an igraph object
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE)
  
  # Check for cycles using girth, which finds the shortest cycle
  has_cycle <- has_cycle_dfs(g, adj_matrix)
  if (has_cycle) {
    return(-100000)  # The matrix has cyclic dependencies
  } 
  
  # Check if the solution is admissible 
  model_string <- create_sem_model_string_from_matrix(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable)
  out <- csem(.data = dataset_generated,.model = model_string)
  ver = verify(out)
  if (!sum(ver) == 0) {
    return(-100000)  # Penalize configurations where any construct is unused
  }
  
  # SEM fitness calculation (using the negative of AIC to make higher values more fit)
  sem_fitness <- -fitness(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated)
  if (is.na(sem_fitness)) {
    return(-100000)
  }
  
  # Store the modified individual
  if (sem_fitness > best_fitness) {
    best_individual <<- adj_matrix
    best_fitness <<- sem_fitness
    best_individuals_all <<- append(best_individuals_all, list(adj_matrix))
  }
  
  return(sem_fitness)
}

# Define the fitness function
fitness <- function(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable, dataset_generated) {
  # Create the model string from the adjacency matrix
  model_string <- create_sem_model_string_from_matrix(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable)
  # Load data and perform SEM analysis
  out <- csem(.data = dataset_generated, .model = model_string)
  
  # Calculate model selection criteria
  model_criteria <- calculateModelSelectionCriteria(out, 
                                                    .by_equation = FALSE,
                                                    .only_structural = FALSE
                                                    )
  # Extract the BIC values
  bic_values <- model_criteria$BIC
  
  return(bic_values)
}

myFitnessSatisfaction <- function(matrix_vector) {
  n_variables <- 6
  adj_matrix <- matrix(matrix_vector, nrow = n_variables, byrow = TRUE)

    if (!check_matrix(adj_matrix)) {
    adj_matrix <- repair_individual_unused(adj_matrix)
  }
  if (!check_matrix_criteria (adj_matrix)) {
    return(-100000)  
  }
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE)
  has_cycle <- has_cycle_dfs(g, adj_matrix)
  if (has_cycle) {
    return(-100000)  # The matrix has cyclic dependencies
  } 
  
  model_string <- create_sem_model_string_from_matrix_satisfaction(adj_matrix)
  out <- csem(.data = satisfaction,.model = model_string)
  ver = verify(out)
  if (!sum(ver) == 0) {
    return(-100000)  # Penalize configurations where any construct is unused
  }
  
  # SEM fitness calculation (using the negative of AIC to make higher values more fit)
  model_criteria <- calculateModelSelectionCriteria(out, .by_equation = FALSE)
  sem_fitness <- - model_criteria$BIC
  
  if (is.na(sem_fitness)) {
    return(-100000)
  }
  
  # Store the modified individual
  if (sem_fitness > best_fitness) {
    best_individual <<- adj_matrix
    best_fitness <<- sem_fitness
    best_individuals_all <<- append(best_individuals_all, list(adj_matrix))
  }
  
  return(sem_fitness)
}
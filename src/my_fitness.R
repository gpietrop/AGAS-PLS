# Ensure the cSEM library and your model creation function are loaded
library(cSEM)

# import model.R expecially: create_sem_model_string_from_matrix
source("utils.R")
source("fitness_utils.R")

best_individuals_all <<- list()
best_individual <<- NULL
best_fitness <<- -Inf

myFitnessTreeRowZero <- function(matrix_vector,
                                 variables,
                                 measurement_model,
                                 structural_coefficients,
                                 type_of_variable,
                                 dataset_generated) {
  n_variables <- length(variables)
  adj_matrix <- matrix(matrix_vector, nrow = n_variables, byrow = TRUE)
  
  # Ensure each node is used (row or column non-zero); repair if needed
  if (!check_matrix(adj_matrix)) {
    adj_matrix <- repair_individual_unused(adj_matrix)
  }
  
  # Structural constraints: first three rows zero + diagonal zero, etc.
  if (!check_matrix_criteriaTreeRowZero(adj_matrix)) {
    return(-100000)  # Penalize if criteria are not met
  }
  
  # Check for cycles directly on the adjacency matrix
  if (has_cycle_matrix(adj_matrix)) {
    return(-100000)  # The matrix has cyclic dependencies
  }
  
  # Build SEM model string from the adjacency matrix
  model_string <- create_sem_model_string_from_matrix(
    adj_matrix,
    variables,
    measurement_model,
    structural_coefficients,
    type_of_variable
  )
  
  # Run SEM
  out <- csem(.data = dataset_generated, .model = model_string)
  
  # Check if the solution is admissible
  ver <- verify(out)
  if (!sum(ver) == 0) {
    return(-100000)  # Penalize configurations where any construct is unused
  }
  
  # Calculate model selection criteria (BIC)
  model_criteria <- calculateModelSelectionCriteria(
    out,
    .by_equation     = FALSE,
    .only_structural = FALSE
  )
  bic_values <- model_criteria$BIC
  
  # If BIC is NA, penalize
  if (is.na(bic_values)) {
    return(-100000)
  }
  
  # Higher fitness = lower BIC → negative sign
  sem_fitness <- -bic_values
  
  # Track best individual seen so far (global variables)
  if (sem_fitness > best_fitness) {
    best_individual <<- adj_matrix
    best_fitness <<- sem_fitness
    best_individuals_all <<- c(best_individuals_all, list(adj_matrix))
  }
  
  return(sem_fitness)
}
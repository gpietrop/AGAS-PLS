# Ensure the cSEM library and your model creation function are loaded
library(cSEM)
library(igraph)

# import model.R expecially: create_sem_model_string_from_matrix
source("utils.R")
source("fitness_utils.R")

best_individuals_all <<- list()
best_individual <<- NULL
best_fitness <<- -Inf


myFitnessOVA <- function(matrix_vector, data_boost) {
  n_variables <- 4
  if (is.matrix(matrix_vector)) {
    adj_matrix <- matrix_vector
  } else {
    adj_matrix <- matrix(matrix_vector, nrow = n_variables, byrow = TRUE)
  }
  # print(adj_matrix)

  if (!check_matrix(adj_matrix)) {
    # print("repaired")
    adj_matrix <- repair_individual_unused(adj_matrix)
  }
  if (!check_matrix_criteria(adj_matrix)) {
    # print("here check matrix")
    return(-100000)  
  }
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE)
  has_cycle <- has_cycle_dfs(g, adj_matrix)
  if (has_cycle) {
    # print("here")
    return(-100000)  # The matrix has cyclic dependencies
  } 
  
  model_string <- create_sem_model_string_from_matrix_ova(adj_matrix)
  out <- csem(.data = data_boost,.model = model_string, .PLS_modes = 'modeA')
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

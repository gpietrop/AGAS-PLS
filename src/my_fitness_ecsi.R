# Ensure the cSEM library and your model creation function are loaded
library(cSEM)
library(igraph)

# import model.R expecially: create_sem_model_string_from_matrix
source("utils.R")
source("fitness_utils.R")

best_individuals_all <<- list()
best_individual <<- NULL
best_fitness <<- -Inf


myFitnessECSI <- function(matrix_vector) {
  n_variables <- 7
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
  
  model_string <- create_sem_model_string_from_matrix_trans(adj_matrix)
  dataTrans <- read.csv("ds/data_ecsi.csv", sep = ";")
  out <- csem(.data = dataTrans,.model = model_string, .PLS_modes = 'modeA')
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
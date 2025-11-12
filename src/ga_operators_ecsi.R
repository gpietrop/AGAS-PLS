library(GA)

source("fitness_utils.R")

myMutationECSI <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  mutate_matrix <- matrix(mutate, nrow = 7, byrow = TRUE)
  
  diag(mutate_matrix) <- 0  
  
  # Ensure the first three rows are all zeros
  mutate_matrix[1, ] <- 0
  # mutate_matrix[2, ] <- 0
  # mutate_matrix[3, ] <- 0 
  
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Create indices of all diagonal entries
  indices <- which(!diag(7), arr.ind = TRUE)
  indices <- indices[indices[, 1] > 1, ]  # Exclude first three rows
  
  # Convert row and column indices to vector indices
  if (length(indices) > 0) {
    subdiag_indices <- (indices[, 1] - 1) * 7 + indices[, 2]
    # Select a random index from the sub-diagonal indices 
    if (length(subdiag_indices) > 0 && runif(1) <= 0.2) {
      available_indices <- subdiag_indices  # Keep track of available indices to flip
      while (length(available_indices) > 1) {
        j <- sample(available_indices, size = 1)
        mutate_vector[j] <- abs(mutate_vector[j] - 1)
        
        # Check if the mutation creates a cycle
        adj_matrix <- matrix(mutate_vector, nrow = 7, byrow = TRUE)
        g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", diag = FALSE)
        has_cycle <- has_cycle_dfs(g, adj_matrix)
        
        if (has_cycle) {
          mutate_vector[j] <- abs(mutate_vector[j] - 1)
          available_indices <- setdiff(available_indices, j)
        } else {
          break  
        }
      }
    }
  }
  return(mutate_vector)
}
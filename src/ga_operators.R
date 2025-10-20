library(GA)

source("fitness_utils.R")
source("hyperparameters.R")


myMutation <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  mutate_matrix <- matrix(mutate, nrow = n_variables, byrow = TRUE)
  
  diag(mutate_matrix) <- 0  
  
  mutate_matrix[1, ] <- 0
  
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Create indices of lower triangular part (excluding diagonal) starting from row 2
  indices <- which(lower.tri(matrix(1, nrow = n_variables, ncol = n_variables)) & !diag(n_variables), arr.ind = TRUE)
  indices <- indices[indices[, 1] > 1, ]  # Exclude first row
  
  # Convert row and column indices to vector indices
  if (length(indices) > 0) {
    subdiag_indices <- (indices[, 1] - 1) * n_variables + indices[, 2]
    
    # Select a random index from the sub-diagonal indices to flip
    if (length(subdiag_indices) > 0 && runif(1) <= 0.2) {
      j <- sample(subdiag_indices, size = 1)
      mutate_vector[j] <- abs(mutate_vector[j] - 1)
    }
  }
  
  return(mutate_vector)
}

myMutationTreeRowZero <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  mutate_matrix <- matrix(mutate, nrow = n_variables, byrow = TRUE)
  
  diag(mutate_matrix) <- 0  
  
  # Ensure the first three rows are all zeros
  mutate_matrix[1, ] <- 0
  mutate_matrix[2, ] <- 0
  mutate_matrix[3, ] <- 0 
  
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Create indices of lower triangular part (excluding diagonal) starting from row 4
  indices <- which(!diag(n_variables), arr.ind = TRUE)
  indices <- indices[indices[, 1] > 3, ]  # Exclude first three rows
  
  # Convert row and column indices to vector indices
  if (length(indices) > 0) {
    subdiag_indices <- (indices[, 1] - 1) * n_variables + indices[, 2]
    # Select a random index from the sub-diagonal indices 
    if (length(subdiag_indices) > 0 && runif(1) <= 0.2) {
      available_indices <- subdiag_indices  # Keep track of available indices to flip
      while (length(available_indices) > 1) {
        j <- sample(available_indices, size = 1)
        mutate_vector[j] <- abs(mutate_vector[j] - 1)
        
        # Check if the mutation creates a cycle
        adj_matrix <- matrix(mutate_vector, nrow = n_variables, byrow = TRUE)
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


myMutationSatisfaction <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  mutate_matrix <- matrix(mutate, nrow = n_variables, byrow = TRUE)
  
  diag(mutate_matrix) <- 0  
  
  # Ensure the first row is all zeros
  mutate_matrix[1, ] <- 0
  
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Create indices of lower triangular part (excluding diagonal) starting from row 4
  indices <- which(!diag(n_variables), arr.ind = TRUE)
  indices <- indices[indices[, 1] > 1, ]  # Exclude first row
  
  # Convert row and column indices to vector indices
  if (length(indices) > 0) {
    subdiag_indices <- (indices[, 1] - 1) * n_variables + indices[, 2]
    # Select a random index from the sub-diagonal indices 
    if (length(subdiag_indices) > 0 && runif(1) <= 0.2) {
      available_indices <- subdiag_indices  # Keep track of available indices to flip
      while (length(available_indices) > 1) {
        j <- sample(available_indices, size = 1)
        mutate_vector[j] <- abs(mutate_vector[j] - 1)
        
        # Check if the mutation creates a cycle
        adj_matrix <- matrix(mutate_vector, nrow = n_variables, byrow = TRUE)
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
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
  
  diag(mutate_matrix) <- 0  # Set the diagonal elements to zero
  
  mutate_matrix[upper.tri(mutate_matrix)] <- 0
  
  mutate_matrix[1, ] <- 0  # Set all elements in the first row to zero
  
  row_sums <- rowSums(mutate_matrix)
  col_sums <- colSums(mutate_matrix)
  if (any(row_sums == 0 & col_sums == 0)) {
    mutate_matrix <- repair_individual_unused(mutate_matrix)
  }
  
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Calculate sub-diagonal indices for a square matrix flattened into a vector
  # subdiag_indices <- sapply(2:n_variables, function(i) (i - 1) * n_variables + (i - 1))
  
  # Create indices of lower triangular part (excluding diagonal)
  indices <- which(lower.tri(matrix(1, nrow = n_variables, ncol = n_variables)) & !diag(n_variables), arr.ind = TRUE)
  # Convert row and column indices to vector indices
  subdiag_indices <- (indices[, 1] - 1) * n_variables + indices[, 2]
  # Select a random index from the sub-diagonal indices to flip
  if (length(subdiag_indices) > 0 && runif(1) <= 0.4) {
    # print("before")
    # print(matrix(mutate_vector, nrow = n_variables, byrow = TRUE))
    j <- sample(subdiag_indices, size = 1)
    mutate_vector[j] <- abs(mutate_vector[j] - 1)
    # print("after")
    # print(matrix(mutate_vector, nrow = n_variables, byrow = TRUE))
  }
  return(mutate_vector)
}
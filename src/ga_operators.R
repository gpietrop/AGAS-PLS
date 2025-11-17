library(GA)

source("fitness_utils.R")
source("hyperparameters.R")

myMutationTreeRowZero <- function(object, parent) {
  # Extract the individual's adjacency vector and reshape as matrix
  mutate <- as.vector(object@population[parent, ])
  mutate_matrix <- matrix(mutate, nrow = n_variables, byrow = TRUE)
  
  # Zero out diagonal
  diag(mutate_matrix) <- 0  
  
  # Ensure the first three rows are all zeros
  mutate_matrix[1, ] <- 0
  mutate_matrix[2, ] <- 0
  mutate_matrix[3, ] <- 0 
  
  # Back to vector (note the transpose as in your original code)
  mutate_vector <- as.vector(t(mutate_matrix))
  
  # Indices of all off-diagonal entries
  indices <- which(!diag(n_variables), arr.ind = TRUE)
  # Exclude first three rows
  indices <- indices[indices[, 1] > 3, , drop = FALSE]
  
  if (nrow(indices) > 0 && runif(1) <= 0.2) {
    # Convert (row, col) to linear indices in mutate_vector
    subdiag_indices <- (indices[, 1] - 1) * n_variables + indices[, 2]
    
    if (length(subdiag_indices) > 0) {
      available_indices <- subdiag_indices  # candidates to flip
      
      while (length(available_indices) > 1) {  # same condition as your code
        j <- sample(available_indices, size = 1)
        
        # Flip bit
        old_val <- mutate_vector[j]
        mutate_vector[j] <- 1 - old_val
        
        # Rebuild adjacency matrix and check for cycles
        adj_matrix <- matrix(mutate_vector, nrow = n_variables, byrow = TRUE)
        has_cycle <- has_cycle_matrix(adj_matrix)
        
        if (has_cycle) {
          # Revert flip and remove j from candidates
          mutate_vector[j] <- old_val
          available_indices <- setdiff(available_indices, j)
        } else {
          # Valid mutation: keep it and stop
          break
        }
      }
    }
  }
  
  return(mutate_vector)
}

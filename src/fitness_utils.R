source("utils.R")


check_matrix_criteria <- function(adj_matrix) {
  # Check if the first rows is all zeros
  if (any(rowSums(adj_matrix[1, , drop = FALSE]) != 0)) {
    return(FALSE)
  }
  
  # Check if the diagonal elements are all zeros
  if (any(diag(adj_matrix) != 0)) {
    return(FALSE)
  }
  
  return(TRUE)
}


check_matrix_criteriaTreeRowZero <- function(adj_matrix) {
  # Check if the first three rows are all zeros
  if (any(rowSums(adj_matrix[1:3, ]) != 0)) {
    return(FALSE)
  }
  
  # Check if the diagonal elements are all zeros
  if (any(diag(adj_matrix) != 0)) {
    return(FALSE)
  }
  
  return(TRUE)
}


repair_individual_unused <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  
  # Precompute row and column sums once
  row_sums <- rowSums(adj_matrix)
  col_sums <- colSums(adj_matrix)
  
  # Nodes with both row and column all zeros
  empty_indices <- which(row_sums == 0 & col_sums == 0)
  
  if (length(empty_indices) == 0) {
    return(adj_matrix)
  }
  
  for (k in empty_indices) {
    
    # Decide randomly whether to modify the column or the row (unchanged logic)
    if (runif(1) < 0.5) {
      ## -------------------------------
      ## Case 1: modify a column (incoming edges to node k)
      ## -------------------------------
      valid_rows <- if (k == n) 2:(n - 1) else (k + 1):n
      if (k <= n) {
        valid_rows <- valid_rows[valid_rows != k]  # remove diagonal as in your code
      }
      
      if (length(valid_rows) > 0) {
        if (length(valid_rows) == 1) {
          # Same behavior: direct assignment, no cycle check here
          adj_matrix[valid_rows, k] <- 1
        } else {
          # Try candidates in a random order (equivalent to your while+sample+remove)
          for (chosen_row in sample(valid_rows)) {
            old_val <- adj_matrix[chosen_row, k]
            adj_matrix[chosen_row, k] <- 1
            
            # Check for cycle on the updated matrix
            if (!has_cycle_matrix(adj_matrix)) {
              # good, keep the edge and stop trying for this k
              break
            } else {
              # cycle → revert and try another row
              adj_matrix[chosen_row, k] <- old_val
            }
          }
        }
      }
      
    } else {
      ## -------------------------------
      ## Case 2: modify a row (outgoing edges from node k – per your comment)
      ## -------------------------------
      valid_cols <- if (k == n) 1:(n - 1) else (k + 1):n
      if (k <= n) {
        valid_cols <- valid_cols[valid_cols != k]  # remove diagonal as in your code
      }
      
      if (length(valid_cols) > 0) {
        if (length(valid_cols) == 1) {
          # Same behavior: direct assignment
          adj_matrix[k, valid_cols] <- 1
        } else {
          # Try candidates in random order
          for (chosen_col in sample(valid_cols)) {
            old_val <- adj_matrix[chosen_col, k]
            # ⚠ this line is intentionally kept exactly as in your original code
            adj_matrix[chosen_col, k] <- 1
            
            if (!has_cycle_matrix(adj_matrix)) {
              # keep the edge
              break
            } else {
              # revert and try another column
              adj_matrix[chosen_col, k] <- old_val
            }
          }
        }
      }
    }
  }
  
  adj_matrix
}



check_matrix <- function(mat) {
  row_sums <- rowSums(mat)
  col_sums <- colSums(mat)
  # TRUE if no node has both row and column all zero
  !any(row_sums == 0 & col_sums == 0)
}


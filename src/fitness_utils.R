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
  
  # Precompute row/col sums once
  row_sums <- rowSums(adj_matrix)
  col_sums <- colSums(adj_matrix)
  
  # Indices of nodes with row and column all zeros
  empty_indices <- which(row_sums == 0 & col_sums == 0)
  
  if (length(empty_indices) == 0) {
    return(adj_matrix)
  }
  
  for (k in empty_indices) {
    
    # Decide randomly whether to modify a column or a row (as in your code)
    if (runif(1) < 0.5) {
      ## -------------------------------
      ## Case 1: modify a column (incoming edge to k)
      ## -------------------------------
      valid_rows <- if (k == n) 2:(n - 1) else (k + 1):n
      # remove k from candidates (keeps diagonal excluded as in your code)
      valid_rows <- valid_rows[valid_rows != k]
      
      if (length(valid_rows) > 0) {
        if (length(valid_rows) == 1) {
          # same behavior: no cycle check here
          adj_matrix[valid_rows, k] <- 1
        } else {
          # Try rows in a random order (equivalent to your while+sample)
          for (chosen_row in sample(valid_rows)) {
            adj_matrix_mod <- adj_matrix
            adj_matrix_mod[chosen_row, k] <- 1
            
            g <- graph_from_adjacency_matrix(
              adj_matrix_mod, mode = "directed", diag = FALSE
            )
            condition_met <- has_cycle_dfs(g, adj_matrix_mod)
            
            if (!condition_met) {
              adj_matrix <- adj_matrix_mod
              break
            }
          }
        }
      }
      
    } else {
      ## -------------------------------
      ## Case 2: modify a row (outgoing edge from k)
      ## -------------------------------
      valid_cols <- if (k == n) 1:(n - 1) else (k + 1):n
      # remove k from candidates (as in your code)
      valid_cols <- valid_cols[valid_cols != k]
      
      if (length(valid_cols) > 0) {
        if (length(valid_cols) == 1) {
          # same behavior: no cycle check here
          adj_matrix[k, valid_cols] <- 1
        } else {
          # Try columns in a random order (equivalent probability)
          for (chosen_col in sample(valid_cols)) {
            adj_matrix_mod <- adj_matrix
            # NOTE: this line is *unchanged* from your code
            adj_matrix_mod[chosen_col, k] <- 1
            
            g <- graph_from_adjacency_matrix(
              adj_matrix_mod, mode = "directed", diag = FALSE
            )
            condition_met <- has_cycle_dfs(g, adj_matrix_mod)
            
            if (!condition_met) {
              adj_matrix <- adj_matrix_mod
              break
            }
          }
        }
      }
    }
  }
  
  return(adj_matrix)
}


check_matrix <- function(mat) {
  row_sums <- rowSums(mat)
  col_sums <- colSums(mat)
  # TRUE if no node has both row and column all zero
  !any(row_sums == 0 & col_sums == 0)
}

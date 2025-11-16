library(igraph)


create_sem_model_string_from_matrix <- function(adj_matrix, variables, measurement_model, structural_coefficients, type_of_variable) {
  # Initialize the model string
  model_string <- "# Composite model\n"
  
  # Include the measurement model specified in the input for composite types
  for (var in names(measurement_model)) {
    if (type_of_variable[var] == "composite") {
      items <- paste(measurement_model[[var]], collapse = " + ")
      model_string <- paste(model_string, sprintf("  %s <~ %s\n", var, items), sep = "")
    }
  }
  
  # Adding reflective measurement models
  model_string <- paste(model_string, "\n# Reflective measurement model\n")
  for (var in names(measurement_model)) {
    if (type_of_variable[var] == "reflective") {
      items <- paste(measurement_model[[var]], collapse = " + ")
      model_string <- paste(model_string, sprintf("  %s =~ %s\n", var, items), sep = "")
    }
  }
  
  # Structural model section
  model_string <- paste(model_string, "\n# Structural model\n")
  
  # Iterate over each variable to define its dependencies based on the matrix
  for (i in seq_along(variables)) {
    dependent <- variables[i]
    predictors <- variables[adj_matrix[i, ] == 1]
    
    if (length(predictors) > 0) {
      relationship_str <- paste(predictors, collapse = " + ")
      model_string <- paste(model_string, sprintf("  %s ~ %s\n", dependent, relationship_str), sep = "")
    }
  }
  
  # Cleanup the string: remove any unnecessary characters and trim trailing spaces/new lines
  model_string <- gsub("\\n\\s+$", "", model_string)  
  model_string <- trimws(model_string)  
  
  # Return the complete model string
  return(model_string)
}

create_sem_model_string_from_matrix_satisfaction <- function(adj_matrix) {
  # Define variable names corresponding to the rows and columns of the matrix
  variables <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
  
  # Start with the composite and measurement models (static part)
  model_string <- "
  # Composite model
  IMAG <~ imag1 + imag2 + imag3
  EXPE <~ expe1 + expe2 + expe3
  QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5
  VAL  <~ val1  + val2  + val3
  
  # Reflective measurement model
  SAT  =~ sat1  + sat2  + sat3  + sat4
  LOY  =~ loy1  + loy2  + loy3  + loy4
  
  # Measurement error correlation
  sat1 ~~ sat2
  
  # Structural model
  "
  
  # Iterate over each variable to define its dependencies based on the matrix
  for (i in seq_along(variables)) {
    dependent <- variables[i]
    predictors <- variables[adj_matrix[i, ] == 1]
    
    if (length(predictors) > 0) {
      model_string <- paste(model_string, sprintf("%s ~ %s\n  ", dependent, paste(predictors, collapse = " + ")), sep = "")
    }
  }
  
  # Ensure there are no leading '+' in the structural model lines and trim trailing spaces/new lines
  model_string <- gsub("\\n  $", "", model_string)  # Remove trailing new line and spaces
  model_string <- trimws(model_string)  # Remove any leading or trailing whitespace
  # model_string <- paste0(model_string, "\"")
  
  return(model_string)
}


create_sem_model_string_from_matrix_trans <- function(adj_matrix) {
  # Define variable names corresponding to the rows and columns of the matrix
  variables <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY", "COMP")
  
  # Start with the composite and measurement models (static part)
  model_string <- "
  # Composite model
  IMAG =~ Image1 + Image2 + Image3 + Image4 + Image5 
  EXPE =~ Expec1 + Expec2 + Expec3 
  QUAL =~ PerQual1 + PerQual2 + PerQual3 + PerQual3 + PerQual5 + PerQual6 + PerQual7
  VAL =~ PerVal1 + PerVal2
  COMP =~ Compl
  SAT =~ Satis1 + Satis2 + Satis3
  LOY =~ Loyal1 + Loyal2 + Loyal3
  
  # Reflective measurement model
  
  # Measurement error correlation
  
  # Structural model
  "
  
  # Iterate over each variable to define its dependencies based on the matrix
  for (i in seq_along(variables)) {
    dependent <- variables[i]
    predictors <- variables[adj_matrix[i, ] == 1]
    
    if (length(predictors) > 0) {
      model_string <- paste(model_string, sprintf("%s ~ %s\n  ", dependent, paste(predictors, collapse = " + ")), sep = "")
    }
  }
  
  # Ensure there are no leading '+' in the structural model lines and trim trailing spaces/new lines
  model_string <- gsub("\\n  $", "", model_string)  # Remove trailing new line and spaces
  model_string <- trimws(model_string)  # Remove any leading or trailing whitespace
  # model_string <- paste0(model_string, "\"")
  
  return(model_string)
}


create_sem_model_string_from_matrix_tam <- function(adj_matrix) {
  # Define variable names corresponding to the rows and columns of the matrix
  variables <- c("EOI", "USEF", "ATT", "BI", "USE")
  
  # Start with the composite and measurement models (static part)
  model_string <- "
  # Composite model
  EOI <~ EOU1 + EOU2 + EOU3 + EOU4 + EOU5
  USEF <~ USEF1 + USEF2 + USEF3 + USEF4 + USEF5
  BI <~ BI1 + BI2 +  BI3   
  ATT <~ ATT1 + ATT2 + ATT3 + ATT4 + ATT5
  USE <~ USE1 + USE2 + USE3 + USE4 
  
  # Reflective measurement model
  
  # Measurement error correlation
  
  # Structural model
  "
  
  # Iterate over each variable to define its dependencies based on the matrix
  for (i in seq_along(variables)) {
    dependent <- variables[i]
    predictors <- variables[adj_matrix[i, ] == 1]
    
    if (length(predictors) > 0) {
      model_string <- paste(model_string, sprintf("%s ~ %s\n  ", dependent, paste(predictors, collapse = " + ")), sep = "")
    }
  }
  
  # Ensure there are no leading '+' in the structural model lines and trim trailing spaces/new lines
  model_string <- gsub("\\n  $", "", model_string)  # Remove trailing new line and spaces
  model_string <- trimws(model_string)  # Remove any leading or trailing whitespace
  # model_string <- paste0(model_string, "\"")
  
  return(model_string)
}



update_p_value_file <- function(file_name, p_name, p_val) {
  # Read the existing file
  df <- read.table(file_name, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)
  
  # Add a new column for the current run and initializa with NA
  new_col_name <- paste0("Run", ncol(df))  
  df[[new_col_name]] <- NA  
  
  # Fill in p-values for current run
  for (i in 1:length(p_name)) {
    connection <- p_name[i]
    df[df$Connection == connection, new_col_name] <- p_val[i]
  }
  
  # Write the updated data frame back to the file
  write.table(df, file = file_name, row.names = FALSE, quote = FALSE, sep = "\t")
}


has_cycle_dfs <- function(graph, adj_matrix) {
  visited <- rep(FALSE, vcount(graph))
  recStack <- rep(FALSE, vcount(graph))
  
  for (v in 1:vcount(graph)) {
    if (!visited[v]) {
      if (dfs_util(graph, v, visited, recStack, adj_matrix)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

dfs_util <- function(graph, v, visited, recStack, adj_matrix) {
  visited[v] <- TRUE
  recStack[v] <- TRUE
  
  neighbors <- which(adj_matrix[v, ] == 1)
  for (u in neighbors) {
    if (!visited[u] && dfs_util(graph, u, visited, recStack, adj_matrix)) {
      return(TRUE)
    } else if (recStack[u]) {
      return(TRUE)
    }
  }
  
  recStack[v] <- FALSE
  return(FALSE)
}

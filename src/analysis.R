library(readr)
library(dplyr)
library(here)

# setwd("src")
source("analysis_utils.R")

# Define the specific matrices for different str values
specific_matrices_list <- list(
  str1 = matrix(
    c(0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 1, 0),
    nrow = 6, byrow = TRUE
  ),
  str2 = matrix(
    c(0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      0, 0, 1, 1, 0, 0,
      1, 0, 0, 0, 1, 0),
    nrow = 6, byrow = TRUE
  ),
  str3 = matrix(
    c(0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 1, 0),
    nrow = 6, byrow = TRUE
  ),
  str4 = matrix(
    c(0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      1, 1, 1, 1, 0, 0,
      1, 1, 1, 1, 1, 0),
    nrow = 6, byrow = TRUE
  )
)

process_folder <- function(name_folder, str_id, 
                           effect_size, sample_size,
                           print_frequent = FALSE,
                           print_examples = FALSE, num_examples = 3) {
  specific_matrix <- specific_matrices_list[[str_id]]
  run_suffix <- paste(effect_size, sample_size, sep = "_")
  folder_path <- here("results_oct", name_folder, str_id)
  
  path <- file.path(folder_path, paste0(str_id, "_", run_suffix))

  # Check matrices against the specific matrix
  check <- check_matrices(path, specific_matrix, print_examples, num_examples)
  
  # Calculate the mean matrix
  mean_matrix <- calculate_mean_matrix(path)
  
  # Print top 5 matrices found
  if (print_frequent) {
    cat("\nTop 5 frequent matrices:\n")
    top5 <- find_top_5_frequent_matrices(path)
    print(top5)
  }
  
  # Print the mean matrix (if needed)
  print(mean_matrix)
}


str_id <- "str4"
effect_size <- "high"
sample_size <- "100"
name_folder <- "200_100_TRUE"
process_folder(name_folder, str_id, effect_size, sample_size, print_frequent = FALSE, print_examples = FALSE)


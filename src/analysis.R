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
                           effect_size, sample_size, dimension = 6) {
  specific_matrix <- specific_matrices_list[[str_id]]
  run_suffix <- paste(effect_size, sample_size, sep = "_")
  folder_path <- here("results", name_folder, str_id)
  
  path <- file.path(folder_path, paste0(str_id, "_", run_suffix))
  
  # path <- file.path(folder_path, paste0(str_id, "_", run_suffix))
  path <- "/Users/gpietrop/Desktop/plsGP/results/200_200/ecsi/"
  # Check matrices against the specific matrix
  # check <- check_matrices(path, specific_matrix, print_examples, num_examples)
  
  # Calculate the mean matrix
  mean_matrix <- calculate_mean_matrix(path, dimension)
  
  # Print the mean matrix (if needed)
  print(mean_matrix)
}

str_id <- "ecsi"
effect_size <- ""
sample_size <- "100"
name_folder <- "200_200" # "200_100_TRUE"

process_folder(name_folder, str_id, effect_size, sample_size, dimension=7)

# str_id <- "str1"
effect_size <- "high"
# sample_size <- "100"
name_folder <- "400_50_TRUE"
# process_folder(name_folder, str_id, effect_size, sample_size, print_frequent = FALSE, print_examples = FALSE)

path_plot = paste0("/Users/gpietrop/Desktop/plsGP/results/", name_folder, "/str1/str1_", effect_size, "_100")

# path_plot <- "/Users/gpietrop/Desktop/plsGP/results/200_200/ecsi/"
find_top_matrices(path_plot, 2)
visualize_fitness_distribution(path_plot)
visualize_time_distribution(path_plot)

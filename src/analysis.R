library(readr)
library(dplyr)
library(here)

# setwd("src")
source("analysis_utils.R")

str_id <- "str1"
specific_matrix <- specific_matrices_list[[str_id]]
sample_size <- "100"

effect_size <- "small"
name_folder <- "50_50_TRUE"
path_folder <- file.path(here("results", name_folder, str_id), paste0(str_id, "_", paste(effect_size, sample_size, sep = "_")))

calculate_mean_matrix(path_folder, 6)
check <- check_matrices(path_folder, specific_matrix)

visualize_fitness_distribution(path_folder)
visualize_time_distribution(path_folder)




path_folder <- "/Users/gpietrop/Desktop/plsGP/results/100_100/ova/"
dimn = 4
calculate_mean_matrix(path_folder, dimn)
find_top_matrices(path_folder, 1)




source("utils.R")
source("hyperparameters.R")

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


get_best_individual <- function(folder_path) {
  hyper_file <- file.path(folder_path, "hyperparameters.csv")
  hyper_data <- read.csv(hyper_file)
  true_fitness <- hyper_data$True.AIC[1]
  
  fitness_files <- list.files(path = folder_path, pattern = "*_fitness.csv", full.names = TRUE)
  
  best_fitness <- Inf
  best_run <- NULL
  
  for (file in fitness_files) {
    fitness_data <- read.csv(file)
    current_best_fitness <- min(fitness_data$Fitness)
    if (current_best_fitness < best_fitness) {
      best_fitness <- current_best_fitness
      best_run <- gsub("_fitness.csv", "", basename(file))
    }
  }
  
  if (!is.null(best_run)) {
    best_file <- file.path(folder_path, paste0(best_run, "_best.csv"))
    best_individual <- read.csv(best_file, row.names = 1)
    
    cat("Best BIC from all runs:", best_fitness, "\n")
    cat("BIC from hyperparameters.csv (True BIC):", true_fitness, "\n")
    
    return(best_individual)
  } else {
    cat("No best individual found.\n")
    return(NULL)
  }
}

get_run_info <- function(folder_path, run_number) {
  hyper_file <- file.path(folder_path, "hyperparameters.csv")
  hyper_data <- read.csv(hyper_file)
  true_fitness <- hyper_data$True.AIC[1]
  
  fitness_file <- file.path(folder_path, paste0(run_number, "_fitness.csv"))
  best_file <- file.path(folder_path, paste0(run_number, "_best.csv"))
  
  if (!file.exists(fitness_file) || !file.exists(best_file)) {
    cat("Files for run", run_number, "not found.\n")
    return(NULL)
  }
  
  fitness_data <- read.csv(fitness_file)
  run_fitness <- min(fitness_data$Fitness)
  
  best_individual <- read.csv(best_file, row.names = 1)
  
  cat("Run", run_number, "fitness:", run_fitness, "\n")
  cat("Fitness from hyperparameters.csv (True AIC):", true_fitness, "\n")
  cat("Comparison result:", ifelse(run_fitness < true_fitness, "Better", "Worse or Equal"), "\n")
  
  return(list(fitness = run_fitness, best_individual = best_individual))
}


visualize_fitness_difference <- function(folder_path) {
  # Get all fitness files: e.g., 0_fitness.csv, 1_fitness.csv, ...
  fitness_files <- list.files(path = folder_path,
                              pattern = "_fitness\\.csv$",
                              full.names = TRUE)
  
  all_diff <- c()
  
  for (fit_file in fitness_files) {
    # Extract run id from filename (before the first "_")
    base <- basename(fit_file)                # "0_fitness.csv"
    run_id <- strsplit(base, "_")[[1]][1]     # "0"
    
    # Build corresponding hyperparameters filename
    hyper_file <- file.path(folder_path, paste0(run_id, "_hyperparameters.csv"))
    if (!file.exists(hyper_file)) {
      warning("Hyperparameters file not found for run: ", run_id)
      next
    }
    
    # Read true fitness for this run
    hyper_data <- read.csv(hyper_file)
    true_fitness <- hyper_data$True.BIC[1]    # or True.AIC if that's the correct column
    
    # Read obtained fitness for this run
    fitness_data <- read.csv(fit_file)
    found_fitness <- fitness_data$Fitness
    # Optional filter, if needed:
    # found_fitness <- found_fitness[found_fitness >= 0 & found_fitness <= 1e4]
    
    # Difference: found - true (change sign if you prefer true - found)
    diff_values <- found_fitness - true_fitness
    
    all_diff <- c(all_diff, diff_values)
  }
  
  if (length(all_diff) == 0) {
    warning("No fitness differences to plot.")
    return(invisible(NULL))
  }
  
  ylim <- range(all_diff, 0)
  boxplot(all_diff,
          main = "Difference Between Found and True Fitness Across Runs",
          ylab = "Found fitness - True fitness",
          ylim = ylim)
  abline(h = 0, col = "red", lty = 2)
  legend("topright",
         legend = c("No difference (0)"),
         col = "red", lty = 2)
}


visualize_fitness_difference <- function(folder_path) {
  # Get all fitness files: e.g., 0_fitness.csv, 1_fitness.csv, ...
  fitness_files <- list.files(path = folder_path,
                              pattern = "_fitness\\.csv$",
                              full.names = TRUE)
  
  all_diff <- c()
  
  for (fit_file in fitness_files) {
    # Extract run id from filename (before the first "_")
    base <- basename(fit_file)                # "0_fitness.csv"
    run_id <- strsplit(base, "_")[[1]][1]     # "0"
    
    # Build corresponding hyperparameters filename
    hyper_file <- file.path(folder_path, paste0(run_id, "_hyperparameters.csv"))
    if (!file.exists(hyper_file)) {
      warning("Hyperparameters file not found for run: ", run_id)
      next
    }
    
    # Read true fitness for this run
    hyper_data <- read.csv(hyper_file)
    true_fitness <- hyper_data$True.BIC[1]    # or True.AIC if that's the correct column
    
    # Read obtained fitness for this run
    fitness_data <- read.csv(fit_file)
    found_fitness <- fitness_data$Fitness
    # Optional filter, if needed:
    # found_fitness <- found_fitness[found_fitness >= 0 & found_fitness <= 1e4]
    
    # Difference: found - true (change sign if you prefer true - found)
    diff_values <- found_fitness - true_fitness
    
    all_diff <- c(all_diff, diff_values)
  }
  
  if (length(all_diff) == 0) {
    warning("No fitness differences to plot.")
    return(invisible(NULL))
  }
  
  ylim <- range(all_diff, 0)
  boxplot(all_diff,
          main = "Difference Between Found and True Fitness Across Runs",
          ylab = "Found fitness - True fitness",
          ylim = ylim)
  abline(h = 0, col = "red", lty = 2)
  legend("topright",
         legend = c("No difference (0)"),
         col = "red", lty = 2)
}


visualize_time_distribution <- function(folder_path) {
  fitness_files <- list.files(path = folder_path, pattern = "*_time.csv", full.names = TRUE)
  all_fitness <- c()
  
  for (file in fitness_files) {
    fitness_data <- read.csv(file)
    all_fitness <- c(all_fitness, fitness_data$ComputationTime)
  }
  
  ylim <- range(all_fitness)
  boxplot(all_fitness, main = "Time Distribution Across All Runs", ylab = "Time")
  cat("Median computation time:", median(all_fitness), "\n")
}


is_matrix_contained <- function(specific_matrix, candidate_matrix) {
  all(specific_matrix == candidate_matrix | specific_matrix == 0)
}

is_matrix_equal <- function(specific_matrix, candidate_matrix) {
  all(specific_matrix == candidate_matrix)
}

check_matrices <- function(folder_path,
                           specific_matrix,
                           print_examples = FALSE,
                           num_examples = 1,
                           verbose = FALSE) {
  if (!file.exists(folder_path)) {
    stop("Invalid folder path.")
  }
  
  best_files <- list.files(path = folder_path, pattern = "*_best.csv", full.names = TRUE)
  total <- length(best_files)
  if (total == 0) {
    warning("No *_best.csv files found in the folder.")
    return(invisible(NULL))
  }
  
  count_cont <- 0
  count_cont_strict <- 0
  count_equal <- 0
  count_rc <- 0
  cont_strict_examples <- list()
  rc_examples <- list()
  
  for (file in best_files) {
    candidate_matrix <- as.matrix(read.csv(file, row.names = 1))
    
    # original contained in generated
    if (is_matrix_contained(specific_matrix, candidate_matrix)) {
      count_cont <- count_cont + 1
      if (!is_matrix_equal(specific_matrix, candidate_matrix)) {
        count_cont_strict <- count_cont_strict + 1
        if (verbose && print_examples && length(cont_strict_examples) < num_examples) {
          cont_strict_examples[[length(cont_strict_examples) + 1]] <- candidate_matrix
        }
      }
    }
    
    # equal
    if (is_matrix_equal(specific_matrix, candidate_matrix)) {
      count_equal <- count_equal + 1
    }
    
    # generated strictly contained in original
    if (is_matrix_contained(candidate_matrix, specific_matrix) &&
        !is_matrix_equal(specific_matrix, candidate_matrix)) {
      count_rc <- count_rc + 1
      if (verbose && print_examples && length(rc_examples) < num_examples) {
        rc_examples[[length(rc_examples) + 1]] <- candidate_matrix
      }
    }
  }
  
  # success rate = equal OR strictly contained in the original
  success_rate <- (count_equal + count_rc) / total
  
  # ===========================
  #        VERBOSE TRUE
  # ===========================
  if (verbose) {
    cat("\n================== Matrix Containment Summary ==================\n")
    cat("Total matrices analyzed:", total, "\n\n")
    
    cat("1. Original matrix contained in generated matrices:\n")
    cat("   - Total occurrences:", count_cont, "\n")
    cat("   - Proportion:", round(count_cont / total, 4), "\n\n")
    
    cat("2. Original matrix strictly contained in generated matrices (not equal):\n")
    cat("   - Total occurrences:", count_cont_strict, "\n")
    cat("   - Proportion:", round(count_cont_strict / total, 4), "\n\n")
    
    cat("3. Original matrix equal to generated matrices:\n")
    cat("   - Total occurrences:", count_equal, "\n")
    cat("   - Proportion:", round(count_equal / total, 4), "\n\n")
    
    cat("4. Generated matrices strictly contained in original matrix:\n")
    cat("   - Total occurrences:", count_rc, "\n")
    cat("   - Proportion:", round(count_rc / total, 4), "\n\n")
    
    cat("5. Success rate (equal OR strictly contained in original):\n")
    cat("   - Success rate:", round(success_rate, 4), "\n")
    
    if (print_examples) {
      if (length(cont_strict_examples) > 0) {
        cat("\nExamples of matrices strictly contained in generated matrices:\n")
        for (i in seq_along(cont_strict_examples)) {
          cat("\nExample", i, "(Strictly Contained in Generated):\n")
          print(cont_strict_examples[[i]])
        }
      }
      if (length(rc_examples) > 0) {
        cat("\nExamples of generated matrices strictly contained in the original matrix:\n")
        for (i in seq_along(rc_examples)) {
          cat("\nExample", i, "(Strictly Contained in Original):\n")
          print(rc_examples[[i]])
        }
      }
    }
    
    cat("\n===============================================================\n")
    
    return(invisible(NULL))
  }
  
  # ===========================
  #        VERBOSE FALSE
  # ===========================
  cat("Number of samples:", total, "\n")
  cat("Success rate:", round(success_rate, 4), "\n")
  return(invisible(NULL))
}



calculate_mean_matrix <- function(folder_path, dimension) {
  best_files <- list.files(path = folder_path, pattern = "_best\\.csv$", full.names = TRUE)
  
  sum_matrix <- matrix(0, nrow = dimension, ncol = dimension)
  used_count <- 0
  
  for (best_file in best_files) {
    
    # 1) costruisci il nome del file fitness
    fitness_file <- file.path(dirname(best_file),
                              paste0(gsub("_best\\.csv$", "", basename(best_file)),
                                     "_fitness.csv"))
    
    # 2) leggi il fitness come fai per hyperparameters
    fitness_data <- read.csv(fitness_file)
    fitness_value <- fitness_data$Fitness[1]

    # 3) controlla threshold
    # if (fitness_value >= 1e3) {
    #   next
    # }
    
    # if (fitness_value < 0) {
    #   next
    # }
    
    # 4) usa la matrice
    candidate_matrix <- as.matrix(read.csv(best_file, row.names = 1))
    sum_matrix <- sum_matrix + candidate_matrix
    used_count <- used_count + 1
  }
  
  if (used_count == 0) {
    cat("No matrices passed the threshold.\n")
    return(matrix(0, nrow = dimension, ncol = dimension))
  }
  
  mean_matrix <- sum_matrix / used_count
  # print(round(mean_matrix, 2))
  cat("Number of samples:", used_count, "\n")
  return(round(mean_matrix, 2))
}

find_top_matrices <- function(folder_path, k = 3, decreasing = FALSE) {
  best_files <- list.files(
    path = folder_path,
    pattern = "_best\\.csv$",
    full.names = TRUE
  )
  
  if (length(best_files) == 0) {
    cat("No best files found.\n")
    return(NULL)
  }
  
  matrices  <- list()
  fitnesses <- numeric(0)
  keys      <- character(0)
  
  for (best_file in best_files) {
    # corresponding fitness file
    fitness_file <- sub("_best\\.csv$", "_fitness.csv", best_file)
    if (!file.exists(fitness_file)) {
      next
    }
    
    fitness_data  <- read.csv(fitness_file)
    fitness_value <- fitness_data$Fitness[1]
    
    # fitness filter
    # if (is.na(fitness_value) || fitness_value < 0 || fitness_value > 1e3) {
    #  next
    # }
    
    # read matrix
    m <- as.matrix(read.csv(best_file, row.names = 1))
    
    # create a unique key for this matrix
    key <- paste(c(m), collapse = ",")
    
    matrices[[length(matrices) + 1]] <- m
    fitnesses <- c(fitnesses, fitness_value)
    keys <- c(keys, key)
  }
  
  if (length(matrices) == 0) {
    cat("No matrices passed the fitness filter.\n")
    return(NULL)
  }
  
  # compute frequencies of identical matrices
  freq <- table(keys)
  
  # sort by fitness
  ord <- order(fitnesses, decreasing = decreasing)
  
  # retain only top k ranked by fitness
  ord <- ord[seq_len(min(k, length(ord)))]
  
  result <- vector("list", length(ord))
  for (i in seq_along(ord)) {
    idx <- ord[i]
    key_i <- keys[idx]
    result[[i]] <- list(
      matrix  = matrices[[idx]],
      fitness = fitnesses[idx],
      count   = as.integer(freq[key_i])   # how many times this matrix appears
    )
  }
  
  names(result) <- paste0("top_", seq_along(result))
  return(result)
}


find_top_5_frequent_matrices <- function(folder_path) {
  variables <- c("eta1", "eta2", "eta3", "eta4", "eta5", "eta6") 
  
  measurement_model <- list(
    eta1 = c("y1", "y2", "y3"),
    eta2 = c("y4", "y5", "y6"),
    eta3 = c("y7", "y8", "y9"), 
    eta4 = c("y10", "y11", "y12"),
    eta5 = c("y13", "y14", "y15"),
    eta6 = c("y16", "y17", "y18")
  )
  
  type_of_variable <- c(eta1 = "composite", eta2 = "composite", 
                        eta3 = "composite", eta4 = "composite",
                        eta5 = "composite", eta6 = "composite")
  structural_coefficients <- list()
  
  best_files <- list.files(path = folder_path, pattern = "*_best.csv", full.names = TRUE)
  total_matrices <- length(best_files)
  
  if (total_matrices == 0) {
    cat("No matrices found.\n")
    return(NULL)
  }
  
  matrix_list <- list()
  
  for (file in best_files) {
    candidate_matrix <- as.matrix(read.csv(file, row.names = 1))
    matrix_list[[length(matrix_list) + 1]] <- candidate_matrix
  }
  
  matrix_string_list <- sapply(matrix_list, function(m) paste(as.vector(t(m)), collapse = ","))
  matrix_freq_table <- table(matrix_string_list)
  
  top_5 <- head(sort(matrix_freq_table, decreasing = TRUE), 5)
  top_5_matrices <- list()
  for (matrix_string in names(top_5)) {
    matrix_values <- as.numeric(strsplit(matrix_string, ",")[[1]])
    matrix_dim <- sqrt(length(matrix_values))  # Assuming square matrices
    matrix_top <- matrix(matrix_values, nrow = matrix_dim, byrow = TRUE)  # Use byrow = TRUE for proper row order
    top_5_matrices[[length(top_5_matrices) + 1]] <- list(matrix = matrix_top, frequency = top_5[[matrix_string]])
  }
  
  cat("Top 5 most frequent matrices, their frequencies (percentage), and SEM model strings:\n")
  for (i in 1:length(top_5_matrices)) {
    percentage <- (top_5_matrices[[i]]$frequency / total_matrices) * 100
    
    sem_model_string <- create_sem_model_string_from_matrix_small(
      adj_matrix = top_5_matrices[[i]]$matrix, 
      variables = variables, 
      measurement_model = measurement_model, 
      structural_coefficients = structural_coefficients, 
      type_of_variable = type_of_variable
    )
    
    cat("\nSEM Model", i, "-- Frequency:", round(percentage, 2), "%:\n", sem_model_string, "\n")
  }
  
  return(top_5_matrices)
}



process_p_values_directory <- function(input_dir) {
  output_dir <- file.path(dirname(input_dir), "p_values_for_tkz")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  files <- list.files(input_dir, full.names = TRUE)
  
  for (file_path in files) {
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    subdirectory <- file.path(output_dir, paste0("bp_", file_name))
    if (!dir.exists(subdirectory)) {
      dir.create(subdirectory)
    }
    
    data <- read_delim(file_path, delim = "\t", , show_col_types = FALSE)
    data <- data[-1, ]
    
    for (i in 1:nrow(data)) {
      connection <- gsub(" ~ ", "_", data$Connection[i])
      non_na_values <- as.numeric(data[i, -1])
      non_na_values <- non_na_values[!is.na(non_na_values)]
      file_name <- paste0(subdirectory, "/bp_", connection, ".txt")
      write.table("res", file = file_name, row.names = FALSE, col.names = FALSE, quote = FALSE)
      write.table(non_na_values, file = file_name, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}
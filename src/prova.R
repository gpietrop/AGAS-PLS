library(GA)
library(optparse)

source("utils.R")

sat_matrix <- matrix(c(
  0, 0, 0, 0, 0, 0,  # IMAG dependencies
  0, 0, 1, 1, 0, 0,  # EXPE dependencies
  0, 0, 0, 1, 1, 0,  # QUAL dependencies
  0, 0, 0, 0, 0, 0,  # VAL dependencies
  1, 0, 0, 1, 0, 0,  # SAT dependencies
  1, 1, 1, 0, 1, 0   # LOY dependencies
), nrow = 6, byrow = TRUE)

sat_string <- create_sem_model_string_from_matrix_satisfaction(sat_matrix)
sat_out <- csem(.data = satisfaction,.model = sat_string)
sat_criteria <- calculateModelSelectionCriteria(sat_out, .by_equation = FALSE)
bic_true <- sat_criteria$BIC
print(bic_true)

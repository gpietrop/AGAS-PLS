library(GA)
library(optparse)

source("utils.R")

modelTrans='  
EXPE~IMAG
QUAL ~ EXPE
VAL ~ EXPE + QUAL
SAT ~ IMAG + EXPE + QUAL + VAL
LOY ~ IMAG + SAT + COMP
COMP ~ SAT

IMAG <~ Image1 + Image2 + Image3 + Image4 + Image5 
EXPE <~ Expec1 + Expec2 + Expec3 
QUAL <~ PerQual1 + PerQual2 + PerQual3 + PerQual3 + PerQual5 + PerQual6 + PerQual7
VAL <~ PerVal1 + PerVal2
COMP <~ Compl
SAT<~ Satis1 + Satis2 + Satis3
LOY <~ Loyal1 + Loyal2 + Loyal3
'
# outTrans=csem(.data = dataTrans,.model = modelTrans,.PLS_modes = 'modeA',.PLS_weight_scheme_inner = 'path',.resample_method="bootstrap")
# summarize(outTrans)

gen_matrix <- matrix(c(
  0, 0, 0, 0, 0, 0, 0,  # IMAG dependencies
  0, 0, 1, 1, 0, 0, 0,  # EXPE dependencies
  1, 0, 0, 1, 0, 0, 0,  # QUAL dependencies
  1, 0, 0, 0, 0, 0, 0,  # VAL dependencies
  0, 0, 1, 1, 0, 0, 0,  # SAT dependencies
  0, 0, 0, 0, 1, 0, 0,  # LOY dependencies
  0, 0, 1, 0, 1, 0, 0  # COMP dependencies
), nrow = 7, byrow = TRUE)

true_matrix <- matrix(c(
  0, 0, 0, 0, 0, 0, 0,  # IMAG dependencies
  1, 0, 0, 0, 0, 0, 0,  # EXPE dependencies
  0, 1, 0, 0, 0, 0, 0,  # QUAL dependencies
  0, 1, 1, 0, 0, 0, 0,  # VAL dependencies
  1, 1, 1, 1, 0, 0, 0,  # SAT dependencies
  1, 0, 0, 0, 1, 0, 1,  # LOY dependencies
  0, 0, 0, 0, 1, 0, 0  # COMP dependencies
), nrow = 7, byrow = TRUE)

data <- read.csv("ds/data_ecsi.csv", sep = ";")
sat_string <- create_sem_model_string_from_matrix_trans(gen_matrix)
# print(sat_string)
sat_out <- csem(.data = data,.model = sat_string, .PLS_weight_scheme_inner = 'centroid')
sat_criteria <- calculateModelSelectionCriteria(sat_out, .by_equation = FALSE)
bic_true <- sat_criteria$BIC
print(bic_true)


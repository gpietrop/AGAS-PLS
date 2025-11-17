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

IMAG =~ Image1 + Image2 + Image3 + Image4 + Image5 
EXPE =~ Expec1 + Expec2 + Expec3 
QUAL =~ PerQual1 + PerQual2 + PerQual3 + PerQual3 + PerQual5 + PerQual6 + PerQual7
VAL =~ PerVal1 + PerVal2
COMP =~ Compl
SAT =~ Satis1 + Satis2 + Satis3
LOY =~ Loyal1 + Loyal2 + Loyal3
'
# outTrans=csem(.data = dataTrans,.model = modelTrans,.PLS_modes = 'modeA',.PLS_weight_scheme_inner = 'path',.resample_method="bootstrap")
# summarize(outTrans)

gen_matrix <- matrix(c(
  0, 0, 0, 0, 0, 0, 0,  # IMAG dependencies
  0, 0, 0, 0, 0, 0, 0,  # EXPE dependencies
  0, 0, 0, 0, 0, 0, 0,  # QUAL dependencies
  1, 0, 1, 0, 1, 0, 0,  # VAL dependencies
  0, 0, 0, 0, 0, 0, 0,  # SAT dependencies
  1, 1, 1, 1, 1, 0, 0,  # LOY dependencies
  1, 1, 0, 0, 1, 1, 0  # COMP dependencies
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

tam_matrix <- matrix(c(
  0, 0, 0, 0, 0,  # EOI dependencies
  1, 0, 0, 0, 0,  # USEF dependencies
  1, 1, 0, 0, 0,  # ATT dependencies
  0, 1, 1, 0, 0,  # BI dependencies
  0, 0, 1, 1, 0   # USE dependencies
), nrow = 5, byrow = TRUE)

gen_tam_matrix <- matrix(c(
  0, 0, 0, 0, 0,  # EOI dependencies
  1, 0, 1, 1, 0,  # USEF dependencies
  0, 0, 0, 0, 0,  # ATT dependencies
  0, 0, 0, 0, 0,  # BI dependencies
  1, 0, 1, 0, 0   # USE dependencies
), nrow = 5, byrow = TRUE)

modelTAM_gen='  
# strucutral model
  USEF ~ EOI + BI + ATT
  USE ~ EOI + ATT
# outer model - composites
  EOI <~ EOU1 + EOU2 + EOU3 + EOU4 + EOU5
  USEF <~ USEF1 + USEF2 + USEF3 + USEF4 + USEF5
  BI <~ BI1 + BI2 +  BI3   
  ATT <~ ATT1 + ATT2 + ATT3 + ATT4 + ATT5
  USE <~ USE1 + USE2 + USE3 + USE4 
'

ova_matrix <- matrix(c(
  0, 0, 0, 0,  # OP dependencies
  1, 0, 0, 0,  # OI dependencies
  0, 1, 0, 0,  # ACJ dependencies
  0, 1, 0, 0   # ACL dependencies
), nrow = 4, byrow = TRUE)

gen_ova_matrix <- matrix(c(
  0, 0, 0, 0,  # OP dependencies
  0, 0, 0, 0,  # OI dependencies
  1, 1, 0, 0,  # ACJ dependencies
  0, 1, 1, 0   # ACL dependencies
), nrow = 4, byrow = TRUE)

data <- read.csv("ds/data_ecsi.csv", sep = ";")
dati_TAM <-  read.csv("ds/Data_TAM.txt", sep=";", stringsAsFactors=TRUE)
dati_OVA <-  read.csv("ds/Data_OIvariations.txt", sep=";", stringsAsFactors=TRUE)

sat_string <- create_sem_model_string_from_matrix_tam(gen_tam_matrix)
# print(sat_string)
sat_out <- csem(.data = dati_TAM,.model = sat_string, .PLS_modes = 'modeA')
sat_criteria <- calculateModelSelectionCriteria(sat_out, .by_equation = FALSE)
bic_true <- sat_criteria$BIC
print(bic_true)


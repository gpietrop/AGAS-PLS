
library(cSEM)

#####################################################################################
# Example of TAM model using cSEM package
# Data from SmartPLS website
# https://www.smartpls.com/documentation/sample-projects/tam
#####################################################################################
dati_TAM <-  read.csv("~/Library/CloudStorage/Dropbox/Stat/On.going.works/Automated PLS-PM/Automatic search PLS _ Gloria/Data Smart PLS examples/Data_TAM.txt", sep=";", stringsAsFactors=TRUE)

modelTAM='  
# strucutral model
  USEF ~ EOI
  ATT ~ EOI + USEF
  BI ~ ATT + USEF
  USE ~ BI + ATT
# outer model - composites
  EOI <~ EOU1 + EOU2 + EOU3 + EOU4 + EOU5
  USEF <~ USEF1 + USEF2 + USEF3 + USEF4 + USEF5
  BI <~ BI1 + BI2 +  BI3   
  ATT <~ ATT1 + ATT2 + ATT3 + ATT4 + ATT5
  USE <~ USE1 + USE2 + USE3 + USE4 
'

outTAM=csem(.data = dati_TAM,.model = modelTAM,.PLS_modes = 'modeA')
summarize(outTAM)

#####################################################################################
# Example of Organizational Identification Model model using cSEM package
# Data from SmartPLS website
# https://www.smartpls.com/documentation/sample-projects/organizational-identification-variations-pls
#####################################################################################

dati_OI <-  read.csv("~/Library/CloudStorage/Dropbox/Stat/On.going.works/Automated PLS-PM/Automatic search PLS _ Gloria/Data Smart PLS examples/Data_OIvariations.txt", sep=";", stringsAsFactors=TRUE)

modelOI='  
# strucutral model
  OI ~ OP
  ACJ ~ OI
  ACL ~ OI
# outer model - composites
  OP <~ org_pre1 + org_pre2 + org_pre3 + org_pre4 + org_pre5 + org_pre6 +  org_pre7 + org_pre8 
  OI <~ org_ident1 + org_ident2 + org_ident3 + org_ident4 + org_ident5 + org_ident6
  ACJ <~ ac_joy1 + ac_joy2 + ac_joy3 + ac_joy4
  ACL <~ ac_love1 + ac_love2 + ac_love3
'

outOI=csem(.data = dati_OI,.model = modelOI,.PLS_modes = 'modeA')
summarize(outOI)





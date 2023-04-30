################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 2

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(table1)

##### Generate Tables ##########################################################
# Urinary Arsenic Tertiles
(tbl2 <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | uAs3, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), 
  extra.col = list(`p` = tbl_pval)))


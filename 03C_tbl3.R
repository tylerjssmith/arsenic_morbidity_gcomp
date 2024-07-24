################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 3

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Combine Estimates ########################################################
(tbl3 <- rbind(
  # Drinking Water Arsenic (wAs)
  tbl_incidence(x = wAs1, xlab = "wAs1"),
  tbl_incidence(x = wAs10, xlab = "wAs10"),
  tbl_incidence(x = wAs50, xlab = "wAs50"),
  
  # Urinary Arsenic (∑uAs)
  tbl_incidence(x = uAs_p10, xlab = "uAs_p10"),
  tbl_incidence(x = uAs_p25, xlab = "uAs_p25"),
  tbl_incidence(x = uAs_p50, xlab = "uAs_p50")
))

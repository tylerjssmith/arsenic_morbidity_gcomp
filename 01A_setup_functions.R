################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Functions

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Functions: Check Covariates ##############################################

##### Functions: Generalized Additive Models ###################################
gam_wrapper <- function(data, formula, link = "binomial") 
{
 gam(formula, data = data, family = link, method = "REML")
}

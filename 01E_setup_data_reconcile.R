################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Reconcile

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Assess Selection #########################################################
# Enrolled (n=784)
df %>% 
  nrow()

# Live Births (n=736)
df %>% 
  count(LIVEBIRTH)

# Singleton Live Births (n=722)
df %>% 
  filter(LIVEBIRTH == 1) %>% 
  count(SINGLETON)

# Complete Covariate Data
df %>% 
  filter(LIVEBIRTH == 1) %>% 
  filter(SINGLETON == 1) %>% 
  sapply(function(x) sum(is.na(x)))

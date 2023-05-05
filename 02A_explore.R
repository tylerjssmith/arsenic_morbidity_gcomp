################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Explore

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mgcv)

##### Summarize Variables ######################################################
# Values <LLOD
df %>% 
  filter(COMPLETE == 1) %>% 
  count(wAs < 0.02)

# Urinary Arsenobetaine
df %>% 
  select(uAsB) %>%
  na.omit() %>%
  summarise(
    n = n(), 
    median = median(uAsB), 
    q1 = quantile(uAsB, 0.25), 
    q3 = quantile(uAsB, 0.75)
  )


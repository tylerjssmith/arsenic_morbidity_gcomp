################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- G-Computation

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Run G-Computation ########################################################
# Weeks
df_fig5_weeks <- rbind(
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["wAs1"]], 
      A = "wAs1"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["wAs10"]], 
      A = "wAs10"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["wAs50"]], 
      A = "wAs50"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["uAs_p10"]], 
      A = "uAs_p10"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["uAs_p25"]], 
      A = "uAs_p25"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_weeks[["uAs_p50"]], 
      A = "uAs_p50")
)

# Cases
df_fig5_cases <- rbind(
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["wAs1"]], 
      A = "wAs1"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["wAs10"]], 
      A = "wAs10"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["wAs50"]], 
      A = "wAs50"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["uAs_p10"]], 
      A = "uAs_p10"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["uAs_p25"]], 
      A = "uAs_p25"),
  df_selected %>% 
    gcomp(model = poisson_adj_fit_cases[["uAs_p50"]], 
      A = "uAs_p50")
)


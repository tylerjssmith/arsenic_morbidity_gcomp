################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- G-Computation

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Outcome: ILI Weeks #######################################################
# wAs Standard: 1 µg/L
gcomp_weeks_wAs1 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "wAs1", R = 100000)

# wAs Standard: 10 µg/L
gcomp_weeks_wAs10 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "wAs10", R = 100000)

# wAs Standard: 50 µg/L
gcomp_weeks_wAs50 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "wAs50", R = 100000)

# Continuous: ln wAs
gcomp_weeks_ln_wAs <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "ln_wAs", R = 100000)

# ∑uAs Percentile: 10th
gcomp_weeks_uAs_p10 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "uAs_p10", R = 100000)

# ∑uAs Percentile: 25th
gcomp_weeks_uAs_p25 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "uAs_p25", R = 100000)

# ∑uAs Percentile: 50th
gcomp_weeks_uAs_p50 <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "uAs_p50", R = 100000)

# Continuous: ln ∑uAs
gcomp_weeks_ln_uAs <- df_selected %>% 
  gcomp(y = "ILI_WEEKS", x = "ln_uAs", R = 100000)

##### Outcome: ILI Weeks #######################################################
# wAs Standard: 1 µg/L
gcomp_cases_wAs1 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "wAs1", R = 100000)

# wAs Standard: 10 µg/L
gcomp_cases_wAs10 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "wAs10", R = 100000)

# wAs Standard: 50 µg/L
gcomp_cases_wAs50 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "wAs50", R = 100000)

# Continuous: ln wAs
gcomp_cases_ln_wAs <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "ln_wAs", R = 100000)

# ∑uAs Percentile: 10th
gcomp_cases_uAs_p10 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "uAs_p10", R = 100000)

# ∑uAs Percentile: 25th
gcomp_cases_uAs_p25 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "uAs_p25", R = 100000)

# ∑uAs Percentile: 50th
gcomp_cases_uAs_p50 <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "uAs_p50", R = 100000)

# Continuous: ln ∑uAs
gcomp_cases_ln_uAs <- df_selected %>% 
  gcomp(y = "ILI_CASES", x = "ln_uAs", R = 100000)

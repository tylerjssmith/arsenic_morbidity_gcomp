################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 4

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Combine Estimates ########################################################
# Combine Estimates
df_fig4 <- rbind(
  # ILI Weeks: Unadjusted
  poisson_cru_est_weeks %>%
    filter(term == ".x") %>%
    mutate(outcome = "Weeks"),

  # ILI Weeks: Adjusted
  poisson_adj_est_weeks %>%
    filter(term == ".x") %>%
    mutate(outcome = "Weeks"),
  
  # ILI Cases: Unadjusted
  poisson_cru_est_cases %>%
    filter(term == ".x") %>%
    mutate(outcome = "Cases"),

  # ILI Cases: Adjusted
  poisson_adj_est_cases %>%
    filter(term == ".x") %>%
    mutate(outcome = "Cases")
)

##### Generate Table ###########################################################
tbl4 <- df_fig4 %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ exp(.x))) %>%
  select(outcome, A, estimate, conf.low, conf.high, p.value, set) %>%
  pivot_wider(id_cols = c(outcome,A), names_from = set, values_from = c(estimate, conf.low, conf.high, p.value)) %>%
  mutate(across(-c(outcome,A), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(unadjusted = paste0(estimate_Unadjusted, " (", conf.low_Unadjusted, ", ", conf.high_Unadjusted, ")")) %>%
  mutate(adjusted = paste0(estimate_Adjusted, " (", conf.low_Adjusted, ", ", conf.high_Adjusted, ")")) %>%
  mutate(type = ifelse(grepl("wAs", A), "Water", "Urine")) %>%
  select(type, outcome, A, unadjusted, p.value_Unadjusted, adjusted, p.value_Adjusted) %>%
  arrange(desc(type), outcome, desc(A))

tbl4 %>% head()

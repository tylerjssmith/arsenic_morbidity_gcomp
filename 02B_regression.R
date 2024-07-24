################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Regression Models

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(car)

##### Figure: Conditional Response Variables ###################################
df_selected %>%
  select(ILI_WEEKS,wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50) %>%
  pivot_longer(-ILI_WEEKS, names_to = "Standard", values_to = "Exceeds") %>%
  ggplot(aes(x = ILI_WEEKS)) +
  geom_histogram(stat = "count") +
  facet_grid(Exceeds ~ Standard) +
  labs(
    x = "ILI Weeks",
    y = "Frequency") +
  th

df_selected %>%
  select(ILI_WEEKS,wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50) %>%
  pivot_longer(-ILI_WEEKS, names_to = "Standard", values_to = "Exceeds") %>%
  ggplot(aes(x = ILI_WEEKS)) +
  geom_histogram(stat = "count") +
  facet_grid(Exceeds ~ Standard) +
  labs(
    x = "ILI Cases",
    y = "Frequency") +
  th

##### Fit Poisson Models: Unadjusted ###########################################
# Fit Models
poisson_cru_fit_weeks <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50,ln_wAs,ln_uAs) %>%
  map(~ glm(ILI_WEEKS ~ .x + offset(log(WEEKS)), 
    data = df_selected, family = quasipoisson))

poisson_cru_fit_cases <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50,ln_wAs,ln_uAs) %>%
  map(~ glm(ILI_CASES ~ .x + offset(log(WEEKS)), 
    data = df_selected, family = quasipoisson))

# Extract Estimates
poisson_cru_est_weeks <- poisson_cru_fit_weeks %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(set = "Unadjusted")

poisson_cru_est_weeks %>% 
  filter(term == ".x") %>% 
  head(n = 8)

poisson_cru_est_cases <- poisson_cru_fit_cases %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(set = "Unadjusted")

poisson_cru_est_cases %>% 
  filter(term == ".x") %>% 
  head(n = 8)

##### Fit Poisson Models: Adjusted #############################################
# Fit Models
poisson_adj_fit_weeks <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50,ln_wAs,ln_uAs) %>%
  map(~ glm(ILI_WEEKS ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe + offset(log(WEEKS)), 
    data = df_selected, family = quasipoisson))

poisson_adj_fit_cases <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,uAs_p25,uAs_p50,ln_wAs,ln_uAs) %>%
  map(~ glm(ILI_CASES ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe + offset(log(WEEKS)), 
    data = df_selected, family = quasipoisson))

# Check Multicollinearity
poisson_adj_fit_weeks %>%
  map(vif) %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  filter(`GVIF^(1/(2*Df))` > 2)

poisson_adj_fit_cases %>%
  map(vif) %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  filter(`GVIF^(1/(2*Df))` > 2)

# Extract Estimates
poisson_adj_est_weeks <- poisson_adj_fit_weeks %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(set = "Adjusted")

poisson_adj_est_weeks %>% 
  filter(term == ".x") %>% 
  head(n = 8)

poisson_adj_est_cases <- poisson_adj_fit_cases %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(set = "Adjusted")

poisson_adj_est_cases %>% 
  filter(term == ".x") %>% 
  head(n = 8)










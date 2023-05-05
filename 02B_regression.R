################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Regression Models

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(car)
library(AER)

##### Figure: Conditional Response Variables ###################################
df_selected %>%
  select(CASES,wAs1,wAs10,wAs50,uAs_p10) %>%
  pivot_longer(-CASES, names_to = "Standard", values_to = "Exceeds") %>%
  ggplot(aes(x = CASES)) +
  geom_histogram(stat = "count") +
  facet_grid(Exceeds ~ Standard) +
  labs(
    x = "Cases",
    y = "Frequency") +
  th

##### Fit Poisson Models: Unadjusted ###########################################
# Fit Models
poisson_cru_fit <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,ln_wAs,ln_uAs) %>%
  map(~ glm(CASES ~ .x, 
    offset = log(WEEKS), data = df_selected, family = quasipoisson))

# Extract Estimates
poisson_cru_est <- poisson_cru_fit %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(Set = "Unadjusted")

poisson_cru_est %>% head()

##### Fit Poisson Models: Adjusted #############################################
# Fit Models
poisson_adj_fit <- df_selected %>%
  select(wAs1,wAs10,wAs50,uAs_p10,ln_wAs,ln_uAs) %>%
  map(~ glm(CASES ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
    offset = log(WEEKS), data = df_selected, family = quasipoisson))

# Check Multicollinearity
poisson_adj_fit %>%
  map(vif)

# Extract Estimates
poisson_adj_est <- poisson_adj_fit %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  mutate(Set = "Adjusted")

poisson_adj_est %>% head()

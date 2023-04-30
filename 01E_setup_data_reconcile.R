################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Reconcile

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mgcv)
library(broom)

##### Assess Selection #########################################################
# Enrolled (n=784)
df %>% nrow()

# Live Births (n=736)
df %>% count(LIVEBIRTH)

# Singleton Live Births (n=722)
df %>% 
  filter(LIVEBIRTH == 1) %>% 
  count(SINGLETON)

# Complete Covariate Data
df %>% 
  filter(LIVEBIRTH == 1) %>% 
  filter(SINGLETON == 1) %>% 
  sapply(function(x) sum(is.na(x))) %>%
  as_tibble(rownames = "Variable") %>%
  filter(value != 0)

##### Live Birth ###############################################################
df %>% count(LIVEBIRTH)

# Models: Arsenic
df %>%
  select(ln_wAs,ln_uAs) %>%
  map(~ gam(LIVEBIRTH ~ s(.x), data = df, family = "binomial", 
    method = "REML")) %>%
  map_dfr(tidy, .id = "x")

df %>%
  select(ln_wAs,ln_uAs,wAs1,wAs10,wAs50) %>%
  map(~ glm(LIVEBIRTH ~ .x, data = df, family = "binomial")) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "x") %>%
  filter(term == ".x")

# Models: All Covariates
df %>%
  select(ln_wAs,ln_uAs) %>%
  map(~ gam(LIVEBIRTH ~ s(.x) + s(AGE) + SEGSTAGE + PARITY + EDUCATION + 
      s(LSI) + s(medSEMUAC) + PETOBAC + PEBETEL + PEHCIGAR, data = df, 
    family = "binomial", method = "REML")) %>%
  map_dfr(tidy, .id = "x")

df %>%
  select(ln_wAs,ln_uAs,wAs1,wAs10,wAs50) %>%
  map(~ glm(LIVEBIRTH ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      poly(medSEMUAC, 2) + PETOBAC + PEBETEL + PEHCIGAR, data = df, 
    family = "binomial")) %>%
  map_dfr(tidy, conf.int = TRUE, exponentiate = TRUE, .id = "x") %>%
  filter(term != "(Intercept)") %>%
  filter(p.value < 0.2) %>%
  arrange(term)

##### Singleton | Live Birth ###################################################
df %>% count(SINGLETON)

# Models: Arsenic
df %>%
  select(ln_wAs,ln_uAs) %>%
  map(~ gam(SINGLETON ~ s(.x), data = df, family = "binomial", 
    method = "REML")) %>%
  map_dfr(tidy, .id = "x")

df %>%
  select(ln_wAs,ln_uAs,wAs1,wAs10,wAs50) %>%
  map(~ glm(SINGLETON ~ .x, data = df, family = "binomial")) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "x") %>%
  filter(term == ".x")

# Models: All Covariates
df %>%
  select(ln_wAs,ln_uAs) %>%
  map(~ gam(SINGLETON ~ s(.x) + s(AGE) + SEGSTAGE + PARITY + EDUCATION + 
      s(LSI) + s(medSEMUAC) + PETOBAC + PEBETEL + PEHCIGAR, data = df, 
    family = "binomial", method = "REML")) %>%
  map_dfr(tidy, .id = "x")

df %>%
  select(ln_wAs,ln_uAs,wAs1,wAs10,wAs50) %>%
  map(~ glm(SINGLETON ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR, data = df, 
    family = "binomial")) %>%
  map_dfr(tidy, conf.int = TRUE, exponentiate = TRUE, .id = "x") %>%
  filter(term != "(Intercept)") %>%
  filter(p.value < 0.2) %>%
  arrange(term)



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

##### Assess Missingness #######################################################
df %>% count(LIVEBIRTH)
df %>% count(SINGLETON)

# Models: Live Birth by Arsenic
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

# (Singleton | Live Birth) by Arsenic
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

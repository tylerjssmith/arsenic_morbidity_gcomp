################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 5

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Load Saved Model Output ##################################################
# Set Working Directory
setwd("../gcomp_objects/")

# Drinking Water Arsenic
gcomp_cases_wAs50  <- readRDS("gcomp_cases_wAs50.RDS")
gcomp_cases_wAs10  <- readRDS("gcomp_cases_wAs10.RDS")
gcomp_cases_wAs1   <- readRDS("gcomp_cases_wAs1.RDS")
gcomp_cases_ln_wAs <- readRDS("gcomp_cases_ln_wAs.RDS")

gcomp_weeks_wAs50  <- readRDS("gcomp_weeks_wAs50.RDS")
gcomp_weeks_wAs10  <- readRDS("gcomp_weeks_wAs10.RDS")
gcomp_weeks_wAs1   <- readRDS("gcomp_weeks_wAs1.RDS")
gcomp_weeks_ln_wAs <- readRDS("gcomp_weeks_ln_wAs.RDS")

# Urinary Arsenic
gcomp_cases_uAs_p50 <- readRDS("gcomp_cases_uAs_p50.RDS")
gcomp_cases_uAs_p25 <- readRDS("gcomp_cases_uAs_p25.RDS")
gcomp_cases_uAs_p10 <- readRDS("gcomp_cases_uAs_p10.RDS")
gcomp_cases_ln_uAs  <- readRDS("gcomp_cases_ln_uAs.RDS")

gcomp_weeks_uAs_p50 <- readRDS("gcomp_weeks_uAs_p50.RDS")
gcomp_weeks_uAs_p25 <- readRDS("gcomp_weeks_uAs_p25.RDS")
gcomp_weeks_uAs_p10 <- readRDS("gcomp_weeks_uAs_p10.RDS")
gcomp_weeks_ln_uAs  <- readRDS("gcomp_weeks_ln_uAs.RDS")

# Reset Working Directory
setwd("../arsenic_morbidity_gcomp/")

##### Tidy Estimates ###########################################################
df_tbl5 <- rbind(
  # Drinking Water Arsenic
  # (Cases)
  gcomp_summary(gcomp_cases_wAs50),
  gcomp_summary(gcomp_cases_wAs10),
  gcomp_summary(gcomp_cases_wAs1),
  gcomp_summary(gcomp_cases_ln_wAs),
  
  # (Weeks)
  gcomp_summary(gcomp_weeks_wAs50),
  gcomp_summary(gcomp_weeks_wAs10),
  gcomp_summary(gcomp_weeks_wAs1),
  gcomp_summary(gcomp_weeks_ln_wAs),
  
  # Urinary Arsenic
  # (Cases)
  gcomp_summary(gcomp_cases_uAs_p50),
  gcomp_summary(gcomp_cases_uAs_p25),
  gcomp_summary(gcomp_cases_uAs_p10),
  gcomp_summary(gcomp_cases_ln_uAs),
  
  # (Weeks)
  gcomp_summary(gcomp_weeks_uAs_p50),
  gcomp_summary(gcomp_weeks_uAs_p25),
  gcomp_summary(gcomp_weeks_uAs_p10),
  gcomp_summary(gcomp_weeks_ln_uAs)
)

df_tbl5 %>% head()

##### Generate Table ###########################################################
tbl5 <- df_tbl5 %>%
  pivot_wider(id_cols = x, names_from = y, values_from = c(pt,lb,ub,lt0)) %>%
  mutate(across(-x, ~ format(round(.x, 1), nsmall = 1))) %>%
  mutate(Cases = paste0(pt_ILI_CASES, " (", lb_ILI_CASES, ", ", ub_ILI_CASES, ")")) %>%
  mutate(Weeks = paste0(pt_ILI_WEEKS, " (", lb_ILI_WEEKS, ", ", ub_ILI_WEEKS, ")")) %>%
  select(A = x, Cases, lt0_Cases = lt0_ILI_CASES, Weeks, lt0_Weeks = lt0_ILI_WEEKS)

tbl5 %>% head()











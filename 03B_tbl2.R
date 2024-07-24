################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 2

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(table1)

##### Prepare Variables ########################################################
# Labels
label(df_selected$AGE)       <- "Age"
label(df_selected$SEGSTAGE)  <- "Gestational Age"
label(df_selected$PARITY)    <- "Parity"
label(df_selected$EDUCATION) <- "Education"
label(df_selected$LSI)       <- "Living Standards Index"
label(df_selected$SEBMI)     <- "Body Mass Index"
label(df_selected$medSEMUAC) <- "Mid-upper Arm Circumference"
label(df_selected$PETOBAC)   <- "Chewing Tobacco Use"
label(df_selected$PEBETEL)   <- "Betel Nut Use"
label(df_selected$PEHCIGAR)  <- "Husband Smokes at Home"

# Units
units(df_selected$AGE)       <- "years"
units(df_selected$SEGSTAGE)  <- "weeks"
units(df_selected$medSEMUAC) <- "cm"

# Check Data
df_selected %>% head()

##### Generate Tables ##########################################################
# Urinary Arsenic >10th Percentile
(tbl2a <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | uAs_p10_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Urinary Arsenic >25th Percentile
(tbl2b <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | uAs_p25_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Urinary Arsenic >50th Percentile
(tbl2c <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | uAs_p50_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Get Geometric Means for Arsenic Variables
df_selected %>%
  select(uAs_p10,uAs_p25,uAs_p50,uAs) %>%
  pivot_longer(-uAs) %>%
  group_by(name, value) %>%
  summarise(
    n = n(),
    GM = exp(mean(log(uAs))),
    GSD = exp(sd(log(uAs)))) %>%
  mutate(across(c(GM,GSD), ~ round(.x, 1)))


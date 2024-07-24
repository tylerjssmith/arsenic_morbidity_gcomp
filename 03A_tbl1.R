################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table 1

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
# Drinking Water Arsenic >1 µg/L
(tbl1a <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | wAs1_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Drinking Water Arsenic >10 µg/L
(tbl1b <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | wAs10_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Drinking Water Arsenic >50 µg/L
(tbl1c <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | wAs50_lab, 
  data = df_selected, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

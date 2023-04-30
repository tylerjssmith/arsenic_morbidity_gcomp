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
label(df$AGE)       <- "Age"
label(df$SEGSTAGE)  <- "Gestational Age"
label(df$PARITY)    <- "Parity"
label(df$EDUCATION) <- "Education"
label(df$LSI)       <- "Living Standards Index"
label(df$SEBMI)     <- "Body Mass Index"
label(df$medSEMUAC) <- "Mid-upper Arm Circumference"
label(df$PETOBAC)   <- "Chewing Tobacco Use"
label(df$PEBETEL)   <- "Betel Nut Use"
label(df$PEHCIGAR)  <- "Husband Smokes at Home"

# Units
units(df$AGE)       <- "years"
units(df$SEGSTAGE)  <- "weeks"
units(df$medSEMUAC) <- "cm"

# Check Data
df %>% head()

##### Generate Tables ##########################################################
# Drinking Water Arsenic >1 µg/L
(tbl1a <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs1_lab, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Drinking Water Arsenic >10 µg/L
(tbl1b <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs10_lab, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Drinking Water Arsenic >50 µg/L
(tbl1c <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs50_lab, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

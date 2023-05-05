################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Table S1

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
# Selection
(tblS1a <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | SELECTED, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Live Birth
(tblS1b <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | LIVEBIRTH, 
  data = df, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))

# Selection | Live Birth
(tblS1c <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wAs + ln_uAs | SELECTED, 
  data = df %>% filter(LIVEBIRTH == 1), overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval)))





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
# Parity
df_tbl1 <- df %>%
  mutate(PARITY = factor(PARITY,
    levels = c(0,1,2),
    labels = c("Nulliparous","Primiparous","Multiparous")
  ))

# Education
df_tbl1 <- df_tbl1 %>%
  mutate(EDUCATION = factor(EDUCATION,
    levels = c(0,1,2),
    labels = c("None","Class 1-9","Class ≥10")
  ))

# Chewing Tobacco
df_tbl1 <- df_tbl1 %>%
  mutate(PETOBAC = factor(PETOBAC,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

# Betel Nut
df_tbl1 <- df_tbl1 %>%
  mutate(PEBETEL = factor(PEBETEL,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

# Husband's Smoking
df_tbl1 <- df_tbl1 %>%
  mutate(PEHCIGAR = factor(PEHCIGAR,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

# Labels
label(df_tbl1$AGE)       <- "Age"
label(df_tbl1$SEGSTAGE)  <- "Gestational Age"
label(df_tbl1$PARITY)    <- "Parity"
label(df_tbl1$EDUCATION) <- "Education"
label(df_tbl1$LSI)       <- "Living Standards Index"
label(df_tbl1$medSEMUAC) <- "Mid-upper Arm Circumference"
label(df_tbl1$PETOBAC)   <- "Chewing Tobacco Use"
label(df_tbl1$PEBETEL)   <- "Betel Nut Use"
label(df_tbl1$PEHCIGAR)  <- "Husband Smokes at Home"

# Units
units(df_tbl1$AGE)       <- "years"
units(df_tbl1$SEGSTAGE)  <- "weeks"
units(df_tbl1$medSEMUAC) <- "cm"

# Check Data
df_tbl1 %>% head()

##### Generate Tables ##########################################################
# Drinking Water Arsenic >1 µg/L
table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs1, 
  data = df_tbl1, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval))

# Drinking Water Arsenic >10 µg/L
table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs10, 
  data = df_tbl1, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval))

# Drinking Water Arsenic >50 µg/L
table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR | wAs50, 
  data = df_tbl1, overall = FALSE, render.continuous = c(. = "Mean (SD)"), extra.col = list(`p` = tbl_pval))


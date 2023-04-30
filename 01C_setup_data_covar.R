################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Data

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(lubridate)

##### Read Data ################################################################
# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
kidtrak  <- read_csv("j7kidtrak/pair_kidtrak_2022_0310.csv")
water    <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
urine    <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")
pefsst   <- read_csv("pefsst/pair_pefsst_2022_0310.csv") 
pef      <- read_csv("pef/pair_pef_2022_0310.csv")
parity   <- read_csv("pair_reprohistory/pair_reprohistory_2022_0328.csv")
ses      <- read_csv("ses/pair_ses_2022_0310.csv")

# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_ili/code/arsenic_morbidity_gcomp/")

##### Select Data ##############################################################
# J7PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(
    UID,
    MOMDOB = DOBYY,
    BGLMPWK
  )

pregtrak %>% head()

# J7KIDTRAK
kidtrak <- kidtrak %>%
  select(
    UID = MOMUID,
    CHILDUID,
    CHILDDOB
  )

# Drinking Water Arsenic
water <- water %>%
  select(
    UID,
    wAs = PE_wMetals_As
  )

water <- water %>%
  mutate(UID = as.numeric(UID))

water %>% head()

# Urinary Arsenic
urine <- urine %>%
  select(
    UID,
    uAs = PE_uAs_Sum_SG,
    uAsB = PE_uAs_Ab_SG
  )

urine %>% head()

# PEFSST
pefsst <- pefsst %>%
  select(
    UID,
    SEDATE,
    SEWKINT,
    SEBMI,
    medSEMUAC
  )

# PEF
pef <- pef %>%
  select(
    UID,
    PETOBAC,
    PEBETEL,
    PEHCIGAR
  )

# Parity
parity <- parity %>%
  select(
    UID,
    PARITY = FDPSR_PARITY
  )

# SES
ses <- ses %>%
  select(
    UID,
    EDUCATION = wehclass_mc2,
    LSI = lsi
  )

##### Join Data ################################################################
df <- left_join(pregtrak, kidtrak, by = "UID")
df <- left_join(df, water, by = "UID")
df <- left_join(df, urine, by = "UID")
df <- left_join(df, pefsst, by = "UID")
df <- left_join(df, pef, by = "UID")
df <- left_join(df, parity, by = "UID")
df <- left_join(df, ses, by = "UID")

df %>% head()

# Remove Source Data
rm(list = c("pregtrak","kidtrak","water","urine","pefsst","pef","parity","ses"))

##### Set Sample ###############################################################
# Indicate Live Birth
df <- df %>%
  mutate(LIVEBIRTH = ifelse(!is.na(CHILDUID), 1, 0))

# Indicate Singleton Live Birth
df <- df %>%
  group_by(UID) %>%
  mutate(
    SINGLETON = 
      ifelse(LIVEBIRTH == 1 & n() == 1, 1,
      ifelse(LIVEBIRTH == 1 & n() != 1, 0,
      ifelse(LIVEBIRTH != 1, NA, NA)))) %>%
  ungroup()
    
df %>%
  group_by(LIVEBIRTH) %>%
  count(SINGLETON)

# Reduce to 1 Row/Pregnant Woman
df <- df %>%
  group_by(UID) %>%
  arrange(UID, CHILDDOB) %>%
  slice_head() %>%
  ungroup()

df %>% head()
df %>% nrow()

##### Prepare: Drinking Water Arsenic ##########################################
# Natural Log
df <- df %>%
  mutate(ln_wAs = log(wAs))

df %>% 
  check_continuous(
    x = ln_wAs, 
    title = "Drinking Water Arsenic", 
    xlab = "Log(Drinking Water Arsenic)"
  )

# Common Log
df <- df %>%
  mutate(l10_wAs = log10(wAs))

# Standards
df <- df %>%
  mutate(wAs1  = ifelse(wAs > 1,  1, 0)) %>%
  mutate(wAs10 = ifelse(wAs > 10, 1, 0)) %>%
  mutate(wAs50 = ifelse(wAs > 50, 1, 0))

df <- df %>%
  mutate(wAs1_lab = factor(wAs1,
    levels = c(0,1),
    labels = c("≤1 µg/L",">1 µg/L")
  ))

df <- df %>%
  mutate(wAs10_lab = factor(wAs10,
    levels = c(0,1),
    labels = c("≤10 µg/L",">10 µg/L")
  ))

df <- df %>%
  mutate(wAs50_lab = factor(wAs50,
    levels = c(0,1),
    labels = c("≤50 µg/L",">50 µg/L")
  ))

df %>% check_discrete(wAs1)
df %>% check_discrete(wAs10)
df %>% check_discrete(wAs50)

df %>% head()

##### Prepare: Urinary Arsenic #################################################
# Natural Log
df <- df %>%
  mutate(ln_uAs = log(uAs))

df %>% 
  check_continuous(
    x = ln_uAs, 
    title = "Urinary Arsenic", 
    xlab = "Log(Drinking Water Arsenic [∑uAs])"
  )

# Common Log
df <- df %>%
  mutate(l10_uAs = log10(uAs))

# Tertiles
df <- df %>%
  mutate(uAs3 = ntile(uAs, 3))

df <- df %>%
  mutate(uAs3 = factor(uAs3,
    levels = c(1,2,3),
    labels = c("Tertile 1","Tertile 2","Tertile 3")
  ))

df %>% head()

##### Prepare: Confounders #####################################################
# Age
df <- df %>%
  mutate(AGE = year(SEDATE) - MOMDOB)

df %>% 
  check_continuous(
    x = AGE, 
    title = "Age at Enrollment", 
    xlab = "Age (years)"
  )

# Gestational Age at Enrollment
df <- df %>%
  mutate(SEGSTAGE = SEWKINT - BGLMPWK)

df <- df %>%
  mutate(SEGSTAGE = ifelse(SEGSTAGE < 13, 13, SEGSTAGE)) %>%
  mutate(SEGSTAGE = ifelse(SEGSTAGE > 16, 16, SEGSTAGE))

df <- df %>%
  mutate(SEGSTAGE = factor(SEGSTAGE,
    levels = c(13:16),
    labels = c("11-13","14","15","16-17")
  ))

df %>% check_discrete(SEGSTAGE)

# Parity
df <- df %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

df <- df %>%
  mutate(PARITY = factor(PARITY,
    levels = c(0,1,2),
    labels = c("Nulliparous","Primiparous","Multiparous")
  ))

df %>% check_discrete(PARITY)

# Education
df <- df %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

df <- df %>%
  mutate(EDUCATION = factor(EDUCATION,
    levels = c(0,1,2),
    labels = c("None","Class 1-9","Class ≥10")
  ))

df %>% check_discrete(EDUCATION)

# Living Standards Index
df %>% 
  check_continuous(
    x = LSI, 
    title = "Living Standards Index", 
    xlab = "Living Standards Index"
  )

# Body Mass Index
df %>% 
  check_continuous(
    x = SEBMI, 
    title = "Body Mass Index at Enrollment", 
    xlab = expression("Body Mass Index (kg/m" ^ 2 * ")")
  )

# Mid-upper Arm Circumference
df %>% 
  check_continuous(
    x = medSEMUAC, 
    title = "Mid-upper Arm Circumference at Enrollment", 
    xlab = "Mid-upper Arm Circumference (cm)"
  )

# Chewing Tobacco Use
df <- df %>%
  mutate(PETOBAC = as.numeric(PETOBAC))

df <- df %>%
  mutate(PETOBAC = factor(PETOBAC,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

df %>% check_discrete(PETOBAC)

# Betel Nut Use
df <- df %>%
  mutate(PEBETEL = factor(PEBETEL,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

df %>% check_discrete(PEBETEL)

# Husband's Smoking
df <- df %>%
  mutate(PEHCIGAR = factor(PEHCIGAR,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

df %>% check_discrete(PEHCIGAR)

##### Select Variables #########################################################
df %>% colnames()

df <- df %>%
  select(
    # Identifiers and Pregnancy Outcomes
    UID,CHILDUID,CHILDDOB,LIVEBIRTH,SINGLETON,
    
    # Drinking Water Arsenic
    wAs,ln_wAs,l10_wAs,wAs1,wAs1_lab,wAs10,wAs10_lab,wAs50,wAs50_lab,
    
    # Urinary Arsenic
    uAs,ln_uAs,l10_uAs,uAs3,uAsB,
    
    # Confounders
    AGE,SEGSTAGE,PARITY,EDUCATION,LSI,SEBMI,medSEMUAC,PETOBAC,PEBETEL,PEHCIGAR
  )

df %>% head()





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

# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_ili/code/arsenic_morbidity_gcomp/")

##### Select Data ##############################################################
# J7PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

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

##### Join Data ################################################################
df <- left_join(pregtrak, kidtrak, by = "UID")
df <- left_join(df, water, by = "UID")
df <- left_join(df, urine, by = "UID")

df %>% head()

# Remove Source Data
rm(list = c("pregtrak","kidtrak","water","urine"))

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

# Standards
df <- df %>%
  mutate(wAs1  = ifelse(wAs > 1,  1, 0)) %>%
  mutate(wAs10 = ifelse(wAs > 10, 1, 0)) %>%
  mutate(wAs50 = ifelse(wAs > 50, 1, 0))

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

df %>% head()

##### Prepare: Confounders #####################################################


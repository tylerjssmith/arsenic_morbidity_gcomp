################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Data - Outcomes

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(haven)

##### Read Data ################################################################
# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

# Read Source Data
ili1 <- read_csv("weekly_calls/source_data/current_raw_data/J7_IEDCR_Weekly Morbidity_Infant_April-June 2019.csv")
ili2 <- read_csv("weekly_calls/source_data/current_raw_data/J7_IEDCR_Weekly Morbidity_Infant_July-Dec 2019.csv")
ili3 <- read_csv("weekly_calls/source_data/current_raw_data/J7_IEDCR_Weekly Morbidity_Infant_Jan 2020.csv")

# Stack Source Data
ili <- rbind(
  ili1,
  ili2,
  ili3
)

ili %>% head()

rm(list = c("ili1","ili2","ili3"))
  
# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_ili/code/arsenic_morbidity_gcomp/")

##### Select Data ##############################################################
ili <- ili %>%
  select(
    UID = Mother_UID,
    PSEUDOUID = UID,
    RELATIONSHIP = Relationship,
    VITALSTATUS = Status,
    ILI = Fever_status,
    CALLSTATUS = iSS,
    created_at
  )
    
ili %>% head()

ili %>% summarise(n = n_distinct(UID))

##### Limit to Singleton Live Births, 0-100 Days of Age ########################
# Get Pregnancy Outcomes
ili <- left_join(df %>% select(UID,LIVEBIRTH,SINGLETON,CHILDDOB), ili, by = "UID")

ili %>% head()

ili %>% filter(!is.na(CALLSTATUS)) %>% summarise(n = n_distinct(UID))

# Limit to Singleton Live Births
ili <- ili %>%
  filter(LIVEBIRTH == 1) %>%
  filter(SINGLETON == 1)

ili %>% head()

ili %>% filter(!is.na(CALLSTATUS)) %>% summarise(n = n_distinct(UID))

# Limit to 0-100 Days of Age
# (Format Date)
ili <- ili %>%
  mutate(DATE_TIME = as_datetime(mdy_hm(created_at))) %>%
  mutate(DATE = as_date(DATE_TIME)) %>%
  select(-created_at)

ili %>% head()

# (Filter by Date)
ili <- ili %>%
  filter(DATE >= CHILDDOB) %>%
  filter(DATE <= (CHILDDOB + 100))

ili %>% head()

ili %>% filter(!is.na(CALLSTATUS)) %>% summarise(n = n_distinct(UID))

##### Remove Duplicates ########################################################
tmp <- ili %>%
  filter(CALLSTATUS == 1) %>%
  mutate(WEEK = epiweek(DATE)) %>%
  group_by(UID, WEEK) %>%
  arrange(UID,WEEK,desc(DATE_TIME)) %>%
  filter(n() > 1) %>%
  filter(max(ILI) != min(ILI))

rm(tmp)

ili <- ili %>%
  filter(CALLSTATUS == 1) %>%
  mutate(WEEK = epiweek(DATE)) %>%
  group_by(UID, WEEK) %>%
  arrange(UID,WEEK,desc(DATE_TIME)) %>%
  slice_head() %>%
  ungroup()

ili %>% head()

ili %>% filter(!is.na(CALLSTATUS)) %>% summarise(n = n_distinct(UID))

##### Check Data ###############################################################
check_calls()

##### Calculate Events and Person-weeks by Participant #########################
ili_summary <- ili %>%
  group_by(UID) %>%
  summarise(
    CASES = sum(ILI),
    WEEKS = n()) %>%
  ungroup()


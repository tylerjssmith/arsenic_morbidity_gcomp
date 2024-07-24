################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Explore

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mgcv)

##### Drinking Water Arsenic and Iron ##########################################
# Correlations
df_complete %>% select(wFe, wAs) %>% cor(method = "spearman") %>% round(., 2)
df_selected %>% select(wFe, wAs) %>% cor(method = "spearman") %>% round(., 2)

# Values <LLOD
df %>% 
  filter(COMPLETE == 1) %>% 
  count(wAs < 0.02)

# Range
df_selected %>%
  summarise(
    n = n(),
    min = min(wAs),
    max = max(wAs),
    median = median(wAs),
    q1 = quantile(wAs, 0.25),
    q3 = quantile(wAs, 0.75)
  )

##### Urinary Arsenic ##########################################################
# Urinary Arsenobetaine
df_selected %>% 
  select(uAsB) %>%
  na.omit() %>%
  summarise(
    n = n(), 
    median = median(uAsB), 
    q1 = quantile(uAsB, 0.25), 
    q3 = quantile(uAsB, 0.75)
  )

# Range
df_selected %>%
  summarise(
    n = n(),
    min = min(uAs),
    max = max(uAs),
    median = median(uAs),
    q1 = quantile(uAs, 0.25),
    q3 = quantile(uAs, 0.75)
  )

##### Influenza-like Illness ###################################################
# Dates of Calls
ili %>%
  summarise(
    min = min(DATE),
    max = max(DATE)
  )

ili %>%
  ggplot(aes(x = DATE)) +
  geom_density() +
  scale_x_date(limits = as_date(c("2019-04-01","2020-01-01")), 
    date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0,0.008)) +
  labs(
    x = "Date",
    y = "Density") +
  th

# First Calls
ili %>%
  select(DATE) %>%
  arrange(DATE)

# Last Calls
ili %>%
  select(DATE) %>%
  arrange(desc(DATE))

##### Results ##################################################################
# Maternal Age
df_selected %>%
  summarise(
    n = n(),
    median = median(AGE),
    q1 = quantile(AGE, 0.25),
    q3 = quantile(AGE, 0.75)
  )

# Gestational Age
df_selected %>%
  summarise(
    n = n(),
    median = median(num_SEGSTAGE),
    q1 = quantile(num_SEGSTAGE, 0.25),
    q3 = quantile(num_SEGSTAGE, 0.75)
  )

# Parity
df_selected %>%
  count(PARITY) %>%
  mutate(p = n / sum(n) * 100)

##### Positivity ###############################################################
df_selected %>%
  group_by(SEGSTAGE,PARITY,EDUCATION,PETOBAC,PEBETEL,PEHCIGAR) %>%
  summarise(n = n())

df_selected %>%
  mutate(across(c(AGE,LSI,medSEMUAC), ~ ntile(.x, 4))) %>%
  group_by(AGE,SEGSTAGE,PARITY,EDUCATION,LSI,medSEMUAC,PETOBAC,PEBETEL,PEHCIGAR) %>%
  summarise(n = n())




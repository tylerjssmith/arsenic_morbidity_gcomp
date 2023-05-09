################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Selection

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mgcv)
library(car)
library(broom)

##### Join Data ################################################################
df <- left_join(df, ili_summary, by = "UID")

df %>% head()

##### Indicate Selection #######################################################
# Indicate Selection
df <- df %>%
  mutate(COMPLETE = ifelse(!is.na(wAs) & !is.na(wFe) & !is.na(uAs) & 
      !is.na(AGE) & !is.na(medSEMUAC) & !is.na(PETOBAC), 1, 0))

df %>% head()

df <- df %>%
  mutate(COMPLETE = factor(COMPLETE,
    levels = c(0,1),
    labels = c("Incomplete","Complete")
  ))

df %>% count(COMPLETE)

df <- df %>%
  mutate(SELECTED = ifelse(COMPLETE == "Complete" & LIVEBIRTH == 1 & 
      SINGLETON == 1 & !is.na(WEEKS), 1, 0))

df %>% head()

df <- df %>%
  mutate(SELECTED = factor(SELECTED,
    levels = c(0,1),
    labels = c("Not Selected","Selected")
  ))

df %>% count(SELECTED)

# Subset on Selection
df_complete <- df %>%
  filter(COMPLETE == "Complete")

df_selected <- df_complete %>%
  filter(SELECTED == "Selected")

##### Prepare Relative Variables ###############################################
# Urinary Arsenic Interventions
df_selected %>%
  summarise(
    n = n(),
    p10 = quantile(uAs, 0.10),
    p25 = quantile(uAs, 0.25),
    p50 = quantile(uAs, 0.50)
  )

df_selected <- df_selected %>%
  mutate(uAs_p10 = ifelse(uAs > quantile(uAs, 0.10), 1, 0)) %>%
  mutate(uAs_p25 = ifelse(uAs > quantile(uAs, 0.25), 1, 0)) %>%
  mutate(uAs_p50 = ifelse(uAs > quantile(uAs, 0.50), 1, 0))

df_selected <- df_selected %>%
  mutate(uAs_p10_lab = factor(uAs_p10, levels = 0:1, labels = c("≤12.5 µg/L",">12.5 µg/L"))) %>%
  mutate(uAs_p25_lab = factor(uAs_p25, levels = 0:1, labels = c("≤19.3 µg/L",">19.3 µg/L"))) %>%
  mutate(uAs_p50_lab = factor(uAs_p50, levels = 0:1, labels = c("≤32.3 µg/L",">32.3 µg/L")))

##### Assess Selection #########################################################
# Enrolled (n=784)
df %>% nrow()

# +Live Births
df %>% 
  count(LIVEBIRTH)

# +Singleton Live Births
df %>% 
  filter(LIVEBIRTH == 1) %>% 
  count(SINGLETON)

# +Complete Covariate Data
df %>% 
  select(-contains("ILI"), -WEEKS, -FIRSTCALL) %>%
  filter(SINGLETON == 1) %>%
  sapply(function(x) sum(is.na(x))) %>%
  as_tibble(rownames = "Var") %>%
  filter(value > 0)
  
df %>% 
  select(-contains("ILI"), -WEEKS, -FIRSTCALL) %>%
  filter(SINGLETON == 1) %>%
  na.omit()

# +Contributed ≥1 Person-week
df %>%
  filter(SELECTED == "Selected") %>%
  summarise(
    n = sum(!is.na(WEEKS)),
    weeks = sum(WEEKS, na.rm = TRUE)
  )

##### Selection ################################################################
df %>% count(SELECTED)

# Logistic Regression: Marginal Probability
models_mpoe <- glm(SELECTED ~ 1, 
  family = binomial, data = df_complete)

# Logistic Regression: Conditional Probabilities
models_cpoe <- df_complete %>%
  select(wAs1,wAs10,wAs50,ln_wAs,ln_uAs) %>%
  map(~ glm(SELECTED ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
    family = binomial, data = df_complete))

models_cpoe %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  filter(term == ".x")

# Calculate Stabilized Weights
df_complete$Wt_wAs1   <- fitted.values(models_mpoe) / 
  fitted.values(models_cpoe$wAs1)
df_complete$Wt_wAs10  <- fitted.values(models_mpoe) / 
  fitted.values(models_cpoe$wAs10)
df_complete$Wt_wAs50  <- fitted.values(models_mpoe) / 
  fitted.values(models_cpoe$wAs50)
df_complete$Wt_ln_wAs <- fitted.values(models_mpoe) / 
  fitted.values(models_cpoe$ln_wAs)
df_complete$Wt_ln_uAs <- fitted.values(models_mpoe) / 
  fitted.values(models_cpoe$ln_uAs)

# Check Weights
# (Calculate Sums)
df_complete %>%
  group_by(SELECTED) %>%
  summarise(
    n = n(),
    Wt_wAs1   = sum(Wt_wAs1),
    Wt_wAs10  = sum(Wt_wAs10),
    Wt_wAs50  = sum(Wt_wAs50),
    Wt_ln_wAs = sum(Wt_ln_wAs),
    Wt_ln_uAs = sum(Wt_ln_uAs)
  )

# (Plot: Boxplots)
df_complete %>%
  select(SELECTED,starts_with("Wt_")) %>%
  pivot_longer(-SELECTED) %>%
  ggplot(aes(x = factor(SELECTED), y = value)) +
  geom_boxplot() +
  facet_wrap(. ~ name) +
  labs(
    x = "Selected",
    y = "1 / P(Selection | Exposure, Confounders)") +
  th

# (Plot: Weights by Arsenic)
df_complete %>%
  select(ln_wAs, starts_with("Wt_") & contains("wAs")) %>%
  pivot_longer(-ln_wAs) %>%
  ggplot(aes(x = ln_wAs, y = value)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  facet_wrap(. ~ name) +
  labs(
    x = "ln wAs",
    y = "P(Selection) / P(Selection | ln wAs, Confounders)") +
  th

df_complete %>%
  ggplot(aes(x = ln_uAs, y = Wt_ln_uAs)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  labs(
    x = "ln ∑uAs",
    y = "P(Selection) / P(Selection | ln ∑uAs, Confounders)") +
  th

# If Not Selected, Set Weights Equal to 0
df_complete <- df_complete %>%
  mutate(across(starts_with("Wt_"), ~ 
      ifelse(SELECTED == "Not Selected", 0, .x)))

##### Live Birth ###############################################################
df %>% count(LIVEBIRTH)

df_complete %>%
  select(wAs1,wAs10,wAs50,ln_wAs,ln_uAs) %>%
  map(~ glm(LIVEBIRTH ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
    family = binomial, data = df_complete)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  filter(term == ".x")

##### Selection | Live Birth ###################################################
# All Live Births
df_complete %>%
  filter(LIVEBIRTH == 1) %>%
  select(wAs1,wAs10,wAs50,ln_wAs,ln_uAs) %>%
  map(~ glm(SELECTED ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
    family = binomial, data = subset(df_complete, LIVEBIRTH == 1))) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  filter(term == ".x")

# Singleton Live Births
df_complete %>%
  filter(SINGLETON == 1) %>%
  select(wAs1,wAs10,wAs50,ln_wAs,ln_uAs) %>%
  map(~ glm(SELECTED ~ .x + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
      medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
    family = binomial, data = subset(df_complete, SINGLETON == 1))) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "A") %>%
  filter(term == ".x")


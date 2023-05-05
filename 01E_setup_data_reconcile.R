################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Reconcile

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

##### Assess Selection #########################################################
# Enrolled (n=784)
df %>% nrow()

# +Complete Covariate Data
df %>% count(COMPLETE)

# +Live Births
df %>% 
  filter(COMPLETE == "Complete") %>%
  count(LIVEBIRTH)

# +Singleton Live Births
df %>% 
  filter(COMPLETE == "Complete") %>%
  filter(LIVEBIRTH == 1) %>% 
  count(SINGLETON)

# +Contributed ≥1 Person-week
df %>%
  filter(SELECTED == "Selected") %>%
  summarise(
    n = sum(!is.na(WEEKS)),
    weeks = sum(WEEKS, na.rm = TRUE)
  )

##### Selection ################################################################
df %>% count(SELECTED)

# Logistic Regression: Conditional Probabilities
# (Interventions)
summary(mod_sel_wAs1    <- glm(SELECTED ~ wAs1 + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))
summary(mod_sel_wAs10   <- glm(SELECTED ~ wAs10 + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))
summary(mod_sel_wAs50   <- glm(SELECTED ~ wAs50 + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))
summary(mod_sel_uAs_p10 <- glm(SELECTED ~ uAs_p10 + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))

# (Continuous)
summary(mod_sel_wAs <- glm(SELECTED ~ ln_wAs + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))
summary(mod_sel_uAs <- glm(SELECTED ~ ln_uAs + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = df_complete))

# Logistic Regression: Marginal Probability
summary(mod_sel_mar <- glm(SELECTED ~ 1,
  family = binomial, data = df_complete))

# Calculate Stabilized Weights
df_complete$Wt_ln_wAs <- fitted.values(mod_sel_mar) / fitted.values(mod_sel_wAs)
df_complete$Wt_ln_uAs <- fitted.values(mod_sel_mar) / fitted.values(mod_sel_uAs)

# (Plot: Boxplots)
df_complete %>%
  select(SELECTED,Wt_ln_wAs,Wt_ln_uAs) %>%
  pivot_longer(-SELECTED) %>%
  ggplot(aes(x = factor(SELECTED), y = value)) +
  geom_boxplot() +
  facet_wrap(. ~ name) +
  labs(
    x = "Selected",
    y = "1 / P(Selection | Exposure, Confounders)") +
  th

# (Plot: Weights by ln ∑uAs)
df_complete %>%
  ggplot(aes(x = ln_uAs, y = Wt_ln_uAs)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(
    x = "ln ∑uAs",
    y = "P(Selection) / P(Selection | ln ∑uAs, Confounders)") +
  th

##### Live Birth ###############################################################
df %>% count(LIVEBIRTH)

summary(glm(LIVEBIRTH ~ ln_wAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, COMPLETE == "Complete")))

summary(glm(LIVEBIRTH ~ ln_uAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, COMPLETE == "Complete")))

##### Selection | Live Birth ###################################################
# All Live Births
summary(glm(SELECTED ~ ln_wAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, LIVEBIRTH == 1)))

summary(glm(SELECTED ~ ln_uAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, LIVEBIRTH == 1)))

# Singleton Live Births
summary(glm(SELECTED ~ ln_wAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, SINGLETON == 1)))

summary(glm(SELECTED ~ ln_uAs + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + 
    medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + ln_wFe, 
  family = binomial, data = subset(df, SINGLETON == 1)))

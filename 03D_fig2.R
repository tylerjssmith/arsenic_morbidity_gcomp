################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 2

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Prepare Data
df_fig2 <- df %>%
  select(wAs1,wAs10,wAs50,l10_uAs) %>%
  pivot_longer(
    cols = -l10_uAs,
    names_to = "Standard",
    values_to = "Status")

df_fig2 <- df_fig2 %>%
  mutate(Standard = factor(Standard, levels = c("wAs1","wAs10","wAs50"),
    labels = c("1 µg/L","10 µg/L","50 µg/L")))

df_fig2 <- df_fig2 %>%
  mutate(Status = factor(Status, levels = c(0,1), 
    labels = c("Complies","Exceeds")))

df_fig2 %>% head()

# Generate Figure
(fig2 <- df_fig2 %>%
  na.omit() %>%
  ggplot(aes(x = l10_uAs, fill = Status)) +
  geom_density(alpha = 0.4) +
  facet_wrap(. ~ Standard) +
  scale_x_continuous(limits = c(0,3.1), breaks = seq(-10,10,1), 
    labels = base10) +
  scale_y_continuous(limits = c(0,2)) +
  labs(
    x = "Urinary Arsenic (∑uAs) (µg/L)",
    y = "Density") +
  th)

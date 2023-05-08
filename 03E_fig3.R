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
df_fig3 <- df_selected %>%
  select(wAs1,wAs10,wAs50,l10_uAs) %>%
  pivot_longer(
    cols = -l10_uAs,
    names_to = "Standard",
    values_to = "Status")

# (Label Standard)
df_fig3 <- df_fig3 %>%
  mutate(Standard = factor(Standard, levels = c("wAs1","wAs10","wAs50"),
    labels = c("1 µg/L","10 µg/L","50 µg/L")))

# (Label Status)
df_fig3 <- df_fig3 %>%
  mutate(Status = factor(Status, levels = c(0:1),
    labels = c("Complies","Exceeds")))

df_fig3 %>% head()

# Generate Figure
(fig3 <- df_fig3 %>%
  na.omit() %>%
  ggplot(aes(x = l10_uAs, fill = Status)) +
  geom_density(alpha = 0.4) +
  facet_wrap(. ~ Standard) +
  scale_x_continuous(limits = c(0,3.1), breaks = seq(-10,10,1), 
    labels = base10) +
  scale_y_continuous(limits = c(0,2.1)) +
  labs(
    x = "Urinary Arsenic (∑uAs) (µg/L)",
    y = "Density") +
  th)

# Remove Objects
rm(df_fig3)

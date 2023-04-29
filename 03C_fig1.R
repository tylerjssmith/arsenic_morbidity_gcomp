################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 1
# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Prepare Labels
fig1_labels <- tibble(
  # x-axis Coordinate
  x = c(log(1),log(10),log(50)),
  
  # y-axis Coordinate
  y = rep(6.2, 3),
  
  # Label
  label = c("1 µg/L","10 µg/L","50 µg/L")
)

fig1_labels %>% head()

# Generate Figure
(fig1 <- df %>%
  na.omit() %>%
  ggplot(aes(x = ln_wAs, y = ln_uAs)) +
  geom_vline(data = fig1_labels, aes(xintercept = x), linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  geom_label(data = fig1_labels, aes(x = x, y = y, label = label),
    inherit.aes = FALSE, label.size = NA) +
  labs(
    x = "Drinking Water Arsenic (ln[µg/L])",
    y = "Urinary Arsenic (∑uAs) (ln[µg/L])") +
  th)

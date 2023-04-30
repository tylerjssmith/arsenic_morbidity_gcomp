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
labels_fig1 <- tibble(
  # x-axis Coordinate
  x = c(log10(1),log10(10),log10(50)),
  
  # y-axis Coordinate
  y = rep(2.8, 3),
  
  # Label
  label = c("1 µg/L","10 µg/L","50 µg/L")
)

labels_fig1 %>% head()

# Generate Figure
(fig1 <- df %>%
  na.omit() %>%
  ggplot(aes(x = l10_wAs, y = l10_uAs)) +
  geom_vline(data = labels_fig1, aes(xintercept = x), linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  geom_label(data = labels_fig1, aes(x = x, y = y, label = label),
    inherit.aes = FALSE, label.size = NA) +
  scale_x_continuous(limits = c(-2,3.1), breaks = seq(-10,10,1), 
    labels = base10) +
  scale_y_continuous(limits = c( 0,3.1), breaks = seq(-10,10,1), 
    labels = base10) +
  labs(
    x = "Drinking Water Arsenic (µg/L)",
    y = "Urinary Arsenic (∑uAs) (µg/L)") +
  th)

# Remove Objects
rm(labels_fig1)



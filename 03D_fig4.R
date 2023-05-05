################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 4

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
# Combine Unadjusted and Adjusted Estimates
df_fig4 <- rbind(
  # Unadjusted
  poisson_cru_est %>%
    filter(term == ".x") %>%
    mutate(Set = "Unadjusted") %>%
    select(Set, A, estimate, conf.low, conf.high),

  # Adjusted
  poisson_adj_est %>%
    filter(term == ".x") %>%
    mutate(Set = "Adjusted") %>%
    select(Set, A, estimate, conf.low, conf.high)
)

df_fig4 %>% head()

# Label Adjustment Sets
df_fig4 <- df_fig4 %>%
  mutate(Set = factor(Set,
    levels = c("Unadjusted","Adjusted")
  ))

# Label Arsenic Variable
df_fig4 <- df_fig4 %>%
  mutate(A = factor(A,
    levels = c("ln_wAs","wAs1","wAs10","wAs50","ln_uAs","uAs_p10"),
    labels = c("ln wAs","wAs >1 µg/L","wAs >10 µg/L","wAs >50 µg/L",
      "ln ∑uAs","∑uAs >10th Percentile")
  ))

df_fig4 %>% head()

##### Generate Figure ##########################################################
(fig4 <- df_fig4 %>%
  ggplot(aes(x = Set, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  geom_point() +
  #scale_y_continuous(limits = c(-0.4,0.8), breaks = seq(-0.4,0.8,0.2)) +
  facet_wrap(. ~ A, ncol = 4) +
  labs(
    x = NULL,
    y = "Log Odds Ratio
    (95% Confidence Interval)") +
  th)


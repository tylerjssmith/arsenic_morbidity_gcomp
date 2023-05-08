################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 5

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
df_fig5 <- rbind(
  df_fig5_weeks %>%
    mutate(outcome = "Weeks"),
  df_fig5_cases %>%
    mutate(outcome = "Cases")
)

df_fig5 <- df_fig5 %>%
  mutate(type = ifelse(grepl("wAs", A), "Water", "Urine")) %>%
  mutate(type = factor(type, 
    levels = c("Water","Urine"),
    labels = c("wAs","∑uAs")
  ))

# Label Intervention
df_fig5 <- df_fig5 %>%
  mutate(A = factor(A,
    levels = c("wAs1","wAs10","wAs50","uAs_p10","uAs_p25","uAs_p50"),
    labels = c("Bangladesh\n1 µg/L","WHO\n10 µg/L","The Netherlands\n50 µg/L",
      "10th Percentile\n12.5 µg/L","25th Percentile\n19.3 µg/L",
      "50th Percentile\n32.3 µg/L")
  ))

##### Generate Figure ##########################################################
(fig5 <- df_fig5 %>%
  mutate(across(-c(A,outcome,type), ~ .x * 1000)) %>%
  ggplot(aes(x = A, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  geom_point() +
  scale_y_continuous(limits = c(-60,20), breaks = seq(-80,20,20)) + 
  facet_grid(outcome ~ type, scales = "free_x") +
  labs(
    x = "Intervention", 
    y = "Marginal Incidence Rate Difference
    (Cases/1,000 Person-weeks)") +
  th)

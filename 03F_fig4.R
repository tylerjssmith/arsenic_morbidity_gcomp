################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 4

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Combine Estimates ########################################################
# Combine Estimates
df_fig4 <- rbind(
  # ILI Weeks: Unadjusted
  poisson_cru_est_weeks %>%
    filter(term == ".x") %>%
    mutate(outcome = "Weeks"),

  # ILI Weeks: Adjusted
  poisson_adj_est_weeks %>%
    filter(term == ".x") %>%
    mutate(outcome = "Weeks"),
  
  # ILI Cases: Unadjusted
  poisson_cru_est_cases %>%
    filter(term == ".x") %>%
    mutate(outcome = "Cases"),

  # ILI Cases: Adjusted
  poisson_adj_est_cases %>%
    filter(term == ".x") %>%
    mutate(outcome = "Cases")
)

##### Label Variables ##########################################################
# Label Exposure
df_fig4 <- df_fig4 %>%
  mutate(type = ifelse(grepl("wAs", A), "Water", "Urine")) %>%
  mutate(type = factor(type, 
    levels = c("Water","Urine"),
    labels = c("wAs","∑uAs")
  ))

df_fig4 %>% head()

# Label Adjustment Sets
df_fig4 <- df_fig4 %>%
  mutate(set = factor(set, levels = c("Unadjusted","Adjusted")))

df_fig4 %>% head()

# Label Intervention
df_fig4 <- df_fig4 %>%
  mutate(A = factor(A,
    levels = c("wAs1","wAs10","wAs50","uAs_p10","uAs_p25","uAs_p50",
      "ln_wAs","ln_uAs"),
    labels = c("Bangladesh\nI(1 µg/L)","WHO\nI(10 µg/L)","The Netherlands\nI(50 µg/L)",
      "10th Percentile\nI(12.5 µg/L)","25th Percentile\nI(19.3 µg/L)",
      "50th Percentile\nI(32.3 µg/L)","ln(wAs)","ln(∑uAs)")
  ))

df_fig4 %>% head()

##### Select and Arrange Columns ###############################################
df_fig4 <- df_fig4 %>%
  select(outcome, type, A, set, estimate, conf.low, conf.high, p.value)

df_fig4 %>% head()

##### Generate Figure ##########################################################
(fig4 <- df_fig4 %>%
  ggplot(aes(x = A, y = estimate, ymin = conf.low, ymax = conf.high, 
    color = set)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_y_continuous(limits = c(-0.4,0.8), breaks = seq(-0.4,0.8,0.2)) +
  facet_grid(outcome ~ type, scales = "free_x") +
  labs(
    x = "Intervention/Exposure",
    y = "ln(Conditional Incident Rate Ratio)",
    color = "Model") +
  th + 
  theme(legend.position = "bottom"))
  

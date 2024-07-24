################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Figure 5

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figures: wAs #############################################################
# Model Objects
wAs_obj_cases = c("gcomp_cases_ln_wAs","gcomp_cases_wAs1","gcomp_cases_wAs10","gcomp_cases_wAs50")
wAs_obj_weeks = c("gcomp_weeks_ln_wAs","gcomp_weeks_wAs1","gcomp_weeks_wAs10","gcomp_weeks_wAs50")

# Labels
wAs_lab = c("Set wAs to 0","Shift wAs to\n≤1 µg/L",
  "Shift wAs to\n≤10 µg/L","Shift wAs to\n≤50 µg/L")

# Generate Figures
(figS1 <- gcomp_plot(wAs_obj_cases, wAs_lab, x_incr = 20))
(figS2 <- gcomp_plot(wAs_obj_weeks, wAs_lab, x_incr = 20))

##### Figures: ∑uAs ############################################################
# Model Objects
uAs_obj_cases = c("gcomp_cases_ln_uAs","gcomp_cases_uAs_p10","gcomp_cases_uAs_p25","gcomp_cases_uAs_p50")
uAs_obj_weeks = c("gcomp_weeks_ln_uAs","gcomp_weeks_uAs_p10","gcomp_weeks_uAs_p25","gcomp_weeks_uAs_p50")

# Labels
uAs_lab = c("Set ∑uAs to 0","Shift ∑uAs to \n≤10th Percentile",
  "Shift ∑uAs to \n≤25th Percentile","Shift ∑uAs to \n≤50th Percentile")

# Generate Figures
(fig4 <- gcomp_plot(uAs_obj_cases, uAs_lab, x_incr = 40))
(figS3 <- gcomp_plot(uAs_obj_weeks, uAs_lab, x_incr = 40))


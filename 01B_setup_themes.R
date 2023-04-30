################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Themes

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Define Plot Themes #######################################################
th <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

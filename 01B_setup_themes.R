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
    panel.grid.minor = element_blank()
  )

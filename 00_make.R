################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Make

# Tyler Smith
# April 28, 2023

##### 01 Setup #################################################################
# Functions
source("01A_setup_functions.R")

# Themes
source("01B_setup_themes.R")

# Data - Covariates
source("01C_setup_data_covar.R")

# Data - Outcomes
source("01D_setup_data_outcomes.R")

# Data - Reconcile
source("01E_setup_data_reconcile.R")

##### 02 Analysis ##############################################################
# Exploratory Data Analysis
source("02A_explore.R")

# Regression Models
source("02B_regression.R")

# G-Computation
source("02C_gcomp.R")

##### 03 Tables and Figures ####################################################
# Table 1
source("03A_table1.R")

# Figure 1
# (Note: This figure is the selection flowchart, which was made with draw.io.)

# Figure 2
source("03B_fig2.R")

# Figure 3
source("03C_fig3.R")

# Figure 4
source("03D_fig4.R")

# Figure 5
source("03E_fig5.R")

##### 04 Supplemental Tables and Figures #######################################
# Table S1
source("04A_tblS1.R")



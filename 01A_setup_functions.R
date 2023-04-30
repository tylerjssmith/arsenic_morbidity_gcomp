################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Prenatal Arsenic and Acute Morbidity -- Functions

# Tyler Smith
# April 28, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Functions: Check Covariates ##############################################
# Function: Check Continuous Covariate
check_continuous <- function(data, x, title = NULL, xlab = NULL)
{
  data %>%
    ggplot(aes(x = {{ x }})) +
    geom_density() +
    labs(
      title = title,
      x = xlab,
      y = "Density") +
    th
}

# Function: Check Discrete Covariate
check_discrete <- function(data, x)
{
  data %>%
    count({{ x }}) %>%
    mutate(p = n / sum(n) * 100)
}

##### Functions: Tables ########################################################
# Function: p-values for Tables 1-2
tbl_pval <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
    
  if (is.numeric(y)) {
    # Continuous: One-way ANOVA
    p <- anova(lm(y ~ g))[1,5]
  } else {
    # Categorical: Chi-square Test
    p <- chisq.test(table(y, g))$p.value
  }
    
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

##### Functions: Figures #########################################################
# Function: Base 10
base10 <- function(x) 10 ^ x
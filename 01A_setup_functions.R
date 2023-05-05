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

##### Functions: Check Outcomes ################################################
# Function: Check Calls by Age for Random Subset (n=25)
check_calls <- function(data = ili)
{
  # Sample UID
  tmp1 <- data %>% group_by(UID) %>% slice_head() %>% pull(UID)
  tmp2 <- floor(runif(1, 25, length(tmp1) - 25))
  tmp3 <- tmp1[(tmp2-24):tmp2]

  # Plot Calls by Child Age
  data %>%
    filter(UID %in% tmp3) %>%
    mutate(AGE = DATE - CHILDDOB) %>%
    ggplot(aes(x = AGE, y = factor(UID), group = factor(UID))) +
    geom_line(color = "gray") +
    geom_point() +
    scale_x_continuous(breaks = seq(0,100,10)) +
    labs(
      x = "Age (days)",
      y = "UID") +
    th + 
    theme(
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()
    )
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
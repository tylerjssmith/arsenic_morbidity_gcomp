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
check_calls <- function(data = ili, n = 25)
{
  # Sample UID
  tmp1 <- data %>% group_by(UID) %>% slice_head() %>% pull(UID)
  tmp2 <- sample(tmp1, n, replace = FALSE)

  # Plot Calls by Child Age
  data %>%
    filter(UID %in% tmp2) %>%
    mutate(AGE = DATE - CHILDDOB) %>%
    ggplot(aes(x = AGE, y = factor(UID), group = factor(UID), 
      color = factor(ILI_CAT))) +
    geom_line(color = "gray") +
    geom_point() +
    scale_x_continuous(breaks = seq(0,100,10)) +
    scale_color_manual(values = c("gray","#F8766D","#00BFC4")) +
    labs(
      x = "Age (days)",
      y = paste0("Randomly Selected Participants (n=", n, ")"),
      color = "ILI") +
    th + 
    theme(
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line()
    )
}

##### Functions: G-Computation #################################################
# Function: G-Computation Point Estimate
gcomp_pt <- function(data, model, A) {
  
  # Generate Data Sets
  data_Y1 <- data %>% rename(.x = {{ A }}) %>% mutate(.x = 0)
  data_Y0 <- data %>% rename(.x = {{ A }})
  
  # Get Predictions
  Y1 <- predict(model, data_Y1, type = "response") / data_Y1$WEEKS
  Y0 <- predict(model, data_Y0, type = "response") / data_Y0$WEEKS
  
  # Take Mean Difference
  out <- mean(Y1 - Y0)
  return(out)
  
}

# Function: G-Computation: Bootstrap
gcomp_bs <- function(data, model, A, R = 1000) {

  out <- numeric()
  
  for(i in 1:R) {
  
    boot_uid <- sample(1:nrow(data), nrow(data), replace = TRUE)
    boot_dat <- data[boot_uid, ]
  
    boot_dat <- boot_dat %>% rename(.x = A)
  
    new_model <- update(model, data = boot_dat)
  
    out[i] <- boot_dat %>%
      gcomp_pt(model = new_model, A = ".x")
  }
  
  return(out)
}

# Function: G-Computation
gcomp <- function(data, model, A) {
  
  estimate <- data %>% gcomp_pt(model, A)
  bs <- data %>% gcomp_bs(model, A)
  
  conf.low <- quantile(bs, 0.025)
  conf.high <- quantile(bs, 0.975)
  
  out <- tibble(A, estimate, conf.low, conf.high)
  
  return(out)

}

##### Functions: Tables ########################################################
# Function: p-values for Table 1
tbl_pval <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
    
  if (is.numeric(y)) {
    # Continuous: Student's t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # Categorical: Chi-square Test
    p <- chisq.test(table(y, g))$p.value
  }
    
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# Function: Table 2
tbl_incidence <- function(data = df_selected, x, xlab)
{
  data %>%
    group_by({{ x }}) %>%
    summarise(
      n = n(),
      person_weeks = sum(WEEKS),
      ili_cases = sum(ILI_CASES),
      ili_cases_rate = ili_cases / person_weeks * 1000,
      ili_weeks = sum(ILI_WEEKS),
      ili_weeks_rate = ili_weeks / person_weeks * 1000) %>%
    mutate(xlab = xlab) %>%
    select(xlab, x = {{ x }}, everything())
}

##### Functions: Figures #########################################################
# Function: Base 10
base10 <- function(x) 10 ^ x

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
# Function: Check Calls by Age for Random Subset
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
    geom_point(size = 3) +
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
# Function: Implement G-Computation
gcomp <- function(data, y, x, family = "quasipoisson", n_mc = 10000, 
  R = 1000, seed = 7023, verbose = FALSE) 
{
  ##### Point Estimate #########################################################
  # (1) Fit Model
  frm <- paste(y, "~", x, "+ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PETOBAC + PEBETEL + PEHCIGAR + offset(log(WEEKS))")
  
  model <- glm(frm, data = data, family = family)
  
  model_b <- coef(model)[2]
  
  # (2) Predict Outcomes
  set.seed(seed)
  
  dt_id <- sample(1:nrow(data), n_mc, replace = TRUE)
  dt_mc <- data[dt_id, ]
  
  dt_mc_Y1 <- dt_mc %>% mutate("{x}" := 0)
  dt_mc_Y0 <- dt_mc
  
  Y1 <- predict(model, dt_mc_Y1, type = "response") / 
    dt_mc_Y1$WEEKS
  Y0 <- predict(model, dt_mc_Y0, type = "response") / 
    dt_mc_Y0$WEEKS
  
  # (3) Take Mean Difference
  mean_Y1 <- mean(Y1)
  mean_Y0 <- mean(Y0)

  ##### Bootstrap ##############################################################
  boot_model_b <- numeric()
  boot_mean_Y1 <- numeric()
  boot_mean_Y0 <- numeric()
  
  set.seed(seed)
  
  for(i in 1:R) {
    
    # Sample with Replacement
    boot_id <- sample(1:nrow(data), nrow(data), replace = TRUE)
    boot_dt <- data[boot_id, ]
    
    # (1) Re-fit Model
    new_model <- update(model, data = boot_dt)
    
    boot_model_b <- c(boot_model_b, coef(new_model)[2])
    
    # (2) Predict Outcomes
    boot_dt_id <- sample(1:nrow(boot_dt), n_mc, replace = TRUE)
    boot_dt_mc <- boot_dt[boot_dt_id, ]
    
    boot_dt_mc_Y1 <- boot_dt_mc %>% mutate("{x}" := 0)
    boot_dt_mc_Y0 <- boot_dt_mc
  
    boot_Y1 <- predict(new_model, boot_dt_mc_Y1, type = "response") / 
      boot_dt_mc_Y1$WEEKS
    boot_Y0 <- predict(new_model, boot_dt_mc_Y0, type = "response") / 
      boot_dt_mc_Y0$WEEKS
  
    # (3) Take Mean Difference
    boot_mean_Y1[i] <- mean(boot_Y1)
    boot_mean_Y0[i] <- mean(boot_Y0)
  
    if (R >= 1000) {
      if(verbose & (i %in% seq(1000,R,1000))) {
        message(paste("gcomp:", y, "~", x, ":", i, "/", R))
      }
    }
    
  }
  
  ##### Combine Results ########################################################
  out <- list(
    y = y,
    x = x,
    model = model,
    model_b = model_b,
    mean_Y1 = mean_Y1,
    mean_Y0 = mean_Y0,
    boot_model_b = boot_model_b,
    boot_mean_Y1 = boot_mean_Y1,
    boot_mean_Y0 = boot_mean_Y0,
    n_obs = nobs(model),
    n_mc = n_mc,
    R = R,
    verbose = verbose
  )
  
  return(out)
  
}

# Function: Summarize gcomp Objects
gcomp_summary <- function(obj) {
  
  # Point Estimate
  mean_Y1 <- obj$mean_Y1
  mean_Y0 <- obj$mean_Y0
 
  mean_diff <- mean(mean_Y1 - mean_Y0)
   
  # Confidence Interval
  boot_mean_Y1 <- obj$boot_mean_Y1
  boot_mean_Y0 <- obj$boot_mean_Y0
  
  boot_diff <- boot_mean_Y1 - boot_mean_Y0
  
  lb <- quantile(boot_diff, 0.025)
  ub <- quantile(boot_diff, 0.975)
  
  lt0 <- sum(boot_diff < 0) / length(boot_diff)
  
  out <- tibble(
    y = obj$y,
    x = obj$x,
    pt = round(mean_diff * 1000, 1),
    lb = round(lb * 1000, 1),
    ub = round(ub * 1000, 1),
    lt0 = round(lt0 * 100, 1)
  )

  return(out)
  
}

# Function: Plot gcomp Objects
gcomp_plot <- function(objects, labels, title = NULL, x_incr = NULL) {
 
  require(patchwork)
  
  df_bs <- tibble()
  df_pt <- tibble()
  
  for(i in 1:length(objects)) {
   
    # Bootstrap Estimates
    df_bs_i <- tibble(
      A = objects[i],
      Y1 = get(objects[i])$boot_mean_Y1, 
      Y0 = get(objects[i])$boot_mean_Y0
    )
  
    df_bs_i$diff <- with(df_bs_i, Y1 - Y0)
    df_bs <- rbind(df_bs, df_bs_i)
    
    # Point Estimates
    df_pt_i <- tibble(
      A = objects[i],
      est = mean(df_bs_i$diff) * 1000,
      lb = quantile(df_bs_i$diff, 0.025) * 1000,
      ub = quantile(df_bs_i$diff, 0.975) * 1000
    )

    df_pt <- rbind(df_pt, df_pt_i)
    
  }
  
  df_bs$A <- factor(df_bs$A,
    levels = objects,
    labels = labels
  )
  
  tmp1 <- df_bs %>%
    mutate(across(c(Y1,Y0,diff), ~ .x * 1000)) %>%
    pivot_longer(c(Y1,Y0)) %>%
    mutate(name = ifelse(name == "Y1", "Intervention", "Natural Course")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(breaks = seq(-1000,1000,x_incr)) +
    facet_wrap(. ~ fct_rev(A), nrow = 1) +
    labs(
      x = "Mean Predicted Incidence Rate (Events/1,000 Person-weeks)",
      y = "Density",
      fill = "Scenario") +
    th + theme(legend.position = "right")

  tmp2 <- df_bs %>%
    mutate(across(c(Y1,Y0,diff), ~ .x * 1000)) %>%
    ggplot(aes(x = diff)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_density() +
    scale_x_continuous(breaks = seq(-1000,1000,x_incr)) +
    facet_wrap(. ~ fct_rev(A), nrow = 1) +
    labs(
      x = "Incidence Rate Difference (Events/1,000 Person-weeks)",
      y = "Density") +
    th + theme(strip.text = element_blank())
  
  tmp1 / tmp2 +
    plot_annotation(title = title, 
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      ))
  
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

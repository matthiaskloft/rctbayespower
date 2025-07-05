## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

# Demo settings
n_sims_demo <- 6 # Minimal simulations for fast demo builds
n_cores_demo <- 1 # Single core for vignette building

## ----load_libs, echo=FALSE----------------------------------------------------
library(rctbayespower)
library(ggplot2)
library(dplyr)

## ----cardio_setup-------------------------------------------------------------
# Convert to standardized effect size
baseline_sd <- 40
additional_reduction <- 15
effect_size <- additional_reduction / baseline_sd # Cohen's d = 0.375

# Clinically meaningful difference
clinical_threshold <- 10 / baseline_sd # 0.25

cat("Expected effect size (Cohen's d):", round(effect_size, 3), "\n")
cat("Clinical threshold:", round(clinical_threshold, 3), "\n")

## ----cardio_power-------------------------------------------------------------
# Initial power analysis
cardio_power <- power_analysis_ancova(
  n_control = 100,
  n_treatment = 100,
  effect_size = 0.375,
  baseline_effect = 0.5, # Strong baseline effect (LDL at screening)
  outcome_type = "continuous",
  threshold_success = 0.25, # Based on clinical threshold
  threshold_futility = 0.1,
  p_sig_success = 0.95,
  p_sig_futility = 0.5,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

print(cardio_power)

## ----cardio_sample_size-------------------------------------------------------
# Find optimal sample size for regulatory submission
cardio_sample_size <- power_grid_analysis(
  sample_sizes = c(80, 120, 160, 200),
  effect_sizes = 0.375, # Fixed effect size
  target_power_success = 0.85, # Higher power for regulatory submission
  target_power_futility = 0.8,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.5,
  threshold_success = 0.25,
  threshold_futility = 0.1,
  p_sig_success = 0.95,
  p_sig_futility = 0.5,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo
)

print(cardio_sample_size)

## ----cardio_plot--------------------------------------------------------------
# Plot sample size curve
plot(cardio_sample_size, type = "power_curve")


## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

# Reduced simulation counts for vignette building
n_sims_demo <- 8 # Minimal simulations for fast demo builds
n_cores_demo <- 1 # Single core for vignette building


## ----load_libs, echo=FALSE----------------------------------------------------
library(rctbayespower)
library(ggplot2)
library(dplyr)


## ----unequal_allocation-------------------------------------------------------
# 2:1 allocation ratio
power_unequal <- power_analysis_ancova(
  n_control = 50,
  n_treatment = 100, # 2:1 ratio
  effect_size = 0.4,
  baseline_effect = 0.3,
  outcome_type = "continuous",
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

# Compare with equal allocation (same total N = 150)
power_equal <- power_analysis_ancova(
  n_control = 75,
  n_treatment = 75,
  effect_size = 0.4,
  baseline_effect = 0.3,
  outcome_type = "continuous",
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

# Compare power
cat("Unequal allocation (2:1) power:", round(power_unequal$power_success, 3), "\n")
cat("Equal allocation power:", round(power_equal$power_success, 3), "\n")


## ----multiarm-----------------------------------------------------------------
# Create a comparison table for multiple arms
multiarm_comparison <- data.frame(
  Comparison = character(),
  Power_Success = numeric(),
  Power_Futility = numeric(),
  Mean_Effect = numeric(),
  stringsAsFactors = FALSE
)

# Treatment A vs Control
power_A_vs_C <- power_analysis_ancova(
  n_control = 60,
  n_treatment = 60,
  effect_size = 0.5, # Treatment A effect
  baseline_effect = 0.3,
  outcome_type = "continuous",
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

multiarm_comparison <- rbind(multiarm_comparison, data.frame(
  Comparison = "Treatment A vs Control",
  Power_Success = power_A_vs_C$power_success,
  Power_Futility = power_A_vs_C$power_futility,
  Mean_Effect = power_A_vs_C$mean_effect_estimate
))

# Treatment B vs Control
power_B_vs_C <- power_analysis_ancova(
  n_control = 60,
  n_treatment = 60,
  effect_size = 0.3, # Treatment B effect (smaller)
  baseline_effect = 0.3,
  outcome_type = "continuous",
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

multiarm_comparison <- rbind(multiarm_comparison, data.frame(
  Comparison = "Treatment B vs Control",
  Power_Success = power_B_vs_C$power_success,
  Power_Futility = power_B_vs_C$power_futility,
  Mean_Effect = power_B_vs_C$mean_effect_estimate
))

# Treatment A vs Treatment B (head-to-head)
power_A_vs_B <- power_analysis_ancova(
  n_control = 60,
  n_treatment = 60,
  effect_size = 0.2, # Difference between treatments
  baseline_effect = 0.3,
  outcome_type = "continuous",
  threshold_success = 0.15, # Smaller threshold for head-to-head
  threshold_futility = 0.05,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

multiarm_comparison <- rbind(multiarm_comparison, data.frame(
  Comparison = "Treatment A vs Treatment B",
  Power_Success = power_A_vs_B$power_success,
  Power_Futility = power_A_vs_B$power_futility,
  Mean_Effect = power_A_vs_B$mean_effect_estimate
))

print(multiarm_comparison)


## ----design_priors------------------------------------------------------------
# Compare different design prior specifications
design_prior_comparison <- data.frame(
  Prior_Type = character(),
  Integrated_Power = numeric(),
  Power_Success = numeric(),
  stringsAsFactors = FALSE
)

# Informative prior based on meta-analysis
effect_size_informed <- power_grid_analysis(
  sample_sizes = 80,
  effect_sizes = seq(0.2, 0.6, by = 0.1),
  design_prior = "normal(0.4, 0.1)", # Informed prior
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.3,
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo
)

design_prior_comparison <- rbind(design_prior_comparison, data.frame(
  Prior_Type = "Informed Normal(0.4, 0.1)",
  Integrated_Power = effect_size_informed$integrated_power_success,
  Power_Success = effect_size_informed$power_success[effect_size_informed$effect_size == 0.4]
))

# Skeptical prior
effect_size_skeptical <- power_grid_analysis(
  sample_sizes = 80,
  effect_sizes = seq(0.2, 0.6, by = 0.1),
  design_prior = "normal(0.25, 0.05)", # Skeptical prior
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.3,
  threshold_success = 0.3,
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo
)

design_prior_comparison <- rbind(design_prior_comparison, data.frame(
  Prior_Type = "Skeptical Normal(0.25, 0.05)",
  Integrated_Power = effect_size_skeptical$integrated_power_success,
  Power_Success = effect_size_skeptical$power_success[effect_size_skeptical$effect_size == 0.3]
))

print(design_prior_comparison)


## ----design_priors_plot-------------------------------------------------------
# Visualize the effect of different priors
plot(effect_size_informed, type = "power_curve", show_integrated = TRUE)


## ----custom_priors------------------------------------------------------------
# Define custom data simulation with additional complexity
simulate_complex_data <- function(n_control, n_treatment) {
  total_n <- n_control + n_treatment
  
  # Simulate complex baseline relationships
  age <- rnorm(total_n, mean = 50, sd = 15)
  baseline_severity <- rnorm(total_n, mean = 5, sd = 2)
  
  # Treatment assignment
  treatment <- c(rep(0, n_control), rep(1, n_treatment))
  
  # Complex outcome model with interactions
  outcome <- (
    0.1 * age + 
    0.3 * baseline_severity + 
    0.5 * treatment + 
    0.1 * treatment * baseline_severity + # Treatment-covariate interaction
    rnorm(total_n, 0, 1)
  )
  
  data.frame(
    outcome = outcome,
    age = age,
    baseline_severity = baseline_severity,
    treatment = factor(treatment, levels = c(0, 1), labels = c("ctrl", "treat"))
  )
}

# Create mock data for prior specification
mock_data <- simulate_complex_data(25, 25)

# Define model formulas
model_formula_true <- bf(outcome ~ age + baseline_severity + treatment + treatment:baseline_severity, center = FALSE)
model_formula_est <- bf(outcome ~ age + baseline_severity + treatment + treatment:baseline_severity)

# Set true parameters
priors_true <- c(
  set_prior("constant(0.1)", class = "b", coef = "age"),
  set_prior("constant(0.3)", class = "b", coef = "baseline_severity"),
  set_prior("constant(0.5)", class = "b", coef = "treatmenttreat"),
  set_prior("constant(0.1)", class = "b", coef = "treatmenttreat:baseline_severity"),
  set_prior("constant(0)", class = "b", coef = "Intercept"),
  set_prior("constant(1)", class = "sigma")
)

# Conservative estimation priors
priors_conservative <- c(
  set_prior("student_t(3, 0, 0.5)", class = "b", coef = "age"),
  set_prior("student_t(3, 0, 0.5)", class = "b", coef = "baseline_severity"),
  set_prior("student_t(3, 0, 0.5)", class = "b", coef = "treatmenttreat:baseline_severity"),
  set_prior("student_t(3, 0, 2)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sigma")
)

# Validate the complex design
validation_complex <- validate_power_design(
  n_control = 60,
  n_treatment = 60,
  model_formula_true_params = model_formula_true,
  model_formula_estimation = model_formula_est,
  family = gaussian(),
  priors_true_params = priors_true,
  priors_estimation = priors_conservative,
  target_param = "treatmenttreat",
  simulate_data_fn = simulate_complex_data,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

print(validation_complex)


## ----missing_data_simulation--------------------------------------------------
# Simulate power analysis with missing data considerations
missing_data_analysis <- function(missing_rate = 0.1) {
  # Adjust sample size to account for missing data
  adjusted_n_control <- ceiling(60 / (1 - missing_rate))
  adjusted_n_treatment <- ceiling(60 / (1 - missing_rate))
  
  power_result <- power_analysis_ancova(
    n_control = adjusted_n_control,
    n_treatment = adjusted_n_treatment,
    effect_size = 0.5,
    baseline_effect = 0.3,
    outcome_type = "continuous",
    threshold_success = 0.3,
    threshold_futility = 0.1,
    n_simulations = n_sims_demo,
    n_cores = n_cores_demo,
    brms_args = list(
      algorithm = "meanfield",
      importance_resampling = TRUE,
      iter = 1e4,
      output_samples = 1e3
    )
  )
  
  return(list(
    adjusted_n_control = adjusted_n_control,
    adjusted_n_treatment = adjusted_n_treatment,
    power_success = power_result$power_success,
    effective_n_control = adjusted_n_control * (1 - missing_rate),
    effective_n_treatment = adjusted_n_treatment * (1 - missing_rate)
  ))
}

# Test different missing data rates
missing_rates <- c(0.05, 0.10, 0.15, 0.20)
missing_data_results <- data.frame(
  Missing_Rate = numeric(),
  Adjusted_N_Control = numeric(),
  Adjusted_N_Treatment = numeric(),
  Effective_N_Control = numeric(),
  Effective_N_Treatment = numeric(),
  Power_Success = numeric(),
  stringsAsFactors = FALSE
)

for (rate in missing_rates) {
  result <- missing_data_analysis(rate)
  missing_data_results <- rbind(missing_data_results, data.frame(
    Missing_Rate = rate,
    Adjusted_N_Control = result$adjusted_n_control,
    Adjusted_N_Treatment = result$adjusted_n_treatment,
    Effective_N_Control = result$effective_n_control,
    Effective_N_Treatment = result$effective_n_treatment,
    Power_Success = result$power_success
  ))
}

print(missing_data_results)


## ----sequential_monitoring----------------------------------------------------
# Simulate sequential monitoring at different interim points
sequential_analysis <- function(interim_fractions = c(0.5, 0.75, 1.0)) {
  max_n <- 100
  results <- data.frame(
    Interim_Fraction = numeric(),
    N_Per_Group = numeric(),
    Power_Success = numeric(),
    Power_Futility = numeric(),
    Stop_For_Efficacy = logical(),
    Stop_For_Futility = logical(),
    stringsAsFactors = FALSE
  )
  
  for (fraction in interim_fractions) {
    interim_n <- ceiling(max_n * fraction)
    
    power_result <- power_analysis_ancova(
      n_control = interim_n,
      n_treatment = interim_n,
      effect_size = 0.5,
      baseline_effect = 0.3,
      outcome_type = "continuous",
      threshold_success = 0.3,
      threshold_futility = 0.1,
      p_sig_success = 0.95,
      p_sig_futility = 0.05,
      n_simulations = n_sims_demo,
      n_cores = n_cores_demo,
      brms_args = list(
        algorithm = "meanfield",
        importance_resampling = TRUE,
        iter = 1e4,
        output_samples = 1e3
      )
    )
    
    # Decision rules for stopping
    stop_efficacy <- power_result$power_success > 0.95
    stop_futility <- power_result$power_futility > 0.95
    
    results <- rbind(results, data.frame(
      Interim_Fraction = fraction,
      N_Per_Group = interim_n,
      Power_Success = power_result$power_success,
      Power_Futility = power_result$power_futility,
      Stop_For_Efficacy = stop_efficacy,
      Stop_For_Futility = stop_futility
    ))
  }
  
  return(results)
}

# Run sequential analysis
sequential_results <- sequential_analysis(c(0.4, 0.6, 0.8, 1.0))
print(sequential_results)


## ----binary_advanced----------------------------------------------------------
# Binary outcome with varying baseline risk
binary_baseline_analysis <- power_grid_analysis(
  sample_sizes = c(100, 150, 200),
  effect_sizes = c(0.3, 0.5, 0.7), # Log odds ratios
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "binary",
  baseline_prob = 0.2, # 20% baseline risk
  baseline_effect = 0.3, # Baseline covariate effect
  threshold_success = 0.4, # Clinically meaningful log OR
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

print(binary_baseline_analysis)


## ----binary_visualization-----------------------------------------------------
# Visualize binary outcome power analysis
plot(binary_baseline_analysis, type = "heatmap")


## ----count_advanced-----------------------------------------------------------
# Count outcome analysis with different baseline rates
count_baseline_analysis <- power_grid_analysis(
  sample_sizes = c(80, 120, 160),
  effect_sizes = c(0.2, 0.4, 0.6), # Log rate ratios
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "count",
  baseline_rate = 3.0, # Average 3 events in control
  baseline_effect = 0.2, # Baseline covariate effect
  threshold_success = 0.3, # Clinically meaningful log rate ratio
  threshold_futility = 0.1,
  n_simulations = n_sims_demo,
  n_cores = n_cores_demo,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e4,
    output_samples = 1e3
  )
)

print(count_baseline_analysis)


## ----cost_effectiveness-------------------------------------------------------
# Function to calculate cost-effectiveness
calculate_cost_effectiveness <- function(sample_sizes, cost_per_participant = 1000, fixed_costs = 50000) {
  cost_results <- data.frame(
    Sample_Size = numeric(),
    Total_Cost = numeric(),
    Power_Success = numeric(),
    Cost_Per_Power_Unit = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (n in sample_sizes) {
    power_result <- power_analysis_ancova(
      n_control = n,
      n_treatment = n,
      effect_size = 0.5,
      baseline_effect = 0.3,
      outcome_type = "continuous",
      threshold_success = 0.3,
      threshold_futility = 0.1,
      n_simulations = n_sims_demo,
      n_cores = n_cores_demo,
      brms_args = list(
        algorithm = "meanfield",
        importance_resampling = TRUE,
        iter = 1e4,
        output_samples = 1e3
      )
    )
    
    total_cost <- fixed_costs + (2 * n * cost_per_participant)
    cost_per_power <- total_cost / power_result$power_success
    
    cost_results <- rbind(cost_results, data.frame(
      Sample_Size = n,
      Total_Cost = total_cost,
      Power_Success = power_result$power_success,
      Cost_Per_Power_Unit = cost_per_power
    ))
  }
  
  return(cost_results)
}

# Analyze cost-effectiveness
cost_analysis <- calculate_cost_effectiveness(
  sample_sizes = c(60, 80, 100, 120),
  cost_per_participant = 1000,
  fixed_costs = 50000
)

print(cost_analysis)

# Find most cost-effective design
optimal_design <- cost_analysis[which.min(cost_analysis$Cost_Per_Power_Unit), ]
cat("Most cost-effective design:\n")
cat("Sample size per group:", optimal_design$Sample_Size, "\n")
cat("Total cost: $", format(optimal_design$Total_Cost, big.mark = ","), "\n")
cat("Power:", round(optimal_design$Power_Success, 3), "\n")
cat("Cost per power unit: $", format(round(optimal_design$Cost_Per_Power_Unit), big.mark = ","), "\n")


## ----advanced_workflow--------------------------------------------------------
# Comprehensive workflow for complex study design
advanced_workflow <- function() {
  # Step 1: Initial exploration with fast algorithm
  cat("Step 1: Initial exploration\n")
  pilot_grid <- power_grid_analysis(
    sample_sizes = c(60, 80, 100),
    effect_sizes = c(0.3, 0.5, 0.7),
    design_prior = "normal(0.5, 0.15)",
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.3,
    threshold_success = 0.3,
    threshold_futility = 0.1,
    n_simulations = n_sims_demo,
    n_cores = n_cores_demo,
    brms_args = list(algorithm = "meanfield", importance_resampling = TRUE, iter = 1e4, output_samples = 1e3)
  )
  
  # Step 2: Refine around optimal region
  cat("Step 2: Refinement around optimal region\n")
  refined_analysis <- power_grid_analysis(
    sample_sizes = c(70, 80, 90),
    effect_sizes = seq(0.4, 0.6, by = 0.05),
    design_prior = "normal(0.5, 0.1)",
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.3,
    threshold_success = 0.3,
    threshold_futility = 0.1,
    n_simulations = n_sims_demo,
    n_cores = n_cores_demo,
    brms_args = list(algorithm = "fullrank", importance_resampling = TRUE, iter = 1e4, output_samples = 1e3)
  )
  
  # Step 3: Final confirmation with highest accuracy
  cat("Step 3: Final confirmation\n")
  final_power <- power_analysis_ancova(
    n_control = 80,
    n_treatment = 80,
    effect_size = 0.5,
    baseline_effect = 0.3,
    outcome_type = "continuous",
    threshold_success = 0.3,
    threshold_futility = 0.1,
    n_simulations = n_sims_demo * 2, # More simulations for final analysis
    n_cores = n_cores_demo,
    brms_args = list(algorithm = "fullrank", importance_resampling = TRUE, iter = 1e4, output_samples = 1e3)
  )
  
  return(list(
    pilot = pilot_grid,
    refined = refined_analysis,
    final = final_power
  ))
}

# Run the advanced workflow
workflow_results <- advanced_workflow()
print(workflow_results$final)


## ----session_info-------------------------------------------------------------
sessionInfo()


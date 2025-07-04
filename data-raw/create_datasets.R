# Create example datasets for the rctbayespower package

# Example 1: Simulated cardiovascular trial data
set.seed(123)
cardio_trial <- simulate_rct_data(
  n_control = 100,
  n_treatment = 100,
  effect_size = 0.4,
  outcome_type = "continuous",
  baseline_mean = 30, # LDL cholesterol reduction
  baseline_sd = 15,
  covariates = list(
    age = list(type = "continuous", mean = 65, sd = 8),
    sex = list(type = "binary", prob = 0.6), # 60% male
    baseline_ldl = list(type = "continuous", mean = 140, sd = 25)
  )
)
cardio_trial$outcome_label <- "LDL_reduction_mg_dL"

# Example 2: Simulated depression trial data
set.seed(456)
depression_trial <- simulate_rct_data(
  n_control = 75,
  n_treatment = 75,
  effect_size = 0.6,
  outcome_type = "continuous",
  baseline_mean = 0, # Change in depression score
  baseline_sd = 8,
  covariates = list(
    age = list(type = "continuous", mean = 45, sd = 12),
    sex = list(type = "binary", prob = 0.35), # 35% male
    baseline_severity = list(type = "continuous", mean = 25, sd = 5)
  )
)
depression_trial$outcome_label <- "depression_score_change"

# Example 3: Simulated vaccine efficacy trial (binary outcome)
set.seed(789)
vaccine_trial <- simulate_rct_data(
  n_control = 500,
  n_treatment = 500,
  effect_size = 0.5,
  outcome_type = "binary",
  baseline_prob = 0.1, # 10% infection rate in control
  covariates = list(
    age = list(type = "continuous", mean = 50, sd = 15),
    comorbidities = list(type = "binary", prob = 0.3),
    prior_infection = list(type = "binary", prob = 0.15)
  )
)
vaccine_trial$outcome_label <- "infection_status"

# Save datasets
usethis::use_data(cardio_trial, overwrite = TRUE)
usethis::use_data(depression_trial, overwrite = TRUE)
usethis::use_data(vaccine_trial, overwrite = TRUE)

# Create documentation for datasets
cat('
#\' Simulated Cardiovascular Trial Data
#\'
#\' A dataset containing simulated data from a cardiovascular clinical trial
#\' testing a new LDL cholesterol-lowering medication.
#\'
#\' @format A data frame with 200 rows and 6 variables:
#\' \\describe{
#\'   \\item{group}{Treatment group assignment (control/treatment)}
#\'   \\item{outcome}{LDL cholesterol reduction in mg/dL}
#\'   \\item{age}{Patient age in years}
#\'   \\item{sex}{Patient sex (0 = female, 1 = male)}
#\'   \\item{baseline_ldl}{Baseline LDL cholesterol level}
#\'   \\item{outcome_label}{Label describing the outcome measure}
#\' }
#\' @source Simulated using simulate_rct_data()
"cardio_trial"

#\' Simulated Depression Trial Data
#\'
#\' A dataset containing simulated data from a depression treatment trial
#\' testing a new antidepressant medication.
#\'
#\' @format A data frame with 150 rows and 6 variables:
#\' \\describe{
#\'   \\item{group}{Treatment group assignment (control/treatment)}
#\'   \\item{outcome}{Change in depression score}
#\'   \\item{age}{Patient age in years}
#\'   \\item{sex}{Patient sex (0 = female, 1 = male)}
#\'   \\item{baseline_severity}{Baseline depression severity score}
#\'   \\item{outcome_label}{Label describing the outcome measure}
#\' }
#\' @source Simulated using simulate_rct_data()
"depression_trial"

#\' Simulated Vaccine Efficacy Trial Data
#\'
#\' A dataset containing simulated data from a vaccine efficacy trial
#\' testing infection prevention.
#\'
#\' @format A data frame with 1000 rows and 6 variables:
#\' \\describe{
#\'   \\item{group}{Treatment group assignment (control/treatment)}
#\'   \\item{outcome}{Infection status (0 = no infection, 1 = infection)}
#\'   \\item{age}{Patient age in years}
#\'   \\item{comorbidities}{Presence of comorbidities (0 = no, 1 = yes)}
#\'   \\item{prior_infection}{History of prior infection (0 = no, 1 = yes)}
#\'   \\item{outcome_label}{Label describing the outcome measure}
#\' }
#\' @source Simulated using simulate_rct_data()
"vaccine_trial"
', file = "data-raw/datasets_documentation.R")

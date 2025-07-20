# Load package - try devtools first, then pkgload, then library
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all()
} else {
  # Install and load the package
  install.packages(".", repos = NULL, type = "source")
  library(rctbayespower)
}


#-------------------------------------------------------------------------------
# 1. Create model

model_file <- here::here("test_model_ancova.rds")
if (file.exists(model_file)) {
  # load the model from a file
  model_ancova <- readRDS(model_file)
} else{
  # create the model
  model_ancova <- build_model_ancova_cont()
  # save the model to a file
  saveRDS(model_ancova, file = model_file)
}


#-------------------------------------------------------------------------------
# 2. Create design

design <- build_design(
  model = model_ancova,
  target_params = "b_grouptreat",
  thresholds_success = 0.2,
  thresholds_futility = 0.0,
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
# check the required parameters for the design
required_parameters(design)


#-------------------------------------------------------------------------------
# 3. Create conditions

conditions <- build_conditions(
  design = design,
  condition_values = list(
    # two sample sizes
    n_total = 100,
    # two effect sizes
    b_grouptreat = c(0.3)
  ),
  static_values = list(
    # equal allocation
    p_alloc =
      list(c(0.5, 0.5)),
    # baseline effect
    b_baseline = 0.2
  )
)
print(conditions)

#-------------------------------------------------------------------------------
# 4. Run analysis

n_cores <- 2
result <- power_grid_analysis(
  conditions = conditions,
  n_cores = n_cores,
  n_simulations = n_cores * 100
)

result


conditions$condition_arguments[[1]]

brmsfit <- simulate_single_run(
  condition_arguments = conditions$condition_arguments[[1]],
  design = conditions$design
)

res_df <- compute_measures_brmsfit(brmsfit, conditions$design)

# design$target_params <- c("b_grouptreat", "b_Intercept")
# design$thresholds_success <- c(0.2, 0.0)
# design$thresholds_futility <- c(0.0, 0.0)
# 
# brmsfit <- simulate_single_run(conditions$condition_arguments[[1]], design)

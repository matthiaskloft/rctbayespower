# Load package - try devtools first, then pkgload, then library
devtools::load_all()
#library(rctbayespower)
#-------------------------------------------------------------------------------
# 1. Create model

# get the model
list_predefined_models()
model_ancova <- build_model(predefined_model = "ancova_cont_2arms")


class(model_ancova)
print(model_ancova)

#-------------------------------------------------------------------------------
# 2. Create design

design <- build_design(
  model = model_ancova,
  target_params = "b_arm2",
  thresholds_success = 0.0,
  thresholds_futility = 0.0,
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
# check the required parameters for the design
required_fn_args(design)


#-------------------------------------------------------------------------------
# 3. Create conditions

conditions <- build_conditions(
  design = design,
  condition_values = list(
    # two sample sizes
    n_total = 400,
    
    # two effect sizes
    b_arm_treat = c(.5)
  ),
  static_values = list(
    # baseline effect
    b_covariate = c(0),
    # equal allocation
    p_alloc =
      list(c(0.5, 0.5))
  )
)

#-------------------------------------------------------------------------------
# 4. Run analysis


n_cores <- 15
n_sims <- 1e3
# run the power analysis
result <- power_analysis(
  conditions = conditions,
  n_cores = n_cores,
  n_sims = n_sims,
  verbose = TRUE
)

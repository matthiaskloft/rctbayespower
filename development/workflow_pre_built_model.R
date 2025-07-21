# Load package - try devtools first, then pkgload, then library
devtools::load_all()


#-------------------------------------------------------------------------------
# 1. Create model

model_file <- here::here("test_model_ancova.rds")
if (file.exists(model_file)) {
  # load the model from a file
  model_ancova <- readRDS(model_file)
} else{
  # create the model
  model_ancova <- build_model(pre_defined_model = "ancova_cont_2arms")
  # save the model to a file
  saveRDS(model_ancova, file = model_file)
}

model_ancova$parameter_names_brms


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
    # baseline effect
    b_covariate = c(0, .5),
    # two effect sizes
    b_arms_treat = c(0, .5)
  ),
  static_values = list(
    # equal allocation
    p_alloc =
      list(c(0.5, 0.5))
  )
)
print(conditions, n = 100)

#-------------------------------------------------------------------------------
# 4. Run analysis
#future::plan("sequential")
n_cores <- 2
result <- power_analysis(
  conditions = conditions,
  n_cores = n_cores,
  n_simulations = n_cores * 10,
  verbose = TRUE
)

model_ancova$data_simulation_fn(conditions$condition_arguments[[1]]$sim_args)
conditions$condition_arguments[[1]]$sim_args
model_ancova$data_simulation_fn
do.call(
  model_ancova$data_simulation_fn,
  conditions$condition_arguments[[1]]$sim_args
)
get_arg_defaults(model_ancova$data_simulation_fn)

# design$target_params <- c("b_arms_treat", "b_Intercept")
# design$thresholds_success <- c(0.2, 0.0)
# design$thresholds_futility <- c(0.0, 0.0)
#
# brmsfit <- simulate_single_run(conditions$condition_arguments[[1]], design)

# Debug script for power analysis issues
devtools::load_all(".")

# Try to reproduce the exact conditions from the error
# Create the same setup as the failing code

# Create model
ancova_model <- build_model("ancova_cont_2arms")()

# Create design
design <- build_design(
  model = ancova_model,
  target_params = "b_arm_treat",
  n_interim_analyses = 0,
  thresholds_success = 0.2,
  thresholds_futility = 0,
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

# Create conditions
conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = c(400),
    b_arm_treat = c(0, 0.5)
  ),
  static_values = list(
    b_covariate = c(0, 0.5)
  )
)

# Test a single condition manually
print("Testing single condition...")
test_condition <- conditions@condition_arguments[[1]]

print("Condition arguments:")
str(test_condition)

print("Testing simulate_single_run...")
test_result <- simulate_single_run(
  condition_arguments = test_condition,
  id_sim = 1,
  design = design,
  brms_args = list()
)

print("Result:")
print(test_result)
print(class(test_result))
print(dim(test_result))

# Test if rbind works
if (!is.null(test_result) && is.data.frame(test_result)) {
  print("Testing rbind...")
  test_list <- list(test_result, test_result)
  combined <- do.call(rbind, test_list)
  print(dim(combined))
} else {
  print("simulate_single_run returned NULL or non-data.frame")
}
# =============================================================================
# DEBUG SCRIPT: Raw mlr3mbo Workflow with BayesFlow Backend
# =============================================================================
# Purpose: Step-by-step debugging of optimization to identify output sources
# Run interactively to see where data.frames are printed

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(rctbayespower)
library(paradox)
library(bbotk)
library(mlr3mbo)
library(data.table)

# Suppress mlr3/bbotk logging
lgr::get_logger("mlr3")$set_threshold("off")
lgr::get_logger("bbotk")$set_threshold("off")

# Suppress data.table printing
options(

  datatable.print.nrows = 0,
  datatable.print.topn = 0,
  datatable.print.class = FALSE,
  datatable.verbose = FALSE
)

# -----------------------------------------------------------------------------
# STEP 1: Create Design (BayesFlow backend)
# -----------------------------------------------------------------------------
cat("\n=== STEP 1: Create Design ===\n")

init_bf_python("rctbp-3-12-gpu")

design <- build_design(

  model_name = "ancova_cont_2arms",
  backend = "bf",  # Will fall back to brms if bf unavailable

  target_params = "b_group"
)

cat("Backend:", design@backend, "\n")

# -----------------------------------------------------------------------------
# STEP 2: Define Search Space and Constants
# -----------------------------------------------------------------------------
cat("\n=== STEP 2: Define Parameters ===\n")

search_params <- list(n_total = c(50, 500))
constant_params <- list(

  b_arm_treat = 0.5,
  thr_dec_eff = 0.90,
  thr_dec_fut = 0.5,
  thr_fx_eff = 0,
  thr_fx_fut = 0,
  p_alloc = list(c(0.5, 0.5)),
  intercept = 0,
  b_covariate = 0.3,
  sigma = 1
)

cat("Search:", names(search_params), "\n")
cat("Constant:", names(constant_params), "\n")

# -----------------------------------------------------------------------------
# STEP 3: Create paradox Parameter Space
# -----------------------------------------------------------------------------
cat("\n=== STEP 3: Create Parameter Space (paradox) ===\n")

domain <- ps(

  n_total = p_int(lower = 50, upper = 200)
)

cat("Domain created\n")
print(domain)

# -----------------------------------------------------------------------------
# STEP 4: Create Codomain (Objective)
# -----------------------------------------------------------------------------
cat("\n=== STEP 4: Create Codomain ===\n")

codomain <- ps(
  pwr_eff = p_dbl(tags = "maximize")
)

cat("Codomain created\n")
print(codomain)

# -----------------------------------------------------------------------------
# STEP 5: Create Objective Function
# -----------------------------------------------------------------------------
cat("\n=== STEP 5: Create Objective Function ===\n")

# Counter for tracking evaluations
eval_count <- 0

objective_fn <- function(xs) {
  eval_count <<- eval_count + 1
  cat("  Evaluation", eval_count, "- n_total:", xs$n_total, "\n")

  # Build conditions
  conditions <- build_conditions(
    design = design,
    crossed = list(n_total = xs$n_total),
    constant = constant_params
  )

  # Run power analysis (few sims for debugging)
  pa_result <- power_analysis(
    conditions = conditions,
    n_sims = 10,
    n_cores = 1,
    verbosity = 0,
    run = TRUE
  )

  # Extract power
  pwr <- pa_result@results_conditions$pwr_eff[1]
  cat("    -> pwr_eff:", pwr, "\n")

  list(pwr_eff = pwr)
}

cat("Objective function created\n")

# -----------------------------------------------------------------------------
# STEP 6: Create bbotk Objective
# -----------------------------------------------------------------------------
cat("\n=== STEP 6: Create bbotk ObjectiveRFun ===\n")

objective <- ObjectiveRFun$new(

  fun = objective_fn,
  domain = domain,
  codomain = codomain
)

cat("ObjectiveRFun created\n")

# -----------------------------------------------------------------------------
# STEP 7: Create OptimInstance
# -----------------------------------------------------------------------------
cat("\n=== STEP 7: Create OptimInstance ===\n")

instance <- OptimInstanceBatchSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 6)
)

cat("OptimInstance created\n")

# -----------------------------------------------------------------------------
# STEP 8: Generate Initial Design
# -----------------------------------------------------------------------------
cat("\n=== STEP 8: Generate Initial Design ===\n")

init_design_pts <- generate_design_lhs(domain, n = 3)$data

cat("Initial design points:\n")
print(init_design_pts)

# -----------------------------------------------------------------------------
# STEP 9: Evaluate Initial Design (WATCH FOR OUTPUT HERE)
# -----------------------------------------------------------------------------
cat("\n=== STEP 9: Evaluate Initial Design ===\n")
cat(">>> Calling instance$eval_batch() - watch for unexpected output <<<\n\n")

# This is where data.frames might be printed
result_init <- instance$eval_batch(init_design_pts)

cat("\n>>> eval_batch complete <<<\n")
cat("Archive rows:", nrow(instance$archive$data), "\n")

# -----------------------------------------------------------------------------
# STEP 10: Set up MBO Components
# -----------------------------------------------------------------------------
cat("\n=== STEP 10: Set up MBO Components ===\n")

# Surrogate (Random Forest for simplicity)
surrogate <- default_surrogate(instance)
surrogate$param_set$values$catch_errors <- TRUE

cat("Surrogate created\n")

# Acquisition function
acq_function <- acqf("ei")
cat("Acquisition function: EI\n")

# Acquisition optimizer
acq_optimizer <- acqo(
  optimizer = opt("random_search", batch_size = 100),
  terminator = trm("evals", n_evals = 100)
)
acq_optimizer$param_set$values$logging_level <- "warn"

cat("Acquisition optimizer created\n")

# -----------------------------------------------------------------------------
# STEP 11: Create MBO Optimizer
# -----------------------------------------------------------------------------
cat("\n=== STEP 11: Create MBO Optimizer ===\n")

optimizer <- opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer
)

cat("MBO Optimizer created\n")

# -----------------------------------------------------------------------------
# STEP 12: Run Optimization (WATCH FOR OUTPUT HERE)
# -----------------------------------------------------------------------------
cat("\n=== STEP 12: Run Optimization ===\n")
cat(">>> Calling optimizer$optimize() - watch for unexpected output <<<\n\n")

# This is where data.frames might be printed during the loop
result <- optimizer$optimize(instance)

cat("\n>>> optimize complete <<<\n")

# -----------------------------------------------------------------------------
# STEP 13: Results
# -----------------------------------------------------------------------------
cat("\n=== STEP 13: Results ===\n")

cat("Total evaluations:", nrow(instance$archive$data), "\n")
cat("\nArchive:\n")
print(instance$archive$data[, .(n_total, pwr_eff)])

cat("\nBest result:\n")
print(instance$archive$best())

# -----------------------------------------------------------------------------
# STEP 14: Identify Output Source
# -----------------------------------------------------------------------------
cat("\n=== STEP 14: Summary ===\n")
cat("If data.frames appeared during Steps 9 or 12, the output comes from:\n")
cat("  - Step 9: bbotk instance$eval_batch()\n")
cat("  - Step 12: mlr3mbo optimizer$optimize()\n")
cat("\nTo suppress, wrap these calls in:\n")
cat("  invisible(capture.output(...))\n")
cat("  or use sink() before and after\n")

# =============================================================================
# MINIMAL BAYESFLOW INFERENCE WORKFLOW - UNDER THE HOOD
# =============================================================================
# This script demonstrates the low-level workflow that happens when doing
# BayesFlow inference for one simulation condition, bypassing the high-level
# package API to show the core steps.

# Load necessary packages
library(rctbayespower)

# =============================================================================
# STEP 1: PYTHON ENVIRONMENT INITIALIZATION
# =============================================================================

# Initialize BayesFlow Python environment (required at start of session)
# This sets up the reticulate environment and loads BayesFlow modules
cat("Step 1: Initializing BayesFlow Python environment...\n")

# install venv
if(!reticulate::virtualenv_exists("rctbp-3-12")){
  setup_bf_python(envname = "rctbp-3-12", python_version = "3.12", cuda_version = "auto")
}
# Initialize BayesFlow Python environment
init_bf("rctbp-3-12")

# =============================================================================
# STEP 2: SIMULATION PARAMETERS (ONE CONDITION)
# =============================================================================

cat("\nStep 2: Setting up simulation parameters for one condition...\n")

# Define parameters for a single condition
n_sims <- 4  # Small batch for demonstration (normally 32-64)
n_total <- 100
b_arm_treat <- 0.5    # Treatment effect size
b_covariate <- 0.3    # Covariate effect
sigma <- 1            # Residual standard deviation
intercept <- 0
p_alloc <- 0.5        # 50-50 treatment allocation

# Decision thresholds
thr_fx_eff <- 0.2     # Effect size threshold for efficacy
thr_fx_fut <- 0       # Effect size threshold for futility
thr_dec_eff <- 0.975  # Probability threshold for efficacy decision
thr_dec_fut <- 0.5    # Probability threshold for futility decision

cat("  - n_total:", n_total, "\n")
cat("  - b_arm_treat:", b_arm_treat, "\n")
cat("  - n_sims (batch size):", n_sims, "\n")

# =============================================================================
# STEP 3: DATA SIMULATION (BATCH GENERATION)
# =============================================================================

cat("\nStep 3: Generating simulated data batch...\n")

# Use the internal batch simulation function
# This generates multiple trials simultaneously for efficiency
batch_data <- rctbayespower:::simulate_data_ancova_cont_2arms_batch(
  n_sims = n_sims,
  n_total = n_total,
  p_alloc = p_alloc,
  intercept = intercept,
  b_arm_treat = b_arm_treat,
  b_covariate = b_covariate,
  sigma = sigma
)

cat("  - Generated data shapes:\n")
cat("    * outcome:", dim(batch_data$outcome), "(sims x participants)\n")
cat("    * covariate:", dim(batch_data$covariate), "\n")
cat("    * group:", dim(batch_data$group), "\n")

# Preview first simulation
cat("  - First simulation preview (first 5 participants):\n")
sim_1_df <- data.frame(
  outcome = batch_data$outcome[1, 1:5],
  covariate = batch_data$covariate[1, 1:5],
  group = batch_data$group[1, 1:5]
)
print(sim_1_df)

# =============================================================================
# STEP 4: DATA PREPARATION FOR BAYESFLOW
# =============================================================================

cat("\nStep 4: Preparing data for BayesFlow neural network...\n")

# Convert the batch data to the format expected by BayesFlow models
# This involves field mapping and data transformation
data_batch <- list(
  outcome = batch_data$outcome,      # n_sims x n_total matrix
  covariate = batch_data$covariate,  # n_sims x n_total matrix
  group = batch_data$group,          # n_sims x n_total matrix (0/1)
  N = n_total,                       # Sample size metadata
  p_alloc = p_alloc                  # Allocation probability metadata
)

cat("  - Data batch prepared with fields:", names(data_batch), "\n")
cat("  - Batch ready for neural network inference\n")

# =============================================================================
# STEP 5: BAYESFLOW MODEL LOADING (MOCK MODE)
# =============================================================================

cat("\nStep 5: Loading BayesFlow model...\n")

# For this demonstration, we'll use mock mode since we don't have a trained model
# In practice, you would load a trained .keras model file:
# bf_model <- rctbayespower:::load_bf_model_python("/path/to/trained_model.keras")

# Enable mock mode for demonstration
# Sys.setenv(RCTBP_MOCK_BF = "TRUE")
# bf_model <- NULL  # Will be handled by mock mode

bf_model <- rctbayespower:::load_bf_model_python(here::here("dev","bf_models","ancova_cont_2arms.keras"))

cat("  - Using mock mode (RCTBP_MOCK_BF=TRUE)\n")
cat("  - In practice: load trained BayesFlow model from .keras file\n")

# =============================================================================
# STEP 6: BAYESFLOW INFERENCE (CORE NEURAL NETWORK CALL)
# =============================================================================

cat("\nStep 6: Running BayesFlow inference...\n")

# Set backend arguments
backend_args <- list(
  n_posterior_samples = 1000L,  # Number of posterior samples to draw
  batch_size = n_sims          # Process all simulations in one batch
)

# Core inference call - this is where the neural network runs
target_param <- "b_arm2"  # Parameter we want to analyze (treatment effect)

cat("  - Target parameter:", target_param, "\n")
cat("  - Posterior samples per simulation:", backend_args$n_posterior_samples, "\n")

# Call the core BayesFlow inference function
draws_matrix <- rctbayespower:::estimate_batch_bf(
  data_batch = data_batch,
  bf_model = bf_model,         # NULL in mock mode
  backend_args = backend_args,
  target_params = target_param
)

cat("  - Inference complete!\n")
cat("  - Draws matrix shape:", dim(draws_matrix), "(sims x posterior_samples)\n")

# Show sample of posterior draws for first simulation
cat("  - First simulation posterior samples (first 10):\n")
cat("    ", round(draws_matrix[1, 1:10], 3), "\n")

# =============================================================================
# STEP 7: POSTERIOR SUMMARIZATION (VECTORIZED)
# =============================================================================

cat("\nStep 7: Summarizing posterior distributions...\n")

# Fast vectorized summarization using matrixStats
# This computes decision probabilities and summary statistics
result_summary <- rctbayespower:::summarize_post_bf(
  draws_mat = draws_matrix,
  target_param = target_param,
  thr_fx_eff = thr_fx_eff,      # Efficacy threshold
  thr_fx_fut = thr_fx_fut,      # Futility threshold
  thr_dec_eff = thr_dec_eff,    # Decision threshold for efficacy
  thr_dec_fut = thr_dec_fut,    # Decision threshold for futility
  id_iter = 1:n_sims,           # Simulation IDs
  id_cond = rep(1, n_sims),     # Condition ID (all same condition)
  id_look = rep(1L, n_sims),    # Analysis look (single analysis)
  n_analyzed = n_total,         # Sample size analyzed
  skip_convergence = TRUE       # Skip convergence diagnostics (not needed for NPE)
)

cat("  - Summary computed for", nrow(result_summary), "simulations\n")

# =============================================================================
# STEP 8: RESULTS INTERPRETATION
# =============================================================================

cat("\nStep 8: Results interpretation...\n")

# Show results table
cat("  - Results summary:\n")
print(result_summary[, c("id_iter", "pr_eff", "pr_fut", "dec_eff", "dec_fut",
                         "post_med", "post_sd")])

cat("\n  - Column meanings:\n")
cat("    * pr_eff: P(treatment effect > ", thr_fx_eff, ")\n")
cat("    * pr_fut: P(treatment effect < ", thr_fx_fut, ")\n")
cat("    * dec_eff: Decision for efficacy (1 if pr_eff >= ", thr_dec_eff, ")\n")
cat("    * dec_fut: Decision for futility (1 if pr_fut >= ", thr_dec_fut, ")\n")
cat("    * post_med: Posterior median of treatment effect\n")
cat("    * post_sd: Posterior standard deviation\n")

# Calculate power (proportion of simulations declaring efficacy)
power_eff <- mean(result_summary$dec_eff)
power_fut <- mean(result_summary$dec_fut)

cat("\n  - Power Analysis Results:\n")
cat("    * Power for efficacy:", round(power_eff * 100, 1), "%\n")
cat("    * Power for futility:", round(power_fut * 100, 1), "%\n")
cat("    * True treatment effect:", b_arm_treat, "\n")

# =============================================================================
# STEP 9: WORKFLOW SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("WORKFLOW SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

cat("1. Python Environment: BayesFlow initialized with GPU/CPU detection\n")
cat("2. Data Simulation: Vectorized batch generation (", n_sims, " sims x ", n_total, " participants)\n")
cat("3. Data Preparation: R matrices → NumPy arrays for neural network\n")
cat("4. Model Loading: BayesFlow .keras model (mock mode in this demo)\n")
cat("5. Inference: Single forward pass → ", backend_args$n_posterior_samples, " posterior samples per sim\n")
cat("6. Summarization: Vectorized probability calculations using matrixStats\n")
cat("7. Results: Power analysis with decision thresholds\n")

cat("\nKey Advantages of BayesFlow vs MCMC:\n")
cat("  • Speed: ~100-1000x faster than Stan/brms\n")
cat("  • Scalability: Process multiple simulations in single batch\n")
cat("  • No convergence issues: Neural network produces IID samples\n")
cat("  • GPU acceleration: Leverages modern deep learning infrastructure\n")

cat("\nThis completes the minimal BayesFlow workflow demonstration!\n")

# Clean up mock mode
Sys.unsetenv("RCTBP_MOCK_BF")

# =============================================================================
# ADDITIONAL: COMPARISON WITH HIGH-LEVEL API
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("COMPARISON: High-level API vs Low-level Workflow\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

cat("\nHigh-level API (what users typically use):\n")
cat('design <- build_design(model_name = "ancova_cont_2arms", backend = "bf")\n')
cat('conditions <- build_conditions(design, crossed = list(n_total = 100, b_arm_treat = 0.5), ...)\n')
cat('result <- power_analysis(conditions, n_sims = 100)\n')

cat("\nLow-level workflow (what we demonstrated):\n")
cat("1. init_bf()\n")
cat("2. simulate_data_ancova_cont_2arms_batch()\n")
cat("3. prepare batch data\n")
cat("4. load_bf_model_python()\n")
cat("5. estimate_batch_bf()\n")
cat("6. summarize_post_bf()\n")

cat("\nThe high-level API handles all the low-level steps automatically,\n")
cat("but understanding the workflow helps with:\n")
cat("  • Debugging performance issues\n")
cat("  • Custom model integration\n")
cat("  • Advanced optimization\n")
cat("  • Research and development\n")
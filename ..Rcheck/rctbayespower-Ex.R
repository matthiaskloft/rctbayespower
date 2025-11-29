pkgname <- "rctbayespower"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rctbayespower')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("analytical_power_ancova_cont_2arms")
### * analytical_power_ancova_cont_2arms

flush(stderr()); flush(stdout())

### Name: analytical_power_ancova_cont_2arms
### Title: Power Calculation for ANCOVA with Continuous Covariate and Two
###   Groups
### Aliases: analytical_power_ancova_cont_2arms

### ** Examples





cleanEx()
nameEx("bf_status")
### * bf_status

flush(stderr()); flush(stdout())

### Name: bf_status
### Title: Show Python Environment Status
### Aliases: bf_status

### ** Examples

## Not run: 
##D # Check current/default environment
##D bf_status()
##D 
##D # Check specific environment
##D bf_status(envname = "r-rctbayespower")
## End(Not run)



cleanEx()
nameEx("boundary_constant")
### * boundary_constant

flush(stderr()); flush(stdout())

### Name: boundary_constant
### Title: Constant Boundary
### Aliases: boundary_constant

### ** Examples

# Create constant boundary at 0.95
const <- boundary_constant(0.95)

# Returns same value regardless of number of looks
const(c(0.5, 1.0))  # 0.95, 0.95



cleanEx()
nameEx("boundary_hsd")
### * boundary_hsd

flush(stderr()); flush(stdout())

### Name: boundary_hsd
### Title: Hwang-Shih-DeCani Boundary
### Aliases: boundary_hsd

### ** Examples

## Not run: 
##D # Create HSD boundary (requires gsDesign)
##D hsd <- boundary_hsd(gamma = -4)  # OBF-like
##D hsd(c(0.5, 1.0))
##D 
##D hsd_pocock <- boundary_hsd(gamma = 1)  # Pocock-like
##D hsd_pocock(c(0.5, 1.0))
## End(Not run)



cleanEx()
nameEx("boundary_linear")
### * boundary_linear

flush(stderr()); flush(stdout())

### Name: boundary_linear
### Title: Linear Boundary
### Aliases: boundary_linear

### ** Examples

# Success boundary: strict early, relaxed late
scs_boundary <- boundary_linear(start = 0.999, end = 0.975)
scs_boundary(0.5)  # 0.987

# Futility boundary: lenient early, strict late
ftl_boundary <- boundary_linear(start = 0.70, end = 0.90)
ftl_boundary(0.5)  # 0.80



cleanEx()
nameEx("boundary_obf")
### * boundary_obf

flush(stderr()); flush(stdout())

### Name: boundary_obf
### Title: O'Brien-Fleming Boundary
### Aliases: boundary_obf boundary_obf_threshold

### ** Examples

# Frequentist: control Type I error at 2.5%
obf_freq <- boundary_obf(alpha = 0.025)
obf_freq(c(0.5, 1.0))

# Bayesian: OBF shape ending at 0.95 threshold
obf_bayes <- boundary_obf(threshold = 0.95)
obf_bayes(c(0.5, 1.0))



cleanEx()
nameEx("boundary_pocock")
### * boundary_pocock

flush(stderr()); flush(stdout())

### Name: boundary_pocock
### Title: Pocock Boundary
### Aliases: boundary_pocock boundary_pocock_threshold

### ** Examples

# Frequentist: control Type I error at 2.5%
pocock_freq <- boundary_pocock(alpha = 0.025)
pocock_freq(c(0.5, 1.0))

# Bayesian: constant 0.95 threshold at all looks
pocock_bayes <- boundary_pocock(threshold = 0.95)
pocock_bayes(c(0.5, 1.0))  # Returns c(0.95, 0.95)



cleanEx()
nameEx("boundary_power")
### * boundary_power

flush(stderr()); flush(stdout())

### Name: boundary_power
### Title: Power Family Boundary
### Aliases: boundary_power

### ** Examples

# Compare different rho values at info_frac = 0.5
boundary_power(0.975, rho = 3)(0.5)   # More conservative
boundary_power(0.975, rho = 2)(0.5)   # OBF-like
boundary_power(0.975, rho = 1)(0.5)   # Linear
boundary_power(0.975, rho = 0.5)(0.5) # Less conservative



cleanEx()
nameEx("build_conditions")
### * build_conditions

flush(stderr()); flush(stdout())

### Name: build_conditions
### Title: Build Conditions for Power Analysis
### Aliases: build_conditions

### ** Examples

## Not run: 
##D # Simple example without link()
##D conditions <- build_conditions(
##D   design = my_design,
##D   crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
##D   constant = list(
##D     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
##D     thr_fx_eff = 0.2, thr_fx_fut = 0
##D   )
##D )
##D 
##D # With link() for co-varying parameters
##D conditions <- build_conditions(
##D   design = my_design,
##D   crossed = list(
##D     link(n_total = c(80, 160), analysis_at = list(c(40,80), c(80,160))),
##D     b_arm_treat = c(0, 0.3)
##D   ),
##D   constant = list(thr_dec_eff = 0.975, thr_dec_fut = 0.5)
##D )
## End(Not run)




cleanEx()
nameEx("build_design")
### * build_design

flush(stderr()); flush(stdout())

### Name: build_design
### Title: Build a Design for Power Analysis
### Aliases: build_design

### ** Examples

## Not run: 
##D # Using a predefined model (recommended)
##D design <- build_design(
##D   model_name = "ancova_cont_2arms",
##D   target_params = "b_arm2"
##D )
##D 
##D # Check available parameter names
##D design@par_names_inference
##D design@par_names_sim
##D 
##D # Custom model (advanced)
##D design <- build_design(
##D   sim_fn = my_sim_function,
##D   inference_model = my_brmsfit,
##D   backend = "brms",
##D   n_endpoints = 1,
##D   endpoint_types = "continuous",
##D   n_arms = 2,
##D   target_params = "b_treatment"
##D )
## End(Not run)



cleanEx()
nameEx("build_model_ancova")
### * build_model_ancova

flush(stderr()); flush(stdout())

### Name: build_model_ancova
### Title: Create General ANCOVA Model with Flexible Specifications
### Aliases: build_model_ancova

### ** Examples

## Not run: 
##D # Create 2-arm ANCOVA model
##D model_2arm <- build_model_ancova(
##D   n_arms = 2,
##D   contrasts = "contr.treatment",
##D   p_alloc = c(0.5, 0.5),
##D   intercept = 0,
##D   b_arm_treat = 0.5,
##D   b_covariate = 0.3,
##D   sigma = 1
##D )
## End(Not run)



cleanEx()
nameEx("build_model_ancova_cont_2arms")
### * build_model_ancova_cont_2arms

flush(stderr()); flush(stdout())

### Name: build_model_ancova_cont_2arms
### Title: Create 2-Arm ANCOVA Model for Continuous Outcomes
### Aliases: build_model_ancova_cont_2arms

### ** Examples

## Not run: 
##D # Create 2-arm ANCOVA model (must specify effect sizes)
##D model_2arm <- build_model_ancova_cont_2arms(
##D   b_arm_treat = 0.5,
##D   b_covariate = 0.3
##D )
## End(Not run)



cleanEx()
nameEx("build_model_ancova_cont_3arms")
### * build_model_ancova_cont_3arms

flush(stderr()); flush(stdout())

### Name: build_model_ancova_cont_3arms
### Title: Create 3-Arm ANCOVA Model for Continuous Outcomes
### Aliases: build_model_ancova_cont_3arms

### ** Examples

## Not run: 
##D # Create 3-arm ANCOVA model (must specify effect sizes)
##D model_3arm <- build_model_ancova_cont_3arms(
##D   b_arm_treat = c(0.5, 0.7),
##D   b_covariate = 0.3
##D )
## End(Not run)



cleanEx()
nameEx("check_bf_available")
### * check_bf_available

flush(stderr()); flush(stdout())

### Name: check_bf_available
### Title: Check BayesFlow Availability
### Aliases: check_bf_available

### ** Examples

## Not run: 
##D if (check_bf_available(silent = TRUE)) {
##D   message("BayesFlow is available!")
##D }
## End(Not run)



cleanEx()
nameEx("check_python_sims_available")
### * check_python_sims_available

flush(stderr()); flush(stdout())

### Name: check_python_sims_available
### Title: Check if Python Simulators are Available
### Aliases: check_python_sims_available

### ** Examples

if (check_python_sims_available(silent = TRUE)) {
  message("Python simulators available!")
}



cleanEx()
nameEx("clear_model_cache")
### * clear_model_cache

flush(stderr()); flush(stdout())

### Name: clear_model_cache
### Title: Clear Model Cache
### Aliases: clear_model_cache

### ** Examples

## Not run: 
##D # Clear all cached models
##D clear_model_cache()
##D 
##D # Clear only BayesFlow models
##D clear_model_cache("bf")
## End(Not run)



cleanEx()
nameEx("compare_boundaries")
### * compare_boundaries

flush(stderr()); flush(stdout())

### Name: compare_boundaries
### Title: Compare Multiple Boundary Configurations
### Aliases: compare_boundaries

### ** Examples

## Not run: 
##D # Run simulation once
##D result <- power_analysis(conditions, n_sims = 500, analysis_at = c(0.5, 0.75))
##D 
##D # Compare different boundary configurations
##D comparison <- compare_boundaries(result, list(
##D   "Fixed 0.975" = list(success = 0.975, futility = 0.90),
##D   "OBF-style" = list(success = boundary_obf(0.975), futility = 0.90),
##D   "Stringent" = list(success = 0.99, futility = 0.95),
##D   "Linear" = list(
##D     success = boundary_linear(0.999, 0.975),
##D     futility = boundary_linear(0.70, 0.90)
##D   )
##D ))
##D 
##D print(comparison)
## End(Not run)



cleanEx()
nameEx("compare_power_methods")
### * compare_power_methods

flush(stderr()); flush(stdout())

### Name: compare_power_methods
### Title: Compare Power Calculation Methods
### Aliases: compare_power_methods

### ** Examples




cleanEx()
nameEx("create_python_sim_fn")
### * create_python_sim_fn

flush(stderr()); flush(stdout())

### Name: create_python_sim_fn
### Title: Create Python Simulation Function for Model
### Aliases: create_python_sim_fn

### ** Examples

## Not run: 
##D # Create Python-backed simulation function
##D py_sim_fn <- create_python_sim_fn(
##D   "ancova_cont_2arms",
##D   p_alloc = 0.5,
##D   intercept = 0,
##D   b_covariate = 0.3,
##D   sigma = 1
##D )
##D 
##D # Use in model (for BayesFlow backend)
##D model <- rctbp_model(
##D   data_simulation_fn = py_sim_fn,
##D   bayesflow_model = bf_model,
##D   backend = "bf"
##D )
## End(Not run)



cleanEx()
nameEx("detect_cuda_version")
### * detect_cuda_version

flush(stderr()); flush(stdout())

### Name: detect_cuda_version
### Title: Detect CUDA Version from System
### Aliases: detect_cuda_version

### ** Examples

## Not run: 
##D cuda_ver <- detect_cuda_version()
##D cat("Detected CUDA:", cuda_ver, "\n")
## End(Not run)



cleanEx()
nameEx("export_report")
### * export_report

flush(stderr()); flush(stdout())

### Name: export_report
### Title: Export Report to File
### Aliases: export_report

### ** Examples

## Not run: 
##D design <- build_design(model_name = "ancova_cont_2arms",
##D                        target_params = "b_arm2",
##D                        p_sig_scs = 0.975, p_sig_ftl = 0.5)
##D export_report(design, "design_report.md")
## End(Not run)




cleanEx()
nameEx("f2_from_params_ancova_cont_2arms")
### * f2_from_params_ancova_cont_2arms

flush(stderr()); flush(stdout())

### Name: f2_from_params_ancova_cont_2arms
### Title: Compute Cohen's f-squared for Group Effect in Linear Model
### Aliases: f2_from_params_ancova_cont_2arms

### ** Examples




cleanEx()
nameEx("get_bf_env_info")
### * get_bf_env_info

flush(stderr()); flush(stdout())

### Name: get_bf_env_info
### Title: Get BayesFlow Environment Information
### Aliases: get_bf_env_info

### ** Examples

## Not run: 
##D info <- get_bf_env_info()
##D if (!is.null(info)) {
##D   cat("Device:", info$device_name, "\n")
##D   cat("Environment:", info$envname, "\n")
##D   cat("Python:", info$python_version, "\n")
##D   cat("BayesFlow:", info$pkg_versions$bayesflow, "\n")
##D }
##D 
##D # Check specific environment
##D info <- get_bf_env_info(envname = "r-rctbayespower")
## End(Not run)



cleanEx()
nameEx("get_cache_size")
### * get_cache_size

flush(stderr()); flush(stdout())

### Name: get_cache_size
### Title: Get Cache Size
### Aliases: get_cache_size

### ** Examples

## Not run: 
##D cache_size <- get_cache_size()
##D print(paste("Total cache:", sum(cache_size) / 1e6, "MB"))
## End(Not run)



cleanEx()
nameEx("get_output_mode")
### * get_output_mode

flush(stderr()); flush(stdout())

### Name: get_output_mode
### Title: Get Current Output Mode
### Aliases: get_output_mode

### ** Examples

get_output_mode()




cleanEx()
nameEx("get_python_simulator")
### * get_python_simulator

flush(stderr()); flush(stdout())

### Name: get_python_simulator
### Title: Get Python Simulator Class
### Aliases: get_python_simulator

### ** Examples

## Not run: 
##D # Get simulator class for BayesFlow training
##D simulator <- get_python_simulator(
##D   "ancova_cont_2arms",
##D   b_arm_treat = 0.5,
##D   b_covariate = 0.3
##D )
##D 
##D # Use with BayesFlow
##D import bayesflow as bf
##D workflow <- bf$make_simulator(simulator)
## End(Not run)



cleanEx()
nameEx("get_verbosity")
### * get_verbosity

flush(stderr()); flush(stdout())

### Name: get_verbosity
### Title: Get Current Verbosity Level
### Aliases: get_verbosity

### ** Examples

get_verbosity()




cleanEx()
nameEx("init_bf_python")
### * init_bf_python

flush(stderr()); flush(stdout())

### Name: init_bf_python
### Title: Initialize BayesFlow Python Environment
### Aliases: init_bf_python

### ** Examples

## Not run: 
##D py_mods <- init_bf_python()
##D py_mods$bf  # BayesFlow module
##D py_mods$keras  # Keras module
##D 
##D # Use specific environment
##D py_mods <- init_bf_python(envname = "r-rctbayespower")
## End(Not run)



cleanEx()
nameEx("install_bf_dependencies")
### * install_bf_dependencies

flush(stderr()); flush(stdout())

### Name: install_bf_dependencies
### Title: Install BayesFlow Dependencies
### Aliases: install_bf_dependencies

### ** Examples

## Not run: 
##D # Install with CUDA 12.4 support
##D install_bf_dependencies(cuda_version = "12.4")
## End(Not run)



cleanEx()
nameEx("interim_continue")
### * interim_continue

flush(stderr()); flush(stdout())

### Name: interim_continue
### Title: Default Interim Function: Always Continue
### Aliases: interim_continue

### ** Examples

# Create a design with sequential monitoring but no early stopping
## Not run: 
##D design <- build_design(
##D   model = my_model,
##D   target_params = "b_arm2",
##D   p_sig_scs = 0.975,
##D   p_sig_ftl = 0.5,
##D   analysis_at = c(50, 100, 150),
##D   interim_function = interim_continue()  # Or omit - this is the default
##D )
## End(Not run)



cleanEx()
nameEx("interim_futility_only")
### * interim_futility_only

flush(stderr()); flush(stdout())

### Name: interim_futility_only
### Title: Interim Function Factory: Futility Stopping Only
### Aliases: interim_futility_only

### ** Examples

## Not run: 
##D # Stop if P(futility) > 0.95
##D design <- build_design(
##D   model = my_model,
##D   target_params = "b_arm2",
##D   p_sig_scs = 0.975,
##D   p_sig_ftl = 0.5,
##D   analysis_at = c(50, 100),
##D   interim_function = interim_futility_only(futility_threshold = 0.95)
##D )
## End(Not run)



cleanEx()
nameEx("interim_success_futility")
### * interim_success_futility

flush(stderr()); flush(stdout())

### Name: interim_success_futility
### Title: Interim Function Factory: Success and Futility Stopping
### Aliases: interim_success_futility

### ** Examples

## Not run: 
##D # Stop for overwhelming success or clear futility
##D design <- build_design(
##D   model = my_model,
##D   target_params = "b_arm2",
##D   p_sig_scs = 0.975,
##D   p_sig_ftl = 0.5,
##D   analysis_at = c(50, 100),
##D   interim_function = interim_success_futility(
##D     success_threshold = 0.995,
##D     futility_threshold = 0.90
##D   )
##D )
## End(Not run)



cleanEx()
nameEx("link")
### * link

flush(stderr()); flush(stdout())

### Name: link
### Title: Link Parameters for Co-variation
### Aliases: link

### ** Examples

## Not run: 
##D # Simple example: analysis_at varies with n_total
##D build_conditions(
##D   design = my_design,
##D   crossed = list(
##D     link(
##D       n_total = c(80, 160),
##D       analysis_at = list(c(40, 80), c(80, 160))
##D     ),
##D     b_arm_treat = c(0, 0.3)
##D   ),
##D   constant = list(thr_dec_eff = 0.975, thr_dec_fut = 0.5)
##D )
##D # Creates 4 conditions: 2 (n_total × analysis_at linked) × 2 b_arm_treat
## End(Not run)




cleanEx()
nameEx("list_models")
### * list_models

flush(stderr()); flush(stdout())

### Name: list_models
### Title: List Available Models
### Aliases: list_models

### ** Examples

## Not run: 
##D # List all models
##D list_models()
##D 
##D # List only BayesFlow models
##D list_models("bf")
## End(Not run)



cleanEx()
nameEx("load_bf_model")
### * load_bf_model

flush(stderr()); flush(stdout())

### Name: load_bf_model
### Title: Load Pre-trained BayesFlow Model
### Aliases: load_bf_model

### ** Examples

## Not run: 
##D # Load predefined BayesFlow model
##D bf_model <- load_bf_model("ancova_cont_2arms")
##D 
##D # Force re-download
##D bf_model <- load_bf_model("ancova_cont_2arms", force_download = TRUE)
## End(Not run)



cleanEx()
nameEx("load_brms_model")
### * load_brms_model

flush(stderr()); flush(stdout())

### Name: load_brms_model
### Title: Load Pre-compiled brms Model
### Aliases: load_brms_model

### ** Examples

## Not run: 
##D # Load predefined brms model
##D model <- load_brms_model("ancova_cont_2arms")
##D 
##D # Force re-download
##D model <- load_brms_model("ancova_cont_2arms", force_download = TRUE)
## End(Not run)



cleanEx()
nameEx("load_python_simulators")
### * load_python_simulators

flush(stderr()); flush(stdout())

### Name: load_python_simulators
### Title: Load Python Simulators Module
### Aliases: load_python_simulators

### ** Examples

## Not run: 
##D py_sims <- load_python_simulators()
##D data <- py_sims$simulate_ancova_2arms(
##D   n_sims = 64L,
##D   n_total = 100L,
##D   b_arm_treat = 0.5,
##D   b_covariate = 0.3
##D )
##D dim(data$outcome)  # [64, 100]
## End(Not run)



cleanEx()
nameEx("power_analysis")
### * power_analysis

flush(stderr()); flush(stdout())

### Name: power_analysis
### Title: Build Power Analysis Configuration
### Aliases: power_analysis

### ** Examples

## Not run: 
##D # Create conditions for power analysis
##D conditions <- build_conditions(design, n_total = c(100, 200))
##D 
##D # Basic power analysis (brms backend)
##D result <- power_analysis(conditions, n_sims = 100)
##D 
##D # With custom brms arguments
##D result <- power_analysis(
##D   conditions,
##D   n_sims = 1000,
##D   n_cores = 4,
##D   brms_args = list(chains = 4, iter = 2000)
##D )
##D 
##D # BayesFlow backend with custom settings
##D result <- power_analysis(
##D   conditions,
##D   n_sims = 1000,
##D   bf_args = list(n_posterior_samples = 2000, batch_size = 128)
##D )
##D 
##D # Create without running (for inspection)
##D config <- power_analysis(conditions, n_sims = 100, run = FALSE)
##D config <- run(config)  # Execute later
## End(Not run)



cleanEx()
nameEx("print.rctbp_conditions")
### * print.rctbp_conditions

flush(stderr()); flush(stdout())

### Name: print.rctbp_conditions
### Title: Print Method for rctbp_conditions Objects
### Aliases: print.rctbp_conditions

### ** Examples

## Not run: 
##D conditions <- build_conditions(design, condition_values, static_values)
##D print(conditions) # or just: conditions
## End(Not run)




cleanEx()
nameEx("report")
### * report

flush(stderr()); flush(stdout())

### Name: report
### Title: Generate Topic-Specific Reports
### Aliases: report

### ** Examples

## Not run: 
##D # Power metrics per condition
##D report(result, topic = "power")
##D 
##D # Early stopping summary per condition (sequential only)
##D report(result, topic = "stopping")
##D 
##D # Per-look stopping breakdown (sequential only)
##D report(result, topic = "stopping_by_look")
##D 
##D # Multiple topics - generates concatenated reports
##D report(result, topic = c("power", "stopping", "stopping_by_look"))
##D 
##D # Markdown format for Quarto integration
##D report(result, topic = "power", format = "markdown")
##D 
##D # Start headings at level 3 (###) for embedding in a document section
##D report(result, topic = "stopping", format = "markdown", heading_level = 3)
## End(Not run)



cleanEx()
nameEx("report_power")
### * report_power

flush(stderr()); flush(stdout())

### Name: report_power
### Title: Report on Power Metrics per Condition
### Aliases: report_power report_conditions

### ** Examples

## Not run: 
##D # Power metrics report
##D report_power(result)
##D 
##D # Markdown format for Quarto integration
##D report_power(result, format = "markdown")
## End(Not run)



cleanEx()
nameEx("report_stopping")
### * report_stopping

flush(stderr()); flush(stdout())

### Name: report_stopping
### Title: Report on Early Stopping Metrics per Condition
### Aliases: report_stopping report_early_stopping

### ** Examples

## Not run: 
##D # Early stopping summary per condition
##D report_stopping(result)
##D 
##D # Markdown format for Quarto integration
##D report_stopping(result, format = "markdown")
## End(Not run)



cleanEx()
nameEx("report_stopping_by_look")
### * report_stopping_by_look

flush(stderr()); flush(stdout())

### Name: report_stopping_by_look
### Title: Report on Early Stopping per Look and Condition
### Aliases: report_stopping_by_look

### ** Examples

## Not run: 
##D # Per-look stopping breakdown
##D report_stopping_by_look(result)
##D 
##D # Markdown format for Quarto integration
##D report_stopping_by_look(result, format = "markdown")
## End(Not run)



cleanEx()
nameEx("resummarize_boundaries")
### * resummarize_boundaries

flush(stderr()); flush(stdout())

### Name: resummarize_boundaries
### Title: Re-summarize Results with Different Boundaries
### Aliases: resummarize_boundaries

### ** Examples

## Not run: 
##D # Run simulation once
##D result <- power_analysis(conditions, n_sims = 500, analysis_at = c(0.5, 0.75))
##D 
##D # Re-analyze with O'Brien-Fleming-style boundaries
##D result_obf <- resummarize_boundaries(
##D   result,
##D   thr_dec_eff = boundary_obf(0.975),
##D   thr_dec_fut = boundary_linear(0.70, 0.90)
##D )
##D 
##D # Compare results
##D print(result)      # Original boundaries
##D print(result_obf)  # OBF boundaries
## End(Not run)



cleanEx()
nameEx("run")
### * run

flush(stderr()); flush(stdout())

### Name: run
### Title: Run Analysis Objects
### Aliases: run

### ** Examples

## Not run: 
##D # Create and run power analysis
##D power_config <- rctbp_power_analysis(conditions = conditions, n_sims = 100)
##D power_config <- run(power_config)
## End(Not run)



cleanEx()
nameEx("set_output_mode")
### * set_output_mode

flush(stderr()); flush(stdout())

### Name: set_output_mode
### Title: Set Output Mode
### Aliases: set_output_mode

### ** Examples

# Set to markdown mode
set_output_mode("markdown")

# Set back to CLI mode
set_output_mode("cli")




cleanEx()
nameEx("set_verbosity")
### * set_verbosity

flush(stderr()); flush(stdout())

### Name: set_verbosity
### Title: Set Verbosity Level
### Aliases: set_verbosity

### ** Examples

# Set to quiet mode
set_verbosity(0)

# Set to verbose mode
set_verbosity(2)

# Back to normal
set_verbosity(1)




cleanEx()
nameEx("setup_bf_python")
### * setup_bf_python

flush(stderr()); flush(stdout())

### Name: setup_bf_python
### Title: Set Up Python Environment for BayesFlow
### Aliases: setup_bf_python

### ** Examples

## Not run: 
##D # Auto-detect GPU and set up environment
##D setup_bf_python()
##D 
##D # Force CPU-only installation
##D setup_bf_python(cuda_version = "cpu")
##D 
##D # Use specific CUDA version
##D setup_bf_python(cuda_version = "12.6")
## End(Not run)



cleanEx()
nameEx("should_show")
### * should_show

flush(stderr()); flush(stdout())

### Name: should_show
### Title: Check if Message Should Be Displayed
### Aliases: should_show

### ** Examples

# Check if verbose messages should be shown
if (should_show(2)) {
  message("This is a debug message")
}




cleanEx()
nameEx("show_boundaries")
### * show_boundaries

flush(stderr()); flush(stdout())

### Name: show_boundaries
### Title: Show Available Boundary Functions
### Aliases: show_boundaries

### ** Examples

show_boundaries()




cleanEx()
nameEx("show_condition_args")
### * show_condition_args

flush(stderr()); flush(stdout())

### Name: show_condition_args
### Title: Show Required Arguments for build_conditions()
### Aliases: show_condition_args required_fn_args

### ** Examples

## Not run: 
##D # Show usage info
##D show_condition_args()
##D 
##D # Show required args for a design
##D design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
##D show_condition_args(design)
## End(Not run)




cleanEx()
nameEx("show_predefined_models")
### * show_predefined_models

flush(stderr()); flush(stdout())

### Name: show_predefined_models
### Title: Show Predefined Models
### Aliases: show_predefined_models list_predefined_models

### ** Examples

# List all predefined models
show_predefined_models()

# Filter to ANCOVA models
show_predefined_models("ancova")



cleanEx()
nameEx("show_target_params")
### * show_target_params

flush(stderr()); flush(stdout())

### Name: show_target_params
### Title: Show Available Target Parameters
### Aliases: show_target_params

### ** Examples

## Not run: 
##D # From a predefined model name
##D show_target_params("ancova_cont_2arms")
##D 
##D # From a design object
##D design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
##D show_target_params(design)
##D 
##D # From a brmsfit object
##D show_target_params(my_brmsfit)
## End(Not run)



cleanEx()
nameEx("simulate_data_ancova_cont_2arms_batch")
### * simulate_data_ancova_cont_2arms_batch

flush(stderr()); flush(stdout())

### Name: simulate_data_ancova_cont_2arms_batch
### Title: Simulate ANCOVA Data - Batched Format (2-arm)
### Aliases: simulate_data_ancova_cont_2arms_batch
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate 64 simulations with 100 subjects each
##D batch_data <- simulate_data_ancova_cont_2arms_batch(
##D   n_sims = 64,
##D   n_total = 100,
##D   b_arm_treat = 0.5,
##D   b_covariate = 0.3
##D )
##D dim(batch_data$outcome)  # [64, 100]
## End(Not run)



cleanEx()
nameEx("simulate_data_ancova_cont_3arms_batch")
### * simulate_data_ancova_cont_3arms_batch

flush(stderr()); flush(stdout())

### Name: simulate_data_ancova_cont_3arms_batch
### Title: Simulate ANCOVA Data - Batched Format (3-arm)
### Aliases: simulate_data_ancova_cont_3arms_batch
### Keywords: internal

### ** Examples

## Not run: 
##D # Generate 64 simulations with 150 subjects each
##D batch_data <- simulate_data_ancova_cont_3arms_batch(
##D   n_sims = 64,
##D   n_total = 150,
##D   b_arm_treat = c(0.3, 0.5),
##D   b_covariate = 0.3
##D )
##D dim(batch_data$outcome)  # [64, 150]
## End(Not run)



cleanEx()
nameEx("verify_bf_installation")
### * verify_bf_installation

flush(stderr()); flush(stdout())

### Name: verify_bf_installation
### Title: Verify BayesFlow Installation
### Aliases: verify_bf_installation

### ** Examples

## Not run: 
##D verify_bf_installation()
## End(Not run)



cleanEx()
nameEx("with_output_mode")
### * with_output_mode

flush(stderr()); flush(stdout())

### Name: with_output_mode
### Title: Temporarily Change Output Mode
### Aliases: with_output_mode

### ** Examples

## Not run: 
##D # Print in markdown mode temporarily
##D with_output_mode("markdown", {
##D   print(my_power_analysis)
##D })
## End(Not run)




cleanEx()
nameEx("with_verbosity")
### * with_verbosity

flush(stderr()); flush(stdout())

### Name: with_verbosity
### Title: Temporarily Change Verbosity Level
### Aliases: with_verbosity

### ** Examples

## Not run: 
##D # Run analysis in quiet mode temporarily
##D with_verbosity(0, {
##D   result <- power_analysis(conditions, n_sims = 100)
##D })
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

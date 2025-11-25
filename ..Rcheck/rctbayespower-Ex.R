pkgname <- "rctbayespower"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
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
nameEx("build_conditions")
### * build_conditions

flush(stderr()); flush(stdout())

### Name: build_conditions
### Title: Build Conditions for Power Analysis
### Aliases: build_conditions

### ** Examples

## Not run: 
##D # Create conditions for sample size and effect size analysis
##D conditions <- build_conditions(
##D   design = my_design,
##D   condition_values = list(
##D     n_total = c(100, 200, 300),
##D     effect_size = c(0.2, 0.5, 0.8)
##D   ),
##D   static_values = list(
##D     p_alloc = list(c(0.5, 0.5)),
##D     baseline_effect = 0.1
##D   )
##D )
##D 
##D # Print the conditions
##D print(conditions)
## End(Not run)




cleanEx()
nameEx("build_design")
### * build_design

flush(stderr()); flush(stdout())

### Name: build_design
### Title: Constructor for rctbp_design Objects
### Aliases: build_design

### ** Examples

## Not run: 
##D # Create an ANCOVA model
##D ancova_model <- build_model("ancova_cont_2arms")
##D 
##D # Simple design (no interim analysis)
##D simple_design <- build_design(
##D   model = ancova_model,
##D   target_params = "b_armtreat_1",
##D   p_sig_scs = 0.975,
##D   p_sig_ftl = 0.5
##D )
##D 
##D # Design with interim analysis
##D sequential_design <- build_design(
##D   model = ancova_model,
##D   target_params = "b_armtreat_1",
##D   p_sig_scs = 0.975,
##D   p_sig_ftl = 0.5,
##D   analysis_at = c(50, 100),
##D   interim_function = interim_futility_only(futility_threshold = 0.90)
##D )
## End(Not run)



cleanEx()
nameEx("build_model")
### * build_model

flush(stderr()); flush(stdout())

### Name: build_model
### Title: Create a Build Model Object
### Aliases: build_model

### ** Examples

## Not run: 
##D # Method 1: Use predefined model (recommended)
##D ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
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
nameEx("compare_power_methods")
### * compare_power_methods

flush(stderr()); flush(stdout())

### Name: compare_power_methods
### Title: Compare Power Calculation Methods
### Aliases: compare_power_methods

### ** Examples




cleanEx()
nameEx("export_report")
### * export_report

flush(stderr()); flush(stdout())

### Name: export_report
### Title: Export Report to File
### Aliases: export_report

### ** Examples

## Not run: 
##D model <- build_model("ancova_cont_2arms")
##D export_report(model, "model_report.md")
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
nameEx("get_output_mode")
### * get_output_mode

flush(stderr()); flush(stdout())

### Name: get_output_mode
### Title: Get Current Output Mode
### Aliases: get_output_mode

### ** Examples

get_output_mode()




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
##D   target_params = "b_armtreat_1",
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
##D   target_params = "b_armtreat_1",
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
##D   target_params = "b_armtreat_1",
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
nameEx("list_predefined_models")
### * list_predefined_models

flush(stderr()); flush(stdout())

### Name: list_predefined_models
### Title: List available predefined models
### Aliases: list_predefined_models

### ** Examples

# List all available predefined models
list_predefined_models()

# Filter for ANCOVA models only
list_predefined_models(filter_string = "ancova")

# Use discovered model with build_model()
available_models <- list_predefined_models()
if (length(available_models) > 0) {
  model <- build_model(predefined_model = available_models[1])
}




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
##D # Basic power analysis configuration
##D power_config <- build_power_analysis(conditions, n_sims = 100)
##D 
##D # Parallel execution with custom BRMS arguments
##D power_config <- build_power_analysis(
##D   conditions = conditions,
##D   n_sims = 1000,
##D   n_cores = 4,
##D   brms_args = list(chains = 4, iter = 2000)
##D )
##D 
##D # Execute the analysis
##D results <- run(power_config)
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
nameEx("required_fn_args")
### * required_fn_args

flush(stderr()); flush(stdout())

### Name: required_fn_args
### Title: Identify Required Parameters for Design or Model Objects
### Aliases: required_fn_args

### ** Examples

## Not run: 
##D required_fn_args(my_object)
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

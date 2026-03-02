## class 'model'

* dots for brms arguments like link functions for family
  -> implemented link\_sigma as argument for ancova
* print() should show default values for the data\_simulation\_fn as well as required arguments to specify

* allocation probs need to be disambiguated


## class 'conditions'

* ~~implement check that all required params are specified~~ (done)
* ~~no differentiation between condition\_values and static\_values~~ (done: grid shows only varying cols; fixed params listed below)
* ~~automatic detection of static values and varying parameters~~ (done: warns when crossed param has 1 level)
* ~~n\_total is always included in grid, even when unique(n\_total) == 1~~ (done: single-value columns excluded from grid display)
* inclusion of brms priors as varying parameters

  * parallelization of model compilation
  * compiled models in list are beeing expanded into conditions\*n\_sims list
  
  
# Power Analysis
* save full quantile profile for target parameter posterior to enable post-hoc exploration of alternative thresholds

* reports: report for convergence

# Across Package

* use cli "package" for outputs




# Bayesian Optimization

* after NPE implementation

* New class: rctbp_bayesian_optimization
  * is build on rctbp_conditions
  * creates ranges from min/max of condition values
  * takes a cost function as argument
* use package 'ml33mbo' as backend
  * Put mlr3mbo, bbotk, and paradox in the Suggests field of your DESCRIPTION file.
  * Write a high-level wrapper function (e.g., find_optimal_design()).
  * Inside that function, use rlang::check_installed("mlr3mbo") (a modern, cleaner version of requireNamespace) to prompt the user to install the missing packages if they try to run it.
  
  
# Major Package Development Decisions

- Consolidate API
  - minimal viable product for first release
  - placeholders for future extensions
  - naming conventions:
    - classes
    - arguments
    - pre-defined models
    
- Strategy for rolling out first package version
  - Repo managment
  - which pre-defined models
  
  
  
  
# Sample Accrual (see [25\_sample\_accrual\_plan.md](25_sample_accrual_plan.md))

* Phase 1 (MVP): enrollment time generation, calendar-time subsetting, trial duration metrics
  * New file `R/accrual.R`: `generate_enrollment_times()`, `patients_with_data()`, `calendar_to_available_n()`
  * Modify `R/class_conditions.R`: accrual parameter defaults, validation, calendar-time conversion
  * Modify `R/required_fn_args.R`: show accrual params in `show_condition_args()`
  * Modify `R/worker_functions.R`: post-process sim data with enrollment times
  * Modify `R/backend_brms.R` + `R/backend_bf.R`: accrual-aware subsetting
  * Modify `R/compute_measures.R`: trial duration metrics
* Phase 2: reporting & accrual plot type
* Phase 3: dropout / loss-to-follow-up
* Phase 4: survival/event-driven integration (dual routing for `accrual_rate`)


  # API
  
  
  
  
    
    
    
    
    
    
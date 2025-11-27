## class 'model'

* dots for brms arguments like link functions for family
  -> implemented link\_sigma as argument for ancova
* print() should show default values for the data\_simulation\_fn as well as required arguments to specify


## class 'conditions'

* implement check that all required params are specified
* no differentiation between condition\_values and static\_values
* automatic detection of static values and varying parameters
* n\_total is always included in grid, even when unique(n\_total) == 1
* inclusion of brms priors as varying parameters

  * parallelization of model compilation
  * compiled models in list are beeing expanded into conditions\*n\_sims list
  
  
# Power Analysis
* save full quantile profile for target parameter posterior to enable post-hoc exploration of alternative thresholds

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
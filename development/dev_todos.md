
## class model
- dots for brms arguments like link functions for family 
  -> implemented link_sigma as argument for ancova
- print() should show default values for the data_simulation_fn as well as required arguments to specify


## promotion of classes to top level
- model in conditions


## class conditions
- implement check that all required params are specified
- no differentiation between condition_values and static_values
- automatic detection of static values and varying parameters
- n_total is always included in grid, even when unique(n_total) == 1
- inclusion of brms priors as varying parameters
  - parallelization of model compilation
  - compiled models in list are beeing expanded into conditions*n_sims list
  
  
## plots
- plotly integration
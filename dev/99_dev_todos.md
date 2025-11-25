## class model

* dots for brms arguments like link functions for family
  -> implemented link\_sigma as argument for ancova
* print() should show default values for the data\_simulation\_fn as well as required arguments to specify



## promotion of classes to top level

* model in conditions



## class conditions

* implement check that all required params are specified
* no differentiation between condition\_values and static\_values
* automatic detection of static values and varying parameters
* n\_total is always included in grid, even when unique(n\_total) == 1
* inclusion of brms priors as varying parameters

  * parallelization of model compilation
  * compiled models in list are beeing expanded into conditions\*n\_sims list



## plots

* plotly integration





\# Across Package

* use cli "package" for outputs

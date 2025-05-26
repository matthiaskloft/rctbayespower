# Package Build and Check Script for rctbayespower

# This script provides instructions for building and checking the rctbayespower package
# Run these commands in R or RStudio

# 1. Install required packages for package development
# install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))

# 2. Load development tools
# library(devtools)
# library(roxygen2)

# 3. Set working directory to package root
# setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")

# 4. Generate documentation from roxygen2 comments
# roxygenise()

# 5. Install package dependencies
# install.packages(c("brms", "rstanarm", "bayestestR", "posterior", 
#                   "ggplot2", "dplyr", "tidyr", "purrr", "magrittr", 
#                   "tibble", "rlang"))

# 6. Install the package locally
# install(".", dependencies = TRUE)

# 7. Load the package
# library(rctbayespower)

# 8. Run package checks
# check()

# 9. Build the package
# build()

# 10. Run tests
# test()

# 11. Build vignettes
# build_vignettes()

# Example usage after installation:
# library(rctbayespower)
# 
# # Simple power analysis
# result <- power_analysis(
#   n_control = 50,
#   n_treatment = 50,
#   effect_size = 0.5,
#   outcome_type = "continuous",
#   n_simulations = 100
# )
# 
# # Plot results
# plot_power_curve(result)

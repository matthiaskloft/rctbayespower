# Quick CRAN Check for rctbayespower
cat("Running CRAN checks for rctbayespower...\n")

# Install devtools if needed
if(!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Set working directory
setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")

# Basic checks
cat("1. Loading package...\n")
devtools::load_all(".")

cat("2. Checking documentation...\n")
devtools::check_man()

cat("3. Running R CMD check...\n")
check_result <- devtools::check(
  pkg = ".",
  args = "--as-cran",
  quiet = FALSE
)

cat("Check completed!\n")
print(check_result)

#!/usr/bin/env Rscript

# Script to regenerate documentation after fixing LaTeX/math formatting issues
# This must be run from Windows R where the dependencies are installed

cat("==========================================================\n")
cat("Regenerating package documentation...\n")
cat("==========================================================\n\n")

# Check if roxygen2 is available
if (requireNamespace("roxygen2", quietly = TRUE)) {
  cat("Using roxygen2::roxygenise()...\n")
  roxygen2::roxygenise()
  cat("\n*** Documentation regenerated successfully! ***\n\n")
} else if (requireNamespace("devtools", quietly = TRUE)) {
  cat("Using devtools::document()...\n")
  devtools::document()
  cat("\n*** Documentation regenerated successfully! ***\n\n")
} else {
  cat("ERROR: Neither roxygen2 nor devtools is installed.\n")
  cat("Please run: install.packages(c('roxygen2', 'devtools'))\n")
  quit(status = 1)
}

cat("==========================================================\n")
cat("Next steps:\n")
cat("1. Review the regenerated .Rd files in man/ directory\n")
cat("2. Run: devtools::check(remote = TRUE, manual = TRUE)\n")
cat("3. Verify the PDF manual builds without LaTeX errors\n")
cat("==========================================================\n")

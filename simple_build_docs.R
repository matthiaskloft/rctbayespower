# Simple Documentation Builder for rctbayespower
# Run this in R console or RStudio

# Set working directory (adjust if needed)
setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")

cat("🔧 Building rctbayespower Documentation\n")
cat("=====================================\n\n")

# Check if required packages are installed
required_pkgs <- c("devtools", "roxygen2", "knitr", "rmarkdown")

missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[,"Package"]]
if(length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs)
}

# Load required libraries
suppressMessages({
  library(devtools)
  library(roxygen2)
})

cat("📚 Generating roxygen2 documentation...\n")
try({
  roxygen2::roxygenise(package.dir = ".")
  cat("✅ Documentation generated\n")
}, silent = FALSE)

cat("\n📖 Building vignettes...\n")
if(dir.exists("vignettes")) {
  try({
    devtools::build_vignettes()
    cat("✅ Vignettes built\n")
  }, silent = FALSE)
} else {
  cat("No vignettes directory found\n")
}

cat("\n🔍 Checking documentation files...\n")
if(dir.exists("man")) {
  rd_files <- list.files("man", pattern = "\\.Rd$")
  cat("Found", length(rd_files), "documentation files:\n")
  for(f in rd_files) cat("  -", f, "\n")
}

cat("\n✅ Documentation build complete!\n")
cat("Check the 'man/' directory for .Rd files\n")
cat("Check the 'doc/' directory for built vignettes\n")

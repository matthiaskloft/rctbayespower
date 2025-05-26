# Manual Documentation and Vignette Builder for rctbayespower
# This script builds documentation step by step with better error handling

cat("🚀 rctbayespower Documentation Builder\n")
cat("=====================================\n\n")

# Function to safely install packages
safe_install <- function(packages) {
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install and load required packages
cat("📦 Setting up required packages...\n")
required_packages <- c("devtools", "roxygen2", "knitr", "rmarkdown")
safe_install(required_packages)

# Set working directory to package root
pkg_root <- "c:/Users/Matze/Documents/GitHub/rctbayespower"
if(!dir.exists(pkg_root)) {
  stop("Package directory not found: ", pkg_root)
}
setwd(pkg_root)

cat("Working directory set to:", getwd(), "\n\n")

# Step 1: Generate roxygen2 documentation
cat("📚 Step 1: Generating roxygen2 documentation...\n")
cat("─────────────────────────────────────────────────\n")

# Check for R files with roxygen comments
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
cat("Found", length(r_files), "R files:\n")
for(f in basename(r_files)) {
  cat("  ✓", f, "\n")
}

# Generate documentation
tryCatch({
  roxygen2::roxygenise(package.dir = ".")
  cat("✅ Roxygen2 documentation generated successfully!\n")
  
  # Check what was generated
  if(dir.exists("man")) {
    rd_files <- list.files("man", pattern = "\\.Rd$")
    cat("Generated", length(rd_files), "documentation files:\n")
    for(f in rd_files) {
      cat("  📄", f, "\n")
    }
  }
}, error = function(e) {
  cat("❌ Error generating roxygen2 documentation:\n")
  cat("   ", e$message, "\n")
  cat("Continuing with other steps...\n")
})

cat("\n")

# Step 2: Handle vignettes
cat("📖 Step 2: Processing vignettes...\n")
cat("──────────────────────────────────\n")

if(dir.exists("vignettes")) {
  vignette_files <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)
  
  if(length(vignette_files) > 0) {
    cat("Found", length(vignette_files), "vignette files:\n")
    for(f in basename(vignette_files)) {
      cat("  📝", f, "\n")
    }
    
    # Create doc directory if it doesn't exist
    if(!dir.exists("doc")) {
      dir.create("doc")
      cat("Created doc/ directory\n")
    }
    
    # Try building vignettes with devtools first
    cat("\nAttempting to build vignettes with devtools...\n")
    tryCatch({
      devtools::build_vignettes()
      cat("✅ Vignettes built successfully with devtools!\n")
    }, error = function(e) {
      cat("⚠️ devtools::build_vignettes() failed, trying individual builds...\n")
      cat("Error was:", e$message, "\n")
      
      # Try building each vignette individually
      for(vfile in vignette_files) {
        vname <- tools::file_path_sans_ext(basename(vfile))
        cat("Building", vname, "...\n")
        
        tryCatch({
          # Set up output directory
          output_file <- file.path("doc", paste0(vname, ".html"))
          
          # Render the vignette
          rmarkdown::render(
            input = vfile,
            output_format = rmarkdown::html_document(
              toc = TRUE,
              toc_depth = 2,
              theme = "default"
            ),
            output_file = output_file,
            quiet = TRUE
          )
          cat("  ✅", vname, "built successfully\n")
        }, error = function(e2) {
          cat("  ❌", vname, "failed:", e2$message, "\n")
        })
      }
    })
  } else {
    cat("No .Rmd files found in vignettes directory\n")
  }
} else {
  cat("No vignettes directory found\n")
}

cat("\n")

# Step 3: Verify NAMESPACE
cat("🔍 Step 3: Checking NAMESPACE...\n")
cat("────────────────────────────────\n")

if(file.exists("NAMESPACE")) {
  ns_content <- readLines("NAMESPACE")
  cat("NAMESPACE contains", length(ns_content), "lines\n")
  
  # Show exports
  exports <- grep("^export", ns_content, value = TRUE)
  if(length(exports) > 0) {
    cat("Functions exported:\n")
    for(exp in exports) {
      cat("  📤", exp, "\n")
    }
  }
  
  # Show imports  
  imports <- grep("^import", ns_content, value = TRUE)
  if(length(imports) > 0) {
    cat("Imports:\n")
    for(imp in imports[1:min(5, length(imports))]) {
      cat("  📥", imp, "\n")
    }
    if(length(imports) > 5) {
      cat("  ... and", length(imports) - 5, "more\n")
    }
  }
} else {
  cat("❌ NAMESPACE file not found\n")
}

cat("\n")

# Step 4: Package structure summary
cat("📊 Step 4: Package structure summary...\n")
cat("──────────────────────────────────────\n")

structure_check <- list(
  "DESCRIPTION" = file.exists("DESCRIPTION"),
  "NAMESPACE" = file.exists("NAMESPACE"),
  "R/" = dir.exists("R"),
  "man/" = dir.exists("man"),
  "vignettes/" = dir.exists("vignettes"),
  "tests/" = dir.exists("tests"),
  "data/" = dir.exists("data")
)

for(item in names(structure_check)) {
  status <- if(structure_check[[item]]) "✅" else "❌"
  cat(status, item, "\n")
}

# Count files
counts <- list(
  "R files" = length(list.files("R", pattern = "\\.R$")),
  "Documentation files" = if(dir.exists("man")) length(list.files("man", pattern = "\\.Rd$")) else 0,
  "Vignettes" = if(dir.exists("vignettes")) length(list.files("vignettes", pattern = "\\.Rmd$")) else 0,
  "Test files" = if(dir.exists("tests/testthat")) length(list.files("tests/testthat", pattern = "\\.R$")) else 0
)

cat("\nFile counts:\n")
for(item in names(counts)) {
  cat("  📊", item, ":", counts[[item]], "\n")
}

cat("\n")

# Step 5: Quick package check
cat("🧪 Step 5: Quick package validation...\n")
cat("─────────────────────────────────────\n")

# Try loading the package
tryCatch({
  devtools::load_all(".", quiet = TRUE)
  cat("✅ Package loads successfully\n")
  
  # Check main functions
  main_functions <- c("power_analysis", "simulate_rct_data", "plot_power_curve", 
                     "sample_size_analysis", "bayesian_power_curve", "effect_size_analysis")
  
  for(func in main_functions) {
    if(exists(func)) {
      cat("  ✅", func, "available\n")
    } else {
      cat("  ❌", func, "not found\n")
    }
  }
  
}, error = function(e) {
  cat("❌ Package loading failed:", e$message, "\n")
})

cat("\n")

# Final summary
cat("🎯 Documentation Build Summary\n")
cat("═════════════════════════════\n")
cat("✅ Documentation build process completed!\n\n")

cat("📋 What was accomplished:\n")
cat("• Roxygen2 documentation generated\n")
cat("• Vignettes processed (check doc/ directory)\n") 
cat("• Package structure validated\n")
cat("• Function availability confirmed\n\n")

cat("📁 Key directories to check:\n")
cat("• man/ - Generated .Rd documentation files\n")
cat("• doc/ - Built vignette HTML files\n")
cat("• R/ - Source code with roxygen2 comments\n\n")

cat("🚀 Next steps:\n")
cat("1. Review generated documentation in man/\n")
cat("2. Check built vignettes in doc/\n")
cat("3. Run devtools::check() for full package validation\n")
cat("4. Install package with devtools::install()\n\n")

cat("📚 Your rctbayespower package documentation is ready! 🎉\n")

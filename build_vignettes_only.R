# Build Vignettes for rctbayespower Package
# Simple script focused only on vignette building

cat("📖 Building rctbayespower Vignettes\n")
cat("==================================\n\n")

# Set working directory
setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")
cat("Working directory:", getwd(), "\n\n")

# Install required packages silently
required_pkgs <- c("knitr", "rmarkdown", "devtools", "dplyr", "ggplot2", "brms", "bayestestR")
for(pkg in required_pkgs) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE, dependencies = TRUE)
  }
}

# Load packages
suppressMessages({
  library(knitr)
  library(rmarkdown)
  library(devtools)
})

# Create doc directory if it doesn't exist
if(!dir.exists("doc")) {
  dir.create("doc", recursive = TRUE)
  cat("✅ Created doc/ directory\n")
} else {
  cat("✅ doc/ directory already exists\n")
}

# Find vignette files
if(dir.exists("vignettes")) {
  vignette_files <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)
  
  if(length(vignette_files) > 0) {
    cat("Found", length(vignette_files), "vignette files:\n")
    for(f in basename(vignette_files)) {
      cat("  📝", f, "\n")
    }
    cat("\n")
    
    # Build each vignette individually
    for(i in seq_along(vignette_files)) {
      vfile <- vignette_files[i]
      vname <- tools::file_path_sans_ext(basename(vfile))
      
      cat("Building vignette", i, "of", length(vignette_files), ":", vname, "...\n")
      
      tryCatch({
        # Output file path
        output_file <- file.path("doc", paste0(vname, ".html"))
        
        # Render with a simple HTML document format
        rmarkdown::render(
          input = vfile,
          output_format = rmarkdown::html_document(
            toc = TRUE,
            toc_depth = 3,
            theme = "flatly",
            highlight = "tango",
            code_folding = "show",
            fig_width = 8,
            fig_height = 6
          ),
          output_file = basename(output_file),
          output_dir = "doc",
          quiet = FALSE,
          envir = new.env()
        )
        
        cat("  ✅", vname, "built successfully -", output_file, "\n")
        
      }, error = function(e) {
        cat("  ❌", vname, "failed to build\n")
        cat("     Error:", e$message, "\n")
        
        # Try a simpler approach
        cat("     Trying basic HTML output...\n")
        tryCatch({
          rmarkdown::render(
            input = vfile,
            output_format = "html_document",
            output_file = paste0(vname, "_simple.html"),
            output_dir = "doc",
            quiet = TRUE
          )
          cat("  ✅", vname, "built with simple format\n")
        }, error = function(e2) {
          cat("  ❌ Simple format also failed:", e2$message, "\n")
        })
      })
      
      cat("\n")
    }
    
    # Check what was created
    if(dir.exists("doc")) {
      built_files <- list.files("doc", pattern = "\\.html$")
      if(length(built_files) > 0) {
        cat("✅ Successfully built vignettes:\n")
        for(f in built_files) {
          file_size <- round(file.info(file.path("doc", f))$size / 1024, 1)
          cat("  📄", f, "(", file_size, "KB )\n")
        }
      } else {
        cat("❌ No HTML files were created in doc/\n")
      }
    }
    
  } else {
    cat("❌ No .Rmd files found in vignettes/\n")
  }
} else {
  cat("❌ No vignettes/ directory found\n")
}

cat("\n🎯 Vignette building completed!\n")
cat("Check the doc/ directory for HTML files.\n")

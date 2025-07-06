#!/usr/bin/env Rscript
library(rmarkdown)

rmf <- function(f)
{
  if (file.exists(f))
    file.remove(f)
}

clean <- function()
{
  files = dir(pattern = "*.Rmd", recursive = FALSE)
  for (f in files)
    rmf(f)
}


build_vignette <- function(f)
{
  f_Rmd <- basename(f)
  of <- sub(f_Rmd, pattern = "^_", replacement = "")
  rmf(of)

  fmt <- rmarkdown::md_document(variant = "gfm",
                                preserve_yaml = TRUE,
                                ext = ".Rmd")

  rmarkdown::render(
    f,
    output_file = of,
    output_dir = getwd(),
    output_format = fmt
  )

  invisible(TRUE)
}



# ------------------------------------------------------------------------------

clean()

build_vignette("./src/_01-introduction.Rmd")

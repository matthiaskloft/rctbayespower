# load package
pkgload::load_all()

# ANCOVA
ancova_cont_2arms <- build_model_ancova_cont_2arms()
ancova_cont_3arms <- build_model_ancova_cont_3arms()



# Save the models to R/sysdata.rda
usethis::use_data(ancova_cont_2arms,
                  ancova_cont_3arms,
                  internal = TRUE,
                  overwrite = TRUE)

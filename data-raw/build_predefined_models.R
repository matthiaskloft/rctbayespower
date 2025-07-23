# ANCOVA
ancova_cont_2arms <- build_model(predefined_model = "ancova_cont_2arms")
ancova_cont_3arms <- build_model(predefined_model = "ancova_cont_3arms")



usethis::use_data(ancova_cont_2arms, ancova_cont_3arms, internal = TRUE, overwrite = TRUE)

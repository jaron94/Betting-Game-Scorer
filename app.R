# Launch the ShinyApp (Do not remove this comment)

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(golem.app.prod = TRUE)
Sys.setenv(R_CONFIG_ACTIVE = "production")
bgScorer::run_app()

# Launch the ShinyApp (Do not remove this comment)

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
bgScorer::run_app()

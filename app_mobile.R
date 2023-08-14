# Launch the ShinyApp (Do not remove this comment)

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
options(bgScorer.mobile = TRUE)
run_app()

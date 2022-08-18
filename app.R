# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')
bgScorer::run_app() # add parameters here (if any)

# Launch the ShinyApp (Do not remove this comment)
options("golem.app.prod" = TRUE)
# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')
Sys.setenv(R_CONFIG_ACTIVE = "production")
bgScorer::run_app()

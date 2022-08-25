# Launch the ShinyApp (Do not remove this comment)

options( "golem.app.prod" = TRUE)
# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')
options(shiny.testmode = TRUE)
bgScorer::run_app()
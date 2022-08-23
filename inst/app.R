options( "golem.app.prod" = TRUE)
# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')
bgScorer::run_app() # add parameters here (if any)

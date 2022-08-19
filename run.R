library(shiny)
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

port <- Sys.getenv('PORT')
options( "golem.app.prod" = TRUE)
# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')
bgScorer::run_app(host = '0.0.0.0',
                  port = as.numeric(port)) # add parameters here (if any)

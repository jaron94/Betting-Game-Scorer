# Launch the ShinyApp (Do not remove this comment)

options(golem.app.prod = TRUE)
bgScorer::run_app(options = list(test.mode = TRUE))

# Optionally use RSPM to use Linux binaries
if (Sys.getenv("USE_RSPM") == "TRUE") {
  options(repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest"))
  options(renv.config.repos.override = getOption("repos"))
}

source("renv/activate.R")

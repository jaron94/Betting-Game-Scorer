# Optionally use RSPM to use Linux binaries
if (Sys.info()["sysname"] == "Linux") {
  options(repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest"))
  options(renv.config.repos.override = getOption("repos"))
}

source("renv/activate.R")

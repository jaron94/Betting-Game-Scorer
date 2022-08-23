# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
attachment::att_amend_desc(extra.suggests = c("covr", "pkgload"))

renv::settings$snapshot.type("explicit")
renv::snapshot()


## Add modules ----
## Create a module infrastructure in R/
# golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")

# Documentation

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

## CI ----

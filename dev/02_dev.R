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
## Add one line by package you want to add as dependency
usethis::use_package( "shiny" )     # Required to create an R Shiny app
usethis::use_package( "forestplot" ) # For the creation of the forest plot
usethis::use_package( "shinythemes" ) # shiny themes to change the overall appearance of the app
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "DT" )
usethis::use_package( "collapsibleTree" )
usethis::use_package( "r2d3" )
usethis::use_package( "shinyjs" )
usethis::use_package( "colourpicker" )
usethis::use_package( "Cairo" )
usethis::use_package( "magrittr")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "value_tree" ) # ran (don't re-run)
golem::add_module( name = "about_breasy" ) # ran (don't re-run)
golem::add_module( name = "data_manual" ) # ran (don't re-run)
#golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "breasy_forestplot" ) # don't re-run
golem::add_utils( "helpers" ) # don't re-run

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
#usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
#usethis::use_test( "app" )

# Documentation

## Vignette ----
# usethis::use_vignette("breasy")
# devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")


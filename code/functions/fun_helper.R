# Functions - common functions

save_and_run <- function(){
  rstudioapi::documentSaveAll()
  source('global.R')
  shiny::runApp(app_directory)
}

num_from_fac <- function(fac) as.numeric(levels(fac))[fac]
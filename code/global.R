  ####--GLOBAL--------------------------------------------------------------------------------------------
  
  #  
  # This file loads automatically before ui.R and server.R 
  # Load packages and source files used in the app 
  #
  
  library(shiny)
  library(shinySignals)   # devtools::install_github("hadley/shinySignals")
  library(dplyr)
  library(shinydashboard)
  library(bubbles)        # devtools::install_github("jcheng5/bubbles")
  
  my_packages <- c('tidyverse','shiny', 'shinycssloaders', 
                   'shinydashboard', 'shinydashboardPlus', 'shinyjs', 'shinyWidgets', 
                   'DT', 'dygraphs', 'gridExtra', 'RColorBrewer',
                   'leaflet', 'leaflet.extras', 'tmaptools',
                   'tibbletime', 'data.table', 
                   'broom', 'purrr', 'scales', 'lubridate', 'janitor',    
                   'readxl', 'glue', 'dotenv', 'auth0','httr',
                   'shinyjqui','shinyAce','styler', 'shinyEffects'
                   ,'summarytools' 
                  )
  invisible(lapply(my_packages, library, character.only = TRUE))
  
  # Global constants
  
  enable_oauth_dev = FALSE # Set to FALSE to disable oAuth during development
  app_directory <- './'
  project_name <- 'Suricata Dashboard'
  
  iplookup_db_file <- '../data/IP2LOCATION-LITE-DB9.BIN'
  tmp_json_file <- '../data/tmp.json'
  
  if(!file.exists(iplookup_db_file))
    print("could no locate ip lookup DB file")
  
  # Populate envrionment variables (used during development only)
  
  environment_file <- file.path('..','.env')
  
  if(file.exists(environment_file)){
    print(paste('loading from local env file:',environment_file ))
    load_dot_env(environment_file)
  }
  
  # Global constants loaded from envrionment variables or default value
  
  shiny_port <- as.integer(if(nchar(Sys.getenv("PORT"))> 0){ Sys.getenv("PORT")}else{ '3838' })
  shiny_app_init_timeout <- as.integer(if(nchar(Sys.getenv("APP_INIT_TIMEOUT"))> 0){ Sys.getenv("APP_INIT_TIMEOUT")}else{ '60' })
  shiny_app_idle_timeout <- as.integer(if(nchar(Sys.getenv("APP_IDLE_TIMEOUT"))> 0){ Sys.getenv("APP_IDLE_TIMEOUT")}else{ '5' })
  print(paste('shiny app_init_timeout:', shiny_app_init_timeout, 'shiny app_idle_timeout:',  shiny_app_idle_timeout ))
  
  
  # Make sure to source function and ui files here or they won't be used by the app
  # Source server files within server.R 
  
  source('functions/fun_features.R')
  source('functions/fun_mutate.R')
  source('functions/fun_helper.R')
  source('functions/fun_io.R')
  source('functions/fun_plots.R')
  source('functions/fun_sql.R')
  source('functions/fun_auth0.R')
  
  source('ui/ui_dashboards.R')
  source('ui/ui_alerts_map.R')
  print(paste("global.R finished"))
  

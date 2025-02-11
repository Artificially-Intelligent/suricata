  ####--GLOBAL--------------------------------------------------------------------------------------------
  
  #  
  # This file loads automatically before ui.R and server.R 
  # Load packages and source files used in the app 
  #
  
  library(redux)
  library(shiny)
  library(dplyr)
  library(shinydashboard)
  library(plotly)
  library(waiter)
  library(tidyjson)
  library(hrbrthemes)
  library(jsonlite)
  
  library(bubbles)        # remotes::install_github("jcheng5/bubbles")
  library(shinySignals)   # remotes::install_github("hadley/shinySignals")
  
  
  library(shiny)
  library(shinycssloaders)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(shinyWidgets)
  library(DT)
  library(dygraphs)
  library(gridExtra)
  library(RColorBrewer)
  library(leaflet)
  library(leaflet.extras)
  library(tibbletime)
  library(data.table)
  library(purrr)
  library(scales)
  library(lubridate)
  library(janitor)
  
  library(glue)
  library(dotenv)
  library(auth0)
  library(httr)
  library(shinyjqui)
  library(shinyAce)
  library(styler)
  library(shinyEffects)
  
  # my_packages <- c('tidyverse','shiny', 'shinycssloaders', 
  #                  'shinydashboard', 'shinydashboardPlus', 'shinyjs', 'shinyWidgets', 
  #                  'DT', 'dygraphs', 'gridExtra', 'RColorBrewer',
  #                  'leaflet', 'leaflet.extras', 'tmaptools',
  #                  'tibbletime', 'data.table', 
  #                  'broom', 'purrr', 'scales', 'lubridate', 'janitor',    
  #                  'readxl', 'glue', 'dotenv', 'auth0','httr',
  #                  'shinyjqui','shinyAce','styler', 'shinyEffects'
  #                  ,'summarytools' 
  #                 )
  # invisible(lapply(my_packages, library, character.only = TRUE))
  
  # Global constants
  
  enable_oauth_dev = FALSE # Set to FALSE to disable oAuth during development
  app_directory <- './'
  project_name <- 'Suricata Events GUI'
  
  max_history_load_size <- 20000
  initial_history_load_size <- 100
  default_load_size <- 5000
  
  
  #filter data out if older than max_age_minutes
  max_age_minutes <- 60 * 1
  
  iplookup_db_file <- '../data/IP2LOCATION-LITE-DB9.BIN'
  tmp_json_file <- '../data/tmp.json'
  event_types <- c('alert',
              'drop',
              'flow',
              'netflow',
              'dns',
              'fileinfo',
              'http',
              'tls')
  
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
  redis_host <- if(nchar(Sys.getenv("REDIS_HOST"))> 0){ Sys.getenv("REDIS_HOST")}else{ 'localhost' }
  redis_key  <- if(nchar(Sys.getenv("REDIS_KEY"))> 0){ Sys.getenv("REDIS_KEY")}else{ 'suricata' }
  
  
  # Make sure to source function and ui files here or they won't be used by the app
  # Source server files within server.R 
  
  source('functions/fun_features.R')
  source('functions/fun_mutate.R')
  source('functions/fun_helper.R')
  source('functions/fun_io.R')
  source('functions/fun_plots.R')
  source('functions/fun_sql.R')
  source('functions/fun_auth0.R')
  
  source('ui/ui_event.R')
  # source('ui/ui_all.R')
  # source('ui/ui_http.R')
  # source('ui/ui_flow.R')
  # source('ui/ui_netflow.R')
  # source('ui/ui_alert.R')
  print(paste("global.R finished"))
  

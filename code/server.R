
####--SERVER------------------------------------------------------------------------------------------------
# Shiny server options
options(shiny.port = shiny_port
        ,auth0_disable = (! using_auth)
        ,warn = -1)

auth0_server(function(input, output, session, options) {
  
  
  default_tab = "http_dash"
  current_version = "0.01"
  
  ####--UI BLOCK----------------------------------------------------------------------------------------------
  
  
  print(paste("rewriting search string to: ", '?authentcated'))
  updateQueryString('?authenticated')
  
  output$app_version <- renderUI({
    fluidRow(
      column(12, offset = 1, 
             br(),
             h5(str_c("Version ", current_version)),
             h6(project_name)
      ),
      column(width = 4,
             fluidRow(
               textInput(
                 inputId = "pulse_icon_text",
                 label = "Enter address"
               ),
               textOutput("pulse_icon_message")
             )
      ),
      column(width = 2,
             actionBttn(
               inputId = "pulse_icon_button",
               label = "Go",
               style = "gradient",
               color = "primary"
             )
      )
    )
  })
  
  output$ui_sidebar <- renderUI({
    dashboardSidebar(
      shinyWidgets::sliderTextInput("data_refresh_rate","Data refresh rate (seconds)",
                     choices=c(0, 1, 3, 5, 10, 30, 60, 120, 180,300,600,900,1800,3600,86400,"disabled"),
                     selected=5, grid = T),
      # sliderInput("data_refresh_rate", "Data refresh rate (s)",
      #             min = 0, max = 3600, value = 10, step = 1
      # ),
      sidebarMenu(id = "tabs", 
        menuItem("All Events",icon = icon("cube"),
                 menuSubItem("Event Dashboard", 
                             tabName = "all_dash", 
                             selected = TRUE,
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Event Detail", 
                             tabName = "all_table", 
                             icon = icon("table")
                 )         
        ),
        menuItem("Http",icon = icon("cube"),
                 menuSubItem("Http Dashboard", 
                             tabName = "http_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Http Map", 
                             tabName = "http_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("Http Detail", 
                             tabName = "http_table", 
                             icon = icon("table")
                 )         
        ),
        menuItem("Flow",icon = icon("code-branch"),
                 menuSubItem("Flow Dashboard", 
                             tabName = "flow_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Flow App Traffic", 
                             tabName = "flow_app_traffic", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Flow Map", 
                             tabName = "flow_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("Flow Detail", 
                             tabName = "flow_table", 
                             icon = icon("table")
                 ) 
        ),
        menuItem("NetFlow",icon = icon("code-branch"),
                 menuSubItem("NetFlow Dashboard", 
                             tabName = "netflow_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("NetFlow Map", 
                             tabName = "netflow_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("NetFlow Detail", 
                             tabName = "netflow_table", 
                             icon = icon("table")
                 ) 
        ),
        menuItem("Traffic Alerts",icon = icon("exclamation-triangle"),
                 menuSubItem("Alert Dashboard", 
                             tabName = "alert_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Alert Map", 
                             tabName = "alert_map", 
                             icon = icon("globe")
                 )
        ),
        menuItem("Traffic Drops",icon = icon("bomb"),
                 menuSubItem("Drop Dashboard", 
                             tabName = "drop_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Drop Map", 
                             tabName = "drop_map", 
                             icon = icon("globe")
                 )
        )
      )
    )
  })
  
  #  output$ui_rightsidebar <- renderUI({
  #    rightSidebar(
  #      rightSidebarTabContent(
  #        id = 1,
  #        title = "User Info",
  #        icon = "user"
  #      )
  #    )
  #  })
  
  output$ui_body <- renderUI({
    #updateTabItems()
    #updateTabsetPanel(session, "tabs", selected = "http_dash")
    # tabsetPanel(id = "tabset",
    #             type = "tabs", 
    tabItems(
      tabItem_all_dashboard,
      tabItem_all_table,
      
      tabItem_http_dashboard,
      tabItem_http_map,
      tabItem_http_table,
      tabItem_flow_dashboard,
      tabItem_flow_app_traffic,
      tabItem_flow_map,
      tabItem_flow_table,
      tabItem_netflow_dashboard,
      tabItem_netflow_map,
      tabItem_netflow_table
    )
  })
  
  
  
  ####--SERVER BLOCK-----------------------------------------------------------------------------------------
  
  ## Constants
  
  # show_waiter(spin_fading_circles())
  
  v <- reactiveValues(
    selected_tab = default_tab,
    pulse_icon_message = "",
    map_shape_id = NULL,
    selected_alert_id = NULL
  )
  
  data <- reactiveValues(
  )
  
  u <- reactiveValues(
    # User settings to be saved between logins
    user_id =  "",
    last_lng = 132.84667,
    last_lat = -27.21555,
    last_zoom = 4
    #  ,
    #  export_mode = FALSE,
    #  edit_region = FALSE,
    #  view_overlays = FALSE,
    #  map_overlay = "",
    #  filtered_listings_min = 0,
    #  filtered_listings_max = 0,
    #  agent_region = "580a000000030003060100030500000000055554462d3800000010000000010004000900000000" %>% text_to_spatial()
  )
  
  
  # Server modules 
  source('server/s_data.R', local = TRUE)
  
  source('server/s_all.R', local = TRUE)
  source('server/s_http.R', local = TRUE)
  source('server/s_flow.R', local = TRUE)
  source('server/s_netflow.R', local = TRUE)
  source('server/s_dns.R', local = TRUE)


  # source('server/s_maps.R', local = TRUE)
  source('server/s_maps.R', local = TRUE)
  
  source('server/s_user.R', local = TRUE)
  
  # Max age of data (default = 5 minutes)
  
  max_age_secs <- 60 * max_age_minutes
  
  #### 
  # alert_stream is a reactive expression that represents a stream of
  # new rows from a suricate event log stored in redis; each call only 
  # returns new rows occouring after the last of those delivered prior.
  alert_stream <- alertStream(session)
  
  ###
  # Set minimum refresh rate for tabs which are slower to render
  #
  #
  
  refresh_rates <- data.frame(rbind(c(-1,30,30,-1,-1),c(1,60,60,1,1)), row.names=c('min','default'))
  names(refresh_rates) <- c('default','table','map','dash','plot')
  
  reactive({
    browser()
    v$selected_tab
    inputId <- "data_refresh_rate"
    #v$selected_tab <- session$getCurrentOutputInfo()$name
    output_type <- str_split(v$selected_tab,'_')[[1]][2]
    
    if(v$selected_tab != session$getCurrentOutputInfo()$name)
      v$selected_tab <-  session$getCurrentOutputInfo()$name    
    output_type <- str_split(v$selected_tab,'_')[[1]][2]
print(v$selected_tab)
print(output_type)
    if( ! output_type %in% names(refresh_rates)){
      output_type <- 'default'
      print('update_refresh_rate given invalid value for output_type. using default value: "dash"')
    }

      if(is.null(isolate(input[inputId])))
        print(paste("input object" , inputId ,"does not exist"))
      
      # if(
      # ! is.null(isolate(input[inputId]))
      # && suppressWarnings(!is.na(as.numeric(isolate(input$data_refresh_rate)))) 
      # &&
      # as.numeric(isolate(input$data_refresh_rate) < refresh_rates_df['min',output_type])){
      print(paste('Updating refresh rate from',isolate(input$data_refresh_rate), 'to',refresh_rates['min',output_type] ))
      updateSliderTextInput(session, "data_refresh_rate", selected = refresh_rates_df['default',output_type]) 
      # }
})
  
                         # tabName = "all_dash", 
                         # tabName = "all_table", 
                         # tabName = "http_dash", 
                         # tabName = "http_map", 
                         # tabName = "http_table", 
                         # tabName = "flow_dash", 
                         # tabName = "flow_app_traffic", 
                         # tabName = "flow_map", 
                         # tabName = "flow_table", 
                         # tabName = "netflow_dash", 
                         # tabName = "netflow_map", 
                         # tabName = "netflow_table", 
                         # tabName = "alert_dash", 
                         # tabName = "alert_map", 
                         # tabName = "drop_dash", 
                         # tabName = "drop_map", 
                         # 
  
  #so Record the time that the session started.
  start_timestamp <- as.numeric(Sys.time())
  first_timestamp <- firstTimestamp(alert_stream)
  last_timestamp <- lastTimestamp(alert_stream)
  
  event_count <- eventCount(alert_stream)
  
  output$all.event_count.bubbleplot <- renderBubbles({
    if (nrow(event_count()) == 0)
      return()
    
    order <- unique(event_count()$event_type)
    df <- event_count() 
    
    bubbles(df$event_count, df$event_type, key = df$event_type)
  })
  #hide_waiter()
  
}
, info = a0_info)  
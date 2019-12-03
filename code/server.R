
####--SERVER------------------------------------------------------------------------------------------------
# Shiny server options
options(shiny.port = shiny_port
        ,auth0_disable = (! using_auth)
        ,app_init_timeout = shiny_app_init_timeout 
        ,app_idle_timeout = shiny_app_idle_timeout
        ,warn = -1)

auth0_server(function(input, output, session, options) {
  
  ####--UI BLOCK----------------------------------------------------------------------------------------------
  default_tab = "http_dashboard"
  current_version = "0.01"
  
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
      sliderInput("rateThreshold", "Warn when rate exceeds",
                  min = 0, max = 50, value = 3, step = 0.1
      ),
      sidebarMenu(
          menuItem("All Events",icon = icon("cube"),
            menuSubItem("Event Dashboard", 
              tabName = "all_dash", 
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
    tabItems(
      tabItem_all_dashboard,
      tabItem_all_table,
      tabItem_http_dashboard,
      tabItem_http_map,
      tabItem_http_table,
      tabItem_flow_dashboard,
      # tabItem_flow_map,
      tabItem_flow_table,
      tabItem_netflow_dashboard,
      # tabItem_netflow_map,
      tabItem_netflow_table
    )
    
  })
  
  
  ####--SERVER BLOCK-----------------------------------------------------------------------------------------
  
  ## Constants
  
 # show_waiter(spin_fading_circles())
  
  v <- reactiveValues(
    selected_tab = default_tab,
    event_type = 'http',
    pulse_icon_message = "",
    map_shape_id = NULL,
    selected_alert_id = NULL,
    
    suricata_df = NULL,
    
    http_df = NULL,
    tls_df = NULL,
    fileinfo_df = NULL,
    dns_df = NULL,
    dhcp_df = NULL,
    drop_df = NULL,
    alerts_df = NULL,
    flow_df = NULL,
    netflow_df = NULL
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
  source('server/s_dns.R', local = TRUE)
  source('server/s_flow.R', local = TRUE)
  source('server/s_netflow.R', local = TRUE)
  
  source('server/s_http_map.R', local = TRUE)
  
  source('server/s_user.R', local = TRUE)
  
  
  # 
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  alert_stream <- alertStream(session)
  
  max_age_minutes <- 30
  
  # Max age of data (5 minutes)
  max_age_secs <- 60 * max_age_minutes
  
  #so Record the time that the session started.
  start_timestamp <- as.numeric(Sys.time())
  first_timestamp <- firstTimestamp(alert_stream)
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
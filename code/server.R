source('server/s_dashboards.R', local = TRUE)

####--SERVER------------------------------------------------------------------------------------------------
# Shiny server options
options(shiny.port = shiny_port
        ,auth0_disable = (! using_auth)
        ,app_init_timeout = shiny_app_init_timeout 
        ,app_idle_timeout = shiny_app_idle_timeout
        ,warn = -1)

auth0_server(function(input, output, session, options) {

  v <- reactiveValues(
     redis_last_total = 0
  )
  # 
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  alrtStream <- alertStream(session)
  
  maxAgeMinutes <- 30
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * maxAgeMinutes
  
  # alrtData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  alrtData <- alertData(alrtStream, maxAgeSecs)
  
  
  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  rqstCount <- requestCount(alrtStream)
  
  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  dstCount <- destinationCount(alrtStream)
  
  
  
  #so Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in alrtData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(alrtData()) / min(maxAgeSecs, elapsed)
    
    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = paste("Http request per sec (last",maxAgeMinutes, "min)"),
      icon = icon("area-chart"),
      color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
  
  output$destinations <- renderValueBox({
    valueBox(
      dstCount(),
      "Unique desintations",
      icon = icon("computer-classic")
    )
  })
  
  output$requests <- renderValueBox({
    valueBox(
      rqstCount(),
      "Total HTTP requests",
      icon = icon("browser")
    )
  })
  
  output$packagePlot <- renderBubbles({
    if (nrow(alrtData()) == 0)
      return()
    
    order <- unique(alrtData()$http.hostname)
    df <- alrtData() %>%
      group_by(http.hostname) %>%
      tally() %>%
      arrange(desc(n), tolower(http.hostname)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)
    
    bubbles(df$n, df$http.hostname, key = df$http.hostname)
  })
  
####--UI BLOCK----------------------------------------------------------------------------------------------
  default_tab = "dashboard"
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
      sidebarMenu(id = "tab",
                  menuItem("Dashboard", 
                           tabName = "dashboard", 
                           icon = icon("globe-asia")
                  ),
                  menuItem("Map", 
                           tabName = "alerts", 
                           icon = icon("globe-asia")
                  )
                  #,
                  #menuItem("Queensland map", 
                  #         tabName = "solution", 
                  #         icon = icon("globe-asia")
                  #)
                  #,
                  # menuItem("Problem", 
                  #          tabName = "problem", 
                  #          icon = icon("bug")
                  #          ),
                  # 
                  # menuItem("Data", 
                  #          tabName = "data", 
                  #          icon = icon("cube")
                  #          ),
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
    updateTabsetPanel(session, "tab", selected = default_tab)
    tabsetPanel(
      id = "tab",
      #tabItem_alerts,
      tabItem_dashboard
      # ,
      # tabItem_solution,
      # tabItem_problem,
      # tabItem_data
    )
  })
  
  
####--SERVER BLOCK-----------------------------------------------------------------------------------------
  
  ## Constants
  
  ## ReactiveValues
  v <- reactiveValues(
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
#  source('server/s_alerts_map.R', local = TRUE)
  source('server/s_user.R', local = TRUE)
}
, info = a0_info)  

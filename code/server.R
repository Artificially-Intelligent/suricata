
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
                     choices=c(0, 1, 3, 5, 10, 15, 30, 60, 120, 180,300,600,900,1800,3600,86400,"disabled"),
                     selected=30, grid = T),
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
        menuItem("DNS",icon = icon("cube"),
                 menuSubItem("DNS Dashboard", 
                             tabName = "dns_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("DNS Map", 
                             tabName = "dns_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("DNS Detail", 
                             tabName = "dns_table", 
                             icon = icon("table")
                 )         
        ),
        menuItem("TLS",icon = icon("cube"),
                 menuSubItem("tls Dashboard", 
                             tabName = "tls_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("tls Map", 
                             tabName = "tls_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("tls Detail", 
                             tabName = "tls_table", 
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
                 ),
                 menuSubItem("Alert Detail", 
                             tabName = "alert_table", 
                             icon = icon("table")
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
                 ),
                 menuSubItem("Drop Detail", 
                             tabName = "drop_table", 
                             icon = icon("table")
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
      tabItem_dashboard('all')
      ,tabItem_map('all')
      ,tabItem_table('all')
      
      ,tabItem_dashboard('http')
      ,tabItem_map('http')
      ,tabItem_table('http')
      
      ,tabItem_dashboard('flow')
      ,tabItem_map('flow')
      ,tabItem_table('flow')
      # ,tabItem_flow_app_traffic
      
      ,tabItem_dashboard('netflow')
      ,tabItem_map('netflow')
      ,tabItem_table('netflow')
      
      ,tabItem_dashboard('alert')
      ,tabItem_map('alert')
      ,tabItem_table('alert')
      
      ,tabItem_dashboard('dns')
      ,tabItem_map('dns')
      ,tabItem_table('dns')
      
      ,tabItem_dashboard('tls')
      ,tabItem_map('tls')
      ,tabItem_table('tls')
      
      ,tabItem_dashboard('drop')
      ,tabItem_map('drop')
      ,tabItem_table('drop')
      
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

  
  # Server modules 
  source('server/s_user.R', local = TRUE)
  
  source('server/s_data.R', local = TRUE)
  
  source('server/s_all.R', local = TRUE)
  # source('server/s_http.R', local = TRUE)
  # source('server/s_flow.R', local = TRUE)
  # source('server/s_netflow.R', local = TRUE)
  # source('server/s_alert.R', local = TRUE)

  source('server/s_event.R', local = TRUE)
  
  # source('server/s_maps.R', local = TRUE)

  # Max age of data (default = 5 minutes)
  
  max_age_secs <- 60 * max_age_minutes
  
  #### 
  # event_stream is a reactive expression that represents a stream of
  # new rows from a suricate event log stored in redis; each call only 
  # returns new rows occouring after the last of those delivered prior.
  event_stream <- alertStream(session)
  
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
  first_timestamp <- firstTimestamp(event_stream)
  last_timestamp <- lastTimestamp(event_stream)
  
  event_count <- eventCount(event_stream)
  
  output$all.event_count.bubbleplot <- renderBubbles({
    if (nrow(event_count()) == 0)
      return()
    
    order <- unique(event_count()$event_type)
    df <- event_count() 
    
    bubbles(df$event_count, df$event_type, key = df$event_type)
  })
  #hide_waiter()
  
  

    
  # DNS
  
  dns_data <- eventData(event_stream, max_age_secs, event_type = "dns")
  
  # DNS Dashboard
  output$dns.rate <- renderValueBox_rate( event_type = "dns", event_data = dns_data)
  output$dns.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "dns")
  output$dns.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "dns")
  output$dns.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "dns")
  output$dns.report_period <- renderText_report_period(event_data = dns_data, event_type = "dns")
  output$dns.destination.bubbleplot <- renderBubbles_destination(event_data = dns_data, event_type = "dns")
  output$dns.destination.table <- renderTable_dest_ip(event_data = dns_data, event_type = "dns")
  
  # DNS Table
  output$dns.table <- renderDT_table(event_data = dns_data, event_type = "dns")
  output$dns.download_csv <-downloadHandler_csv(event_data = dns_data, event_type = "dns")
    
  # DNS Map
  output$dns_map_leaflet <- renderLeaflet_map_destination(event_data = dns_data, event_type = "dns", color_column = 'dns.type')
  output$dns_map_table_summary <- renderTable_maptable_summary(event_data = dns_data, event_type = "dns", value_column = 'dns.type')
  output$dns_map_table_detail  <- renderDT_maptable_detail(event_data = dns_data, event_type = "dns")
  output$dns_map.value.1   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", value_column = 'dns.rrname', filter_column = 'dns.type' ,filter_value = 'query', opp = 'count', icon_name = "question-circle")
  output$dns_map.value.2   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", value_column = 'dns.answers', opp = 'count',icon_name = "reply")
  output$dns_map.value.3   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", value_column = 'dns.rrname', opp = 'unique_count',icon_name = "reply")
  output$dns_map.value.4   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", value_column = 'dns.answers', opp = 'unique_count',icon_name = "reply")
  
  
  # HTTP
  
  http_data <- eventData(event_stream, max_age_secs, event_type = "http")
  
  # HTTP Dashboard
  output$http.rate <- renderValueBox_rate( event_type = "http", event_data = http_data)
  output$http.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "http")
  output$http.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "http")
  output$http.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "http")
  output$http.report_period <- renderText_report_period(event_data = http_data, event_type = "http")
  output$http.destination.bubbleplot <- renderBubbles_destination(event_data = http_data, event_type = "http", value_column = 'http.hostname')
  output$http.destination.table <- renderTable_dest_ip(event_data = http_data, event_type = "http")
  
  # HTTP Table
  output$http.table <- renderDT_table(event_data = http_data, event_type = "http")
  output$http.download_csv <-downloadHandler_csv(event_data = http_data, event_type = "http")
  
  # HTTP Map
  output$http_map_leaflet <- renderLeaflet_map_destination(event_data = http_data, event_type = "http", color_column = 'http.status')
  output$http_map_table_summary <- renderTable_maptable_summary(event_data = http_data, event_type = "http", value_column = 'http.status')
  output$http_map_table_detail  <- renderDT_maptable_detail(event_data = http_data, event_type = "http")
  output$http_map.value.1   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", value_column = 'http.hostname', opp = 'unique_count', icon_name = "question-circle",label = 'HTTP Hosts')
  output$http_map.value.2   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", value_column = 'http.http_user_agent', opp = 'unique_count' ,icon_name = "reply")
  output$http_map.value.3   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", value_column = 'http.length', opp = 'sum' ,icon_name = "reply", label = 'Total Bytes')
  output$http_map.value.4   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", value_column = 'http.length', opp = 'max' ,icon_name = "reply")
  
  
  # Flow
  
  flow_data <- eventData(event_stream, max_age_secs, event_type = "flow")
  
  # Flow Dashboard
  output$flow.rate <- renderValueBox_rate( event_type = "flow", event_data = flow_data)
  output$flow.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "flow")
  output$flow.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "flow")
  output$flow.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "flow")
  output$flow.report_period <- renderText_report_period(event_data = flow_data, event_type = "flow")
  output$flow.destination.bubbleplot <- renderBubbles_destination(event_data = flow_data, event_type = "flow", value_column = 'dest_country_name')
  output$flow.destination.table <- renderTable_dest_ip(event_data = flow_data, event_type = "flow")
  
  # Flow Table
  output$flow.table <- renderDT_table(event_data = flow_data, event_type = "flow")
  output$flow.download_csv <-downloadHandler_csv(event_data = flow_data, event_type = "flow")
  
  # Flow Map
  output$flow_map_leaflet <- renderLeaflet_map_destination(event_data = flow_data, event_type = "flow", color_column = 'app_proto')
  output$flow_map_table_summary <- renderTable_maptable_summary(event_data = flow_data, event_type = "flow", value_column = 'app_proto')
  output$flow_map_table_detail  <- renderDT_maptable_detail(event_data = flow_data, event_type = "flow")
  output$flow_map.value.1   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", value_column = 'flow.bytes_toclient', opp = 'sum', icon_name = "reply", label = 'Bytes to Client')
  output$flow_map.value.2   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", value_column = 'flow.bytes_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Bytes to Server')
  output$flow_map.value.3   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", value_column = 'flow.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  output$flow_map.value.4   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", value_column = 'flow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
  
  
  # NetFlow
  
  netflow_data <- eventData(event_stream, max_age_secs, event_type = "netflow")
  
  # NetFlow Dashboard
  output$netflow.rate <- renderValueBox_rate( event_type = "netflow", event_data = netflow_data)
  output$netflow.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "netflow")
  output$netflow.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "netflow")
  output$netflow.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "netflow")
  output$netflow.report_period <- renderText_report_period(event_data = netflow_data, event_type = "netflow")
  output$netflow.destination.bubbleplot <- renderBubbles_destination(event_data = netflow_data, event_type = "netflow", value_column = 'dest_country_name')
  output$netflow.destination.table <- renderTable_dest_ip(event_data = netflow_data, event_type = "netflow")
  
  # NetFlow Table
  output$netflow.table <- renderDT_table(event_data = netflow_data, event_type = "netflow")
  output$netflow.download_csv <-downloadHandler_csv(event_data = netflow_data, event_type = "netflow")
  
  # NetFlow Map
  output$netflow_map_leaflet <- renderLeaflet_map_destination(event_data = netflow_data, event_type = "netflow", color_column = 'app_proto')
  output$netflow_map_table_summary <- renderTable_maptable_summary(event_data = netflow_data, event_type = "netflow", value_column = 'app_proto')
  output$netflow_map_table_detail  <- renderDT_maptable_detail(event_data = netflow_data, event_type = "netflow")
  output$netflow_map.value.1   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", value_column = 'netflow.bytes', opp = 'sum', icon_name = "reply", label = 'Bytes')
  output$netflow_map.value.2   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", value_column = 'netflow.pkts', opp = 'sum' ,icon_name = "question-circle",label = 'Packets')
  # output$netflow_map.value.3   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", value_column = '_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  # output$netflow_map.value.4   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", value_column = 'netflow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
   
  # Alert
  
  alert_data <- eventData(event_stream, max_age_secs, event_type = "alert")
  
  # Alert Dashboard
  output$alert.rate <- renderValueBox_rate( event_type = "alert", event_data = alert_data)
  output$alert.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "alert")
  output$alert.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "alert")
  output$alert.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "alert")
  output$alert.report_period <- renderText_report_period(event_data = alert_data, event_type = "alert")
  output$alert.destination.bubbleplot <- renderBubbles_destination(event_data = alert_data, event_type = "alert", value_column = 'dest_country_name')
  output$alert.destination.table <- renderTable_dest_ip(event_data = alert_data, event_type = "alert")
  
  # Alert Table
  output$alert.table <- renderDT_table(event_data = alert_data, event_type = "alert")
  output$alert.download_csv <-downloadHandler_csv(event_data = alert_data, event_type = "alert")
  
  # Alert Map
  output$alert_map_leaflet <- renderLeaflet_map_destination(event_data = alert_data, event_type = "alert", color_column = 'alert.severity')
  output$alert_map_table_summary <- renderTable_maptable_summary(event_data = alert_data, event_type = "alert", value_column = 'alert.category')
  output$alert_map_table_detail  <- renderDT_maptable_detail(event_data = alert_data, event_type = "alert")
  output$alert_map.value.1   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", value_column = 'alert.severity', value = '3' ,  opp = 'count', icon_name = "reply", label = 'Notice')
  output$alert_map.value.2   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", value_column = 'alert.severity', value = '2' ,  opp = 'count', icon_name = "reply", label = 'Warning')
  output$alert_map.value.3   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", value_column = 'alert.severity', value = '1' ,  opp = 'count', icon_name = "reply", label = 'Error')
  output$alert_map.value.4   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", value_column = 'flow.bytes_toclient', opp = 'sum' ,icon_name = "reply", label = 'Bytes to Client')
  
  
  # Drop
  
  drop_data <- eventData(event_stream, max_age_secs, event_type = "drop")
  
  # Drop Dashboard
  output$drop.rate <- renderValueBox_rate( event_type = "drop", event_data = drop_data)
  output$drop.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "drop")
  output$drop.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "drop")
  output$drop.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "drop")
  output$drop.report_period <- renderText_report_period(event_data = drop_data, event_type = "drop")
  output$drop.destination.bubbleplot <- renderBubbles_destination(event_data = drop_data, event_type = "drop", value_column = 'dest_country_name')
  output$drop.destination.table <- renderTable_dest_ip(event_data = drop_data, event_type = "drop")
  
  # Drop Table
  output$drop.table <- renderDT_table(event_data = drop_data, event_type = "drop")
  output$drop.download_csv <-downloadHandler_csv(event_data = drop_data, event_type = "drop")
  
  # Drop Map
  output$drop_map_leaflet <- renderLeaflet_map_destination(event_data = drop_data, event_type = "drop", color_column = 'app_proto')
  output$drop_map_table_summary <- renderTable_maptable_summary(event_data = drop_data, event_type = "drop", value_column = 'app_proto')
  output$drop_map_table_detail  <- renderDT_maptable_detail(event_data = drop_data, event_type = "drop")
  output$drop_map.value.1   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", value_column = 'flow.bytes_toclient', opp = 'sum', icon_name = "reply", label = 'Bytes to Client')
  output$drop_map.value.2   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", value_column = 'flow.bytes_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Bytes to Server')
  output$drop_map.value.3   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", value_column = 'flow.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  output$drop_map.value.4   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", value_column = 'flow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
  
  
  # TLS
  
  tls_data <- eventData(event_stream, max_age_secs, event_type = "tls")
  
  # TLS Dashboard
  output$tls.rate <- renderValueBox_rate( event_type = "tls", event_data = tls_data)
  output$tls.destinations <- renderValueBox_destinations(event_stream = event_stream, event_type = "tls")
  output$tls.requests <- renderValueBox_requests(event_stream = event_stream, event_type = "tls")
  output$tls.bytes <- renderValueBox_requests(event_stream = event_stream, event_type = "tls")
  output$tls.report_period <- renderText_report_period(event_data = tls_data, event_type = "tls")
  output$tls.destination.bubbleplot <- renderBubbles_destination(event_data = tls_data, event_type = "tls", value_column = 'dest_country_name')
  output$tls.destination.table <- renderTable_dest_ip(event_data = tls_data, event_type = "tls")
  
  # TLS Table
  output$tls.table <- renderDT_table(event_data = tls_data, event_type = "tls")
  output$tls.download_csv <-downloadHandler_csv(event_data = tls_data, event_type = "tls")
  
  # TLS Map
  output$tls_map_leaflet <- renderLeaflet_map_destination(event_data = tls_data, event_type = "tls", color_column = 'tls.version')
  output$tls_map_table_summary <- renderTable_maptable_summary(event_data = tls_data, event_type = "tls", value_column = 'tls.version')
  output$tls_map_table_detail  <- renderDT_maptable_detail(event_data = tls_data, event_type = "tls")
  output$tls_map.value.1   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", value_column = 'tls.sni', opp = 'count', icon_name = "reply", label = 'TLS Requests')
  output$tls_map.value.2   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", value_column = 'tls.sni', opp = 'unique_count' ,icon_name = "question-circle",label = 'TLS Hosts')
  # output$tls_map.value.3   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", value_column = 'tls.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  # output$tls_map.value.4   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", value_column = 'tls.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
  
  
}
, info = a0_info)  
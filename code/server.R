
####--SERVER------------------------------------------------------------------------------------------------
# Shiny server options
options(shiny.port = shiny_port
        ,auth0_disable = (! using_auth)
        ,warn = -1
        ,shiny.reactlog = TRUE)

shiny_server <- function(input, output, session, options) {
  
  default_tab = "http_dash"
  current_version = "0.01"
  
  ####--UI BLOCK----------------------------------------------------------------------------------------------
  
  cdata <- session$clientData
  
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
                     selected=60, grid = T),
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
                 menuSubItem("HTTP Timeseries", 
                             tabName = "http_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Http Overview", 
                             tabName = "http_overview", 
                             
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
                 menuSubItem("DNS Timeseries", 
                             tabName = "dns_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("DNS Overview", 
                             tabName = "dns_overview", 
                             
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
                 menuSubItem("TLS Dashboard", 
                             tabName = "tls_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("TLS Timeseries", 
                             tabName = "tls_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("TLS Overview", 
                             tabName = "tls_overview",
                             icon = icon("dashboard")
                 ),
                 menuSubItem("TLS Map", 
                             tabName = "tls_map", 
                             icon = icon("globe-asia")
                 ),
                 menuSubItem("TLS Detail", 
                             tabName = "tls_table", 
                             icon = icon("table")
                 )         
        ),
        menuItem("Flow",icon = icon("code-branch"),
                 menuSubItem("Flow Dashboard", 
                             tabName = "flow_dash", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Flow Timeseries", 
                             tabName = "flow_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Flow Overview", 
                             tabName = "flow_overview",
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
                 menuSubItem("NetFlow Timeseries", 
                             tabName = "netflow_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Netflow Overview", 
                             tabName = "netflow_overview", 
                             
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
                 menuSubItem("Alert Timeseries", 
                             tabName = "alert_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Alert Overview", 
                             tabName = "alert_overview", 
                             
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
                 menuSubItem("Drop Timeseries", 
                             tabName = "drop_timeseries", 
                             icon = icon("dashboard")
                 ),
                 menuSubItem("Drop Overview", 
                             tabName = "drop_overview", 
                             
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
      ,tabItem_timeseries('http')
      ,tabItem_overview('http')
      ,tabItem_map('http')
      ,tabItem_table('http')
      
      ,tabItem_dashboard('flow')
      ,tabItem_timeseries('flow')
      ,tabItem_overview('flow')
      ,tabItem_map('flow')
      ,tabItem_table('flow')
      
      ,tabItem_dashboard('netflow')
      ,tabItem_timeseries('netflow')
      ,tabItem_overview('netflow')
      ,tabItem_map('netflow')
      ,tabItem_table('netflow')
      
      ,tabItem_dashboard('alert')
      ,tabItem_timeseries('alert')
      ,tabItem_overview('alert')
      ,tabItem_map('alert')
      ,tabItem_table('alert')
      
      ,tabItem_dashboard('dns')
      ,tabItem_timeseries('dns')
      ,tabItem_overview('dns')
      ,tabItem_map('dns')
      ,tabItem_table('dns')
      
      ,tabItem_dashboard('tls')
      ,tabItem_timeseries('tls')
      ,tabItem_overview('tls')
      ,tabItem_map('tls')
      ,tabItem_table('tls')
      
      ,tabItem_dashboard('drop')
      ,tabItem_timeseries('drop')
      ,tabItem_overview('drop')
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
  
  # source('server/s_event_summary.R', local = TRUE)
  
  source('server/s_event.R', local = TRUE)
  
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
  
  event_count <- eventCount(event_stream)
  output$all.event_count.bubbleplot <- renderBubbles({
    if (nrow(event_count()) == 0)
      return()
    
    order <- unique(event_count()$event_type)
    df <- event_count() 
    
    bubbles(df$event_count, df$event_type, key = df$event_type)
  })
  #hide_waiter()
  
  
  # ALL Summary
  
  all_data <- eventData(event_stream, max_age_secs, event_type = "all")
  
  # ALL Dashboard
  output$all.rate <- renderValueBox_rate(event_data = all_data, event_type = "all")
  output$all.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = all_data , event_type = "all")
  output$all.requests <- renderValueBox_requests(event_data = all_data, event_type = "all")
  output$all.bytes <- renderValueBox_value_agg(event_data = all_data, event_type = "all", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$all.report_period <- renderText_report_period()
  output$all.destination.bubbleplot <- renderBubbles_destination(event_data = all_data, event_type = "all")
  output$all.destination.table <- renderTable_value(event_data = all_data, event_type = "all", value_column = 'dest_ip')
  
  output$all.event_count.table <- renderTable_value(event_data = all_data, event_type = "all", value_column = 'event_type')
  
  # ALL Table
  output$all.table <- renderDT_table(event_data = all_data, event_type = "all")
  output$all.download_csv <-downloadHandler_csv(event_data = all_data, event_type = "all")
  
    
  # DNS
  
  dns_data <- eventData(event_stream, max_age_secs, event_type = "dns")
  
  # DNS Dashboard
  output$dns.rate <- renderValueBox_rate(event_data = dns_data, event_type = "dns")
  output$dns.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = dns_data , event_type = "dns")
  output$dns.requests <- renderValueBox_requests(event_data = dns_data, event_type = "dns")
  output$dns.bytes <- renderValueBox_value_agg(event_data = dns_data, event_type = "dns", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$dns.report_period <- renderText_report_period()
  output$dns.destination.bubbleplot <- renderBubbles_destination(event_data = dns_data, event_type = "dns")
  output$dns.destination.table <- renderTable_value(event_data = dns_data, event_type = "dns", value_column = 'dest_ip')
  
  # DNS Timeseries
  output$dns.plotly <- renderPlotly_value.barplot(event_data = dns_data, event_type = "dns", agg_function = 'sum')
  output$dns.download_timeseries_csv <-downloadHandler_csv(event_data = dns_data, event_type = "dns")
  
  
  # DNS Overview
  output$dns_overview.table_summary <- renderTable_value(event_data = dns_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "dns", value_column = 'dns.type')
  output$dns_overview.table_detail  <- renderDT_maptable_detail(event_data = dns_data, event_type = "dns", tab_name_suffix = '_overview', leafletId_suffix = '.leaflet')
  output$dns_overview.plotly <- renderPlotly_value.barplot(event_data = dns_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "dns", agg_function = 'sum')
  output$dns_overview.download_timeseries_csv <-downloadHandler_csv(event_data = dns_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "dns")
  
  # DNS Table
  output$dns.table <- renderDT_table(event_data = dns_data, event_type = "dns")
  output$dns.download_csv <-downloadHandler_csv(event_data = dns_data, event_type = "dns")
    
  # DNS Map
  output$dns_map_leaflet <- renderLeaflet_map_destination(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'dns.type', measure_column = 'count')
  observeEvent_map_button(event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$dns_map_table_summary <- renderTable_value(event_data = dns_data, event_type = "dns", value_column = 'dns.type', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$dns_map_table_detail  <- renderDT_maptable_detail(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$dns_map.value.1   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'dns.rrname', filter_column = 'dns.type' ,filter_value = 'query', opp = 'count', icon_name = "question-circle")
  output$dns_map.value.2   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'dns.answers', opp = 'count',icon_name = "reply")
  output$dns_map.value.3   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'dns.rrname', opp = 'unique_count',icon_name = "reply")
  output$dns_map.value.4   <- renderValueBox_mapvalue(event_data = dns_data, event_type = "dns", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'dns.answers', opp = 'unique_count',icon_name = "reply")
  
  
  # HTTP
  
  http_data <- eventData(event_stream, max_age_secs, event_type = "http")
  
  # HTTP Dashboard
  output$http.rate <- renderValueBox_rate(event_data = http_data, event_type = "http")
  
  output$http.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = http_data , event_type = "http")
  output$http.requests <- renderValueBox_requests(event_data = http_data, event_type = "http")
  output$http.bytes <- renderValueBox_value_agg(event_data = http_data, event_type = "http", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$http.report_period <- renderText_report_period()
  output$http.destination.bubbleplot <- renderBubbles_destination(event_data = http_data, event_type = "http", value_column = 'http.hostname')
  output$http.destination.table <- renderTable_value(event_data = http_data, event_type = "http", value_column = 'dest_ip')
  
  
  # HTTP Overview
  output$http_overview.table_summary <- renderTable_value(event_data = http_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "http", value_column = 'http.hostname')
  output$http_overview.table_detail  <- renderDT_maptable_detail(event_data = http_data, event_type = "http", tab_name_suffix = '_overview', leafletId_suffix = '.leaflet')
  output$http_overview.plotly <- renderPlotly_value.barplot(event_data = http_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "http", agg_function = 'sum')
  output$http_overview.download_timeseries_csv <-downloadHandler_csv(event_data = http_data, tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', event_type = "http")
  output$http_overview.leaflet <- renderLeaflet_map_destination(event_data = http_data, event_type = "http", tab_name_suffix = '_overview', leafletId_suffix = '.leaflet', value_column = 'http.status', measure_column = 'count')
  
  
  # Http Timeseries
  output$http.plotly <- renderPlotly_value.barplot(event_data = http_data, event_type = "http", agg_function = 'sum')
  output$http.download_timeseries_csv <-downloadHandler_csv(event_data = http_data, event_type = "http")
  
  # HTTP Table
  output$http.table <- renderDT_table(event_data = http_data, event_type = "http")
  output$http.download_csv <-downloadHandler_csv(event_data = http_data, event_type = "http")
  
  # HTTP Map
  output$http_map_leaflet <- renderLeaflet_map_destination(event_data = http_data, event_type = "http", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'http.status', measure_column = 'count')
  observeEvent_map_button(event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$http_map_table_summary <- renderTable_value(event_data = http_data, event_type = "http", value_column = 'http.status', measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$http_map_table_detail  <- renderDT_maptable_detail(event_data = http_data, event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$http_map.value.1   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'http.hostname', opp = 'unique_count', icon_name = "question-circle",label = 'HTTP Hosts')
  output$http_map.value.2   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'http.http_user_agent', opp = 'unique_count' ,icon_name = "reply")
  output$http_map.value.3   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'http.length', opp = 'sum' ,icon_name = "reply", label = 'Total Bytes')
  output$http_map.value.4   <- renderValueBox_mapvalue(event_data = http_data, event_type = "http", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'http.length', opp = 'max' ,icon_name = "reply")
  
  # Flow
  
  flow_data <- eventData(event_stream, max_age_secs, event_type = "flow")
  
  # Flow Dashboard
  output$flow.rate <- renderValueBox_rate(event_data = flow_data, event_type = "flow")
  output$flow.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = flow_data , event_type = "flow")
  # output$flow.requests <- renderValueBox_requests(event_data = flow_data, event_type = "flow")
  output$flow.requests <- output$flow.bytes <- renderValueBox_value_agg(event_data = flow_data, event_type = "flow", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  
  output$flow.report_period <- renderText_report_period()
  output$flow.destination.bubbleplot <- renderBubbles_destination(event_data = flow_data, event_type = "flow", value_column = 'dest_country_name')
  output$flow.destination.table <- renderTable_value(event_data = flow_data, event_type = "flow", value_column = 'dest_ip' , measure_column = 'count', tab_name_suffix = '_dash' , leafletId_suffix = "_leaflet")

  # Flow Timeseries
  output$flow.plotly <- renderPlotly_value.barplot(event_data = flow_data, event_type = "flow", agg_function = 'sum')
  output$flow.download_timeseries_csv <-downloadHandler_csv(event_data = flow_data, event_type = "flow")
  
    
  # Flow Table
  output$flow.table <- renderDT_table(event_data = flow_data, event_type = "flow")
  output$flow.download_csv <-downloadHandler_csv(event_data = flow_data, event_type = "flow")
  
  # Flow Map
  output$flow_map_leaflet <- renderLeaflet_map_destination(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'app_proto', measure_column = 'count')
  observeEvent_map_button(event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$flow_map_table_summary <- renderTable_value(event_data = flow_data, event_type = "flow", value_column = 'app_proto' , measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$flow_map_table_detail  <- renderDT_maptable_detail(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$flow_map.value.1   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.bytes_toclient', opp = 'sum', icon_name = "reply", label = 'Bytes to Client')
  output$flow_map.value.2   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.bytes_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Bytes to Server')
  output$flow_map.value.3   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  output$flow_map.value.4   <- renderValueBox_mapvalue(event_data = flow_data, event_type = "flow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
  
  
  # NetFlow
  
  netflow_data <- eventData(event_stream, max_age_secs, event_type = "netflow")
  
  # NetFlow Dashboard
  output$netflow.rate <- renderValueBox_rate(event_data = netflow_data, event_type = "netflow")
  output$netflow.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = netflow_data , event_type = "netflow")
  # output$netflow.requests <- renderValueBox_requests(event_data = netflow_data, event_type = "flow")
  output$netflow.requests <- output$netflow.bytes <- renderValueBox_value_agg(event_data = netflow_data, event_type = "netflow", value_column = 'event_type', measure_column = c('netflow.bytes'), icon_desc = 'desktop')
  
  output$netflow.report_period <- renderText_report_period()
  output$netflow.destination.bubbleplot <- renderBubbles_destination(event_data = netflow_data, event_type = "netflow", value_column = 'dest_country_name')
  output$netflow.destination.table <- renderTable_value(event_data = netflow_data, event_type = "netflow", value_column = 'dest_ip')

  # NetFlow Timeseries
  output$netflow.plotly <- renderPlotly_value.barplot(event_data = netflow_data, event_type = "netflow", agg_function = 'sum')
  output$netflow.download_timeseries_csv <-downloadHandler_csv(event_data = netflow_data, event_type = "netflow")
    
  # NetFlow Table
  output$netflow.table <- renderDT_table(event_data = netflow_data, event_type = "netflow")
  output$netflow.download_csv <-downloadHandler_csv(event_data = netflow_data, event_type = "netflow")
  
  # NetFlow Map
  output$netflow_map_leaflet <- renderLeaflet_map_destination(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'app_proto', measure_column = 'count')
  observeEvent_map_button(event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$netflow_map_table_summary <- renderTable_value(event_data = netflow_data, event_type = "netflow", value_column = 'app_proto' , measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$netflow_map_table_detail  <- renderDT_maptable_detail(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$netflow_map.value.1   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'netflow.bytes', opp = 'sum', icon_name = "reply", label = 'Bytes')
  output$netflow_map.value.2   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'netflow.pkts', opp = 'sum' ,icon_name = "question-circle",label = 'Packets')
  # output$netflow_map.value.3   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = '_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  # output$netflow_map.value.4   <- renderValueBox_mapvalue(event_data = netflow_data, event_type = "netflow", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'netflow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
   
  # Alert
  
  alert_data <- eventData(event_stream, max_age_secs, event_type = "alert")
  
  # Alert Dashboard
  output$alert.rate <- renderValueBox_rate(event_data = alert_data, event_type = "alert")
  output$alert.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = alert_data , event_type = "alert")
  output$alert.requests <- renderValueBox_requests(event_data = alert_data, event_type = "alert")
  output$alert.bytes <- renderValueBox_value_agg(event_data = alert_data, event_type = "alert", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$alert.report_period <- renderText_report_period()
  output$alert.destination.bubbleplot <- renderBubbles_destination(event_data = alert_data, event_type = "alert", value_column = 'dest_country_name')
  output$alert.destination.table <- renderTable_value(event_data = alert_data, event_type = "alert", value_column = 'dest_ip')
  
  # Alert Timeseries
  output$alert.plotly <- renderPlotly_value.barplot(event_data = alert_data, event_type = "alert", agg_function = 'sum')
  output$alert.download_timeseries_csv <-downloadHandler_csv(event_data = alert_data, event_type = "alert")
  
  # Alert Table
  output$alert.table <- renderDT_table(event_data = alert_data, event_type = "alert")
  output$alert.download_csv <-downloadHandler_csv(event_data = alert_data, event_type = "alert")
  
  # Alert Map
  output$alert_map_leaflet <- renderLeaflet_map_destination(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'alert.severity', measure_column = 'count', group_by_src = TRUE)
  observeEvent_map_button(event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$alert_map_table_summary <- renderTable_value(event_data = alert_data, event_type = "alert", value_column = 'alert.category' , measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$alert_map_table_detail  <- renderDT_maptable_detail(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$alert_map.value.1   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'alert.severity', value = '3' ,  opp = 'count', icon_name = "reply", label = 'Notice')
  output$alert_map.value.2   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'alert.severity', value = '2' ,  opp = 'count', icon_name = "reply", label = 'Warning')
  output$alert_map.value.3   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'alert.severity', value = '1' ,  opp = 'count', icon_name = "reply", label = 'Error')
  output$alert_map.value.4   <- renderValueBox_mapvalue(event_data = alert_data, event_type = "alert", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.bytes_toclient', opp = 'sum' ,icon_name = "reply", label = 'Bytes to Client')
  
  
  # Drop
  
  drop_data <- eventData(event_stream, max_age_secs, event_type = "drop")
  
  # Drop Dashboard
  output$drop.rate <- renderValueBox_rate(event_data = drop_data, event_type = "drop")
  output$drop.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = drop_data , event_type = "drop")
  output$drop.requests <- renderValueBox_requests(event_data = drop_data, event_type = "drop")
  output$drop.bytes <- renderValueBox_value_agg(event_data = drop_data, event_type = "drop", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$drop.report_period <- renderText_report_period()
  output$drop.destination.bubbleplot <- renderBubbles_destination(event_data = drop_data, event_type = "drop", value_column = 'dest_country_name')
  output$drop.destination.table <- renderTable_value(event_data = drop_data, event_type = "drop", value_column = 'dest_ip')
  
  # Drop Timeseries
  output$drop.plotly <- renderPlotly_value.barplot(event_data = drop_data, event_type = "drop", agg_function = 'sum')
  output$drop.download_timeseries_csv <-downloadHandler_csv(event_data = drop_data, event_type = "drop")
  
  # Drop Table
  output$drop.table <- renderDT_table(event_data = drop_data, event_type = "drop")
  output$drop.download_csv <-downloadHandler_csv(event_data = drop_data, event_type = "drop")
  
  # Drop Map
  output$drop_map_leaflet <- renderLeaflet_map_destination(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'app_proto', measure_column = 'count')
  observeEvent_map_button(event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$drop_map_table_summary <- renderTable_value(event_data = drop_data, event_type = "drop", value_column = 'app_proto' , measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$drop_map_table_detail  <- renderDT_maptable_detail(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$drop_map.value.1   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.bytes_toclient', opp = 'sum', icon_name = "reply", label = 'Bytes to Client')
  output$drop_map.value.2   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.bytes_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Bytes to Server')
  output$drop_map.value.3   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  output$drop_map.value.4   <- renderValueBox_mapvalue(event_data = drop_data, event_type = "drop", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'flow.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
  
  
  # TLS
  
  tls_data <- eventData(event_stream, max_age_secs, event_type = "tls")
  
  # TLS Dashboard
  output$tls.rate <- renderValueBox_rate(event_data = tls_data, event_type = "tls")
  output$tls.destinations <- renderValueBox_value_count(value_column = 'dest_ip', icon_desc = 'desktop', event_data = tls_data, event_type = "tls")
  output$tls.requests <- renderValueBox_requests(event_data = tls_data, event_type = "tls")
  output$tls.bytes <- renderValueBox_value_agg(event_data = tls_data, event_type = "tls", value_column = 'event_type', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop')
  output$tls.report_period <- renderText_report_period()
  output$tls.destination.bubbleplot <- renderBubbles_destination(event_data = tls_data, event_type = "tls", value_column = 'dest_country_name')
  output$tls.destination.table <- renderTable_value(event_data = tls_data, event_type = "tls", value_column = 'dest_ip')
  
  # TLS Timeseries
  output$tls.plotly <- renderPlotly_value.barplot(event_data = tls_data, event_type = "tls", agg_function = 'sum')
  output$tls.download_timeseries_csv <-downloadHandler_csv(event_data = tls_data, event_type = "tls")
  
  # TLS Table
  output$tls.table <- renderDT_table(event_data = tls_data, event_type = "tls")
  output$tls.download_csv <-downloadHandler_csv(event_data = tls_data, event_type = "tls")
  
  # TLS Map
  output$tls_map_leaflet <- renderLeaflet_map_destination(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map', leafletId_suffix = '_leaflet', value_column = 'tls.version', measure_column = 'count')
  observeEvent_map_button(event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", buttonId = "_zoom_all_button")
  output$tls_map_table_summary <- renderTable_value(event_data = tls_data, event_type = "tls", value_column = 'tls.version' , measure_column = 'count', tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  
    
  output$tls_map_table_detail  <- renderDT_maptable_detail(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet")
  output$tls_map.value.1   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'tls.sni', opp = 'count', icon_name = "reply", label = 'TLS Requests')
  output$tls_map.value.2   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'tls.sni', opp = 'unique_count' ,icon_name = "question-circle",label = 'TLS Hosts')
  # output$tls_map.value.3   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'tls.pkts_toclient', opp = 'sum' ,icon_name = "reply", label = 'Packets to Client')
  # output$tls_map.value.4   <- renderValueBox_mapvalue(event_data = tls_data, event_type = "tls", tab_name_suffix = '_map' , leafletId_suffix = "_leaflet", value_column = 'tls.pkts_toserver', opp = 'sum' ,icon_name = "question-circle",label = 'Packets to Server')
}


auth0_server( shiny_server, info = a0_info)  
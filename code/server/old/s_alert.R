### Alert 

alert_data <- alertData(event_stream, max_age_secs, event_type = "alert")

alert_request_count <- requestCount(event_stream,event_type='alert')
alert_destination_count <- destinationCount(event_stream,event_type='alert')
alert_bytes_total <-  totalBytes(event_stream,event_type='alert')

output$alert.rate <- renderValueBox({
  # The downloadRate is the number of rows in alert_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(alert_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("Alerts per sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart")
    # ,
    # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$alert.destinations <- renderValueBox({
  valueBox(
    alert_destination_count(),
    "Unique alert Desintations",
    icon = icon("desktop")
  )
})


output$alert.requests <- renderValueBox({
  valueBox(
    alert_request_count(),
    "Total alert Volume (requests)",
    icon = icon("window-restore")
  )
})


output$alert.bytes <- renderValueBox({
  valueBox(
    round(alert_bytes_total()/(1024*1024),1),
    "Total Web Volume (MB)",
    icon = icon("window-restore")
  )
})


output$alert.report_period_text <- renderText({
  time_period <- all_data()  %>%
    summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
  
  format(round(time_period$max_timestamp - time_period$min_timestamp,2))
})


output$alert.report_period <- renderValueBox({
  
  time_period <- all_data()  %>%
    summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
  
  period_text <- format(round(time_period$max_timestamp - time_period$min_timestamp,2))
  
  valueBox(
    period_text,
    "Current Report Period Length",
    icon = icon("clock")
  )
})



output$alert.destination.bubbleplot <- output$alert.src_ip.bubbleplot <- renderBubbles({
  if (nrow(alert_data()) == 0)
    return()
  
  order <- unique(alert_data()$src_ip)
  df <- alert_data() %>%
    group_by(src_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(src_ip)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(40)
  
  bubbles(df$n, df$src_ip, key = df$src_ip)
})

output$alert.destination.table <- output$alert.src_ip.table <- renderTable({
  alert_data() %>%
    group_by(src_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(src_ip)) %>%
    mutate(percentage = n / nrow(alert_data()) * 100) %>%
    select("Source IP" = src_ip, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))

output$alert.raw <- renderPrint({
  #orig <- options(width = 1000)
  alert_data(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    select( timestamp,
            flow_id,
            in_iface,
            src_ip,
            src_port,
            dest_ip,
            dest_port, 
            proto,
            starts_with("alert")) %>%
    #tail(input$maxrows) %>%
    print(row.names = FALSE)  
  
  #options(orig)
})



output$alert.table <- renderDT({
  updateSliderTextInput(session,"data_refresh_rate",selected = 120) 
  
  alert_data() %>% 
    mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE))
           # ,alert.start = as_datetime(alert.start, tz = Sys.timezone(location = TRUE)),  
           # ,alert.end = as_datetime(alert.end, tz = Sys.timezone(location = TRUE))  
    ) %>%
    select(-flow_id, -event_type, -host) %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
  }, 
class = "display nowrap compact", # style
filter = "top", # location of column filters
options = list(scrollX = TRUE)
)



output$alert.download_csv <- downloadHandler(
  filename = "alert.csv",
  
  content = function(file) {
      alert_data() %>%
      remove_empty(which = c("rows", "cols")) %>%
      # tail(input$maxrows) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)

output$alert.app_proto_server_bytes.barplot <- renderPlotly({
  df <- alert_data()
  if (nrow(df) == 0)
    return()
  
  df$time <- df$timestamp %>% 
    as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
    floor_date(unit = "seconds")
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps" = round(sum(alert.bytes_toserver)/131072,),3) %>%
    ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})

output$alert.app_proto_client_bytes.barplot <- renderPlotly({
  df <- alert_data()
  if (nrow(df) == 0)
    return()
  
  df$time <- df$timestamp %>% 
    as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
    floor_date(unit = "seconds")
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps" = round(sum(alert.bytes_toclient)/131072,),3) %>%
    ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})


output$alert.app_proto_server_bytes2.barplot <- renderPlotly({
  df <- alert_data()
  if (nrow(df) == 0)
    return()

  ######
  # spread value over seconds in datetime range
  df_results <- data.frame(
    time = numeric(),
    app_proto = character(),
    bytes_toserver = numeric(),
    bytes_toserver = numeric()
  )
  
  for( i in (1:length(df))){
    #df_row <- df[3,]
    df_spread <- (as.numeric(as_datetime(df$alert.start[i]))):(as.numeric(as_datetime(df$alert.end[i]))) %>% 
      as.data.frame(col.names = c('time')) 
    names(df_spread) <- 'time'
    df_spread$app_proto <- df$app_proto[i]
    df_spread$bytes_toserver <- (df$alert.bytes_toserver[i] / length(df_spread))
    df_spread$bytes_toclient <- (df$alert.bytes_toclient[i] / length(df_spread))
    df_results <- rbind(df_results,df_spread)
  }
  df_results$time <- as_datetime(df_results$time, origin = lubridate::origin, tz = Sys.timezone(location = TRUE))
###########  
  
  df <- df_results
  rm(df_results)
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps to server" = sum(bytes_toserver)/131072,
              "Mbps to client" = sum(bytes_toclient)/131072,) %>%
    ggplot( aes(x=time, y="Mbps to server", colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})
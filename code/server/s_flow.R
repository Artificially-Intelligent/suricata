### FLOW 

flow_data <- alertData(alert_stream, max_age_secs, event_type = "flow")

flow_request_count <- requestCount(alert_stream,event_type='flow')
flow_destination_count <- destinationCount(alert_stream,event_type='flow')
flow_bytes_total <-  totalBytes(alert_stream,event_type='flow')

output$flow.rate <- renderValueBox({
  # The downloadRate is the number of rows in flow_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(flow_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("Flow request per sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart")
    # ,
    # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$flow.destinations <- renderValueBox({
  valueBox(
    flow_destination_count(),
    "Unique flow Desintations",
    icon = icon("desktop")
  )
})


output$flow.requests <- renderValueBox({
  valueBox(
    flow_request_count(),
    "Total flow Volume (requests)",
    icon = icon("window-restore")
  )
})


output$flow.bytes <- renderValueBox({
  valueBox(
    round(flow_bytes_total()/(1024*1024),1),
    "Total Web Volume (MB)",
    icon = icon("window-restore")
  )
})


output$flow.dest_ip.bubbleplot <- renderBubbles({
  if (nrow(flow_data()) == 0)
    return()
  
  order <- unique(flow_data()$dest_ip)
  df <- flow_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$dest_ip, key = df$dest_ip)
})



output$flow.dest_ip.table <- renderTable({
  flow_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    mutate(percentage = n / nrow(flow_data()) * 100) %>%
    select("Destination IP" = dest_ip, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))


output$flow.raw <- renderPrint({
  #orig <- options(width = 1000)
  flow_data(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    select( timestamp,
            flow_id,
            in_iface,
            src_ip,
            src_port,
            dest_ip,
            dest_port, 
            proto,
            starts_with("flow")) %>%
    #tail(input$maxrows) %>%
    print(row.names = FALSE)  
  
  #options(orig)
})



output$flow.table <- renderDT({
  flow_data() %>% 
    mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE)),  
           flow.start = as_datetime(flow.start, tz = Sys.timezone(location = TRUE)),  
           flow.end = as_datetime(flow.end, tz = Sys.timezone(location = TRUE))  
    ) %>%
    select(-flow_id,-event_type,-host) %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
  
  }, 
class = "display nowrap compact", # style
filter = "top", # location of column filters
options = list(scrollX = TRUE)
)



output$flow.download_csv <- downloadHandler(
  filename = "flow.csv",
  
  content = function(file) {
      flow_data() %>%
      remove_empty(which = c("rows", "cols")) %>%
      # tail(input$maxrows) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)

output$flow.app_proto_server_bytes.barplot <- renderPlotly({
  df <- flow_data()
  if (nrow(df) == 0)
    return()
  
  df$time <- df$timestamp %>% 
    as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
    floor_date(unit = "seconds")
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps" = round(sum(flow.bytes_toserver)/131072,),3) %>%
    ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})

output$flow.app_proto_client_bytes.barplot <- renderPlotly({
  df <- flow_data()
  if (nrow(df) == 0)
    return()
  
  df$time <- df$timestamp %>% 
    as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
    floor_date(unit = "seconds")
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps" = round(sum(flow.bytes_toclient)/131072,),3) %>%
    ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})


output$flow.app_proto_server_bytes2.barplot <- renderPlotly({
  df <- flow_data()
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
    df_spread <- (as.numeric(as_datetime(df$flow.start[i]))):(as.numeric(as_datetime(df$flow.end[i]))) %>% 
      as.data.frame(col.names = c('time')) 
    names(df_spread) <- 'time'
    df_spread$app_proto <- df$app_proto[i]
    df_spread$bytes_toserver <- (df$flow.bytes_toserver[i] / length(df_spread))
    df_spread$bytes_toclient <- (df$flow.bytes_toclient[i] / length(df_spread))
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
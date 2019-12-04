### netflow 

netflow_data <- alertData(alert_stream, max_age_secs, event_type = "netflow")

netflow_request_count <- requestCount(alert_stream,event_type='netflow')
netflow_destination_count <- destinationCount(alert_stream,event_type='netflow')
netflow_bytes_total <-  totalBytes(alert_stream,event_type='netflow')

output$netflow.rate <- renderValueBox({
  # The downloadRate is the number of rows in netflow_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(netflow_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("netflow request/sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart")
    # ,
    # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$netflow.destinations <- renderValueBox({
  valueBox(
    netflow_destination_count(),
    "Unique netflow Desintations",
    icon = icon("desktop")
  )
})


output$netflow.requests <- renderValueBox({
  valueBox(
    netflow_request_count(),
    "Total netflow Volume (requests)",
    icon = icon("window-restore")
  )
})


output$netflow.bytes <- renderValueBox({
  valueBox(
    round(netflow_bytes_total()/(1024*1024),1),
    "Total Web Volume (MB)",
    icon = icon("window-restore")
  )
})


output$netflow.app_proto_bytes.barplot <- renderPlotly({
  df <- netflow_data()
  if (nrow(df) == 0)
    return()
  
  df$time <- df$timestamp %>% 
    as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
    floor_date(unit = "seconds")
  
  p <- df %>%
    group_by(time,app_proto) %>%
    summarise("Mbps" = round(sum(netflow.bytes)/131072,),3) %>%
    ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
    geom_col() +
    #  geom_area(alpha=0.5) +
    ylab("Mbps") + xlab("Time") + 
    theme_ipsum()
  p
})

output$netflow.dest_ip.bubbleplot <- renderBubbles({
  if (nrow(netflow_data()) == 0)
    return()
  
  order <- unique(netflow_data()$dest_ip)
  df <- netflow_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$dest_ip, key = df$dest_ip)
})



output$netflow.dest_ip.table <- renderTable({
  netflow_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    mutate(percentage = n / nrow(netflow_data()) * 100) %>%
    select("Destination IP" = dest_ip, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))


output$netflow.raw <- renderPrint({
  #orig <- options(width = 1000)
  netflow_data(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    select( timestamp,
            netflow_id,
            in_iface,
            src_ip,
            src_port,
            dest_ip,
            dest_port, 
            proto,
            starts_with("netflow")) %>%
    #tail(input$maxrows) %>%
    print(row.names = FALSE)  
  
  #options(orig)
})



output$netflow.table <- renderDT({
  netflow_data() %>% 
    mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE))  ) %>%
    select(-flow_id,-event_type,-host) %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
}, 
class = "display nowrap compact", # style
filter = "top", # location of column filters
options = list(scrollX = TRUE)
)



output$netflow.download_csv <- downloadHandler(
  filename = "netflow.csv",
  
  content = function(file) {
      netflow_data() %>%
      remove_empty(which = c("rows", "cols")) %>%
      # tail(input$maxrows) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)
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
    icon = icon("area-chart"),
    color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
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



output$flow.table <- renderTable({
  flow_data() %>% 
    tail(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    # select( timestamp,
    #         flow_id,
    #         src_ip,
    #         src_port,
    #         dest_ip,
    #         dest_port, 
    #         proto, starts_with("flow")) %>%
    as.data.frame()
  
}, digits = 1)



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
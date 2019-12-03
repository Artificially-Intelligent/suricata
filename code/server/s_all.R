### all 

all_data <- alertData(alert_stream, max_age_secs)

all_request_count <- requestCount(alert_stream)
all_destination_count <- destinationCount(alert_stream)
all_bytes_total <-  totalBytes(alert_stream)

output$all.rate <- renderValueBox({
  # The downloadRate is the number of rows in all_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(all_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("events per sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart"),
    color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$all.destinations <- renderValueBox({
  valueBox(
    all_destination_count(),
    "Unique all Desintations",
    icon = icon("desktop")
  )
})


output$all.requests <- renderValueBox({
  valueBox(
    all_request_count(),
    "Total Event Volume (requests)",
    icon = icon("window-restore")
  )
})


output$all.bytes <- renderValueBox({
  valueBox(
    round(all_bytes_total()/(1024*1024),1),
    "Total Event Volume (MB)",
    icon = icon("window-restore")
  )
})


output$all.dest_ip.bubbleplot <- renderBubbles({
  if (nrow(all_data()) == 0)
    return()
  
  order <- unique(all_data()$dest_ip)
  df <- all_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$dest_ip, key = df$dest_ip)
})

output$all.dest_ip.table <- renderTable({
  all_data() %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    mutate(percentage = n / nrow(all_data()) * 100) %>%
    select("Destination IP" = dest_ip, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))

output$all.event_count.table <- renderTable({
  all_data() %>%
    group_by(event_type) %>%
    tally() %>%
    arrange(desc(n), tolower(event_type)) %>%
    mutate(percentage = n / nrow(all_data()) * 100) %>%
    select("Event Type" = event_type, "% of events" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))

output$all.raw <- renderPrint({
  #orig <- options(width = 1000)
  all_data(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    select( timestamp,
            all_id,
            in_iface,
            src_ip,
            src_port,
            dest_ip,
            dest_port, 
            proto,
            starts_with("all")) %>%
    #tail(input$maxrows) %>%
    print(row.names = FALSE)  
  
  #options(orig)
})

output$all.table <- renderTable({
  all_data() %>% 
    tail(input$maxrows) %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
  
}, digits = 1)



output$all.download_csv <- downloadHandler(
  filename = "all.csv",
  
  content = function(file) {
      all_data() %>%
      remove_empty(which = c("rows", "cols")) %>%
      # tail(input$maxrows) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)
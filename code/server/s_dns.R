### DNS 

dns_data <- alertData(alert_stream, max_age_secs, event_type = "dns")

dns_request_count <- requestCount(alert_stream,event_type='dns')
dns_destination_count <- destinationCount(alert_stream,event_type='dns')
dns_bytes_total <-  totalBytes(alert_stream,event_type='dns')

output$dns.rate <- renderValueBox({
  # The downloadRate is the number of rows in dns_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(dns_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("DNS request per sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart"),
    color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$dns.destinations <- renderValueBox({
  valueBox(
    dns_destination_count(),
    "Unique DNS Desintations",
    icon = icon("desktop")
  )
})


output$dns.requests <- renderValueBox({
  valueBox(
    dns_request_count(),
    "Total DNS Volume (requests)",
    icon = icon("window-restore")
  )
})


output$dns.bytes <- renderValueBox({
  valueBox(
    round(dns_bytes_total()/(1024*1024),1),
    "Total Web Volume (MB)",
    icon = icon("window-restore")
  )
})


output$dns.rrname.bubbleplot <- renderBubbles({
  if (nrow(dns_data()) == 0)
    return()
  
  order <- unique(dns_data()$dns.rrname)
  df <- dns_data() %>%
    group_by(dns.rrname) %>%
    tally() %>%
    arrange(desc(n), tolower(dns.rrname)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$dns.rrname, key = df$dns.rrname)
})
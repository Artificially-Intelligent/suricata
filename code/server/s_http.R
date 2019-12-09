### HTTP   

# http_data is a reactive expression that accumulates previous
# values of pkgStream, discarding any that are older than
# max_age_secs.
http_data <- alertData(alert_stream, max_age_secs, event_type = "http")

http_success_request_count <- httpSuccessCount(alert_stream)

http_request_count <- requestCount(alert_stream,event_type='http')
http_destination_count <- destinationCount(alert_stream,event_type='http')
http_bytes_total <-  totalBytes(alert_stream,event_type='http')

output$http.rate <- renderValueBox({
  # The downloadRate is the number of rows in http_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(http_data()) / min(max_age_secs, elapsed)
  
  print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("Web request per sec (last",max_age_minutes, "min)"),
    icon = icon("area-chart")
    # ,
    # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})

output$http.destinations <- renderValueBox({
  valueBox(
    http_destination_count(),
    "Unique Web Desintations",
    icon = icon("desktop")
  )
})


output$http.requests <- renderValueBox({
  valueBox(
    http_request_count(),
    "Total Web Volume (requests)",
    icon = icon("window-restore")
  )
})

output$http.status.pct <- renderValueBox({
  pct_successful <- round(100 * http_success_request_count() / http_request_count(),2)
  valueBox(
    paste(pct_successful,"%"),
    "% successful (2xx) http requests",
    icon = icon("window-restore"),
    color = if (pct_successful >= 95) "aqua" else (if (pct_successful >= 90) "yellow" else "red")
  )
})

output$http.bytes <- renderValueBox({
  valueBox(
    round(http_bytes_total()/(1024*1024),1),
    "Total Web Volume (MB)",
    icon = icon("window-restore")
  )
})

output$http.hostname.bubbleplot <- renderBubbles({
  if (nrow(http_data()) == 0)
    return()
  
  order <- unique(http_data()$http.hostname)
  df <- http_data() %>%
    group_by(http.hostname) %>%
    tally() %>%
    arrange(desc(n), tolower(http.hostname)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$http.hostname, key = df$http.hostname)
})


output$http.hostname.table <- renderTable({
  http_data() %>%
    group_by(http.hostname) %>%
    tally() %>%
    arrange(desc(n), tolower(http.hostname)) %>%
    mutate(percentage = n / nrow(http_data()) * 100) %>%
    select("Host name" = http.hostname, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))


output$http.download_csv <- downloadHandler(
  filename = "http.csv",
  
  content = function(file) {
    http_data() %>%
      #tail(input$maxrows) %>%
      remove_empty(which = c("rows", "cols")) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)

output$http.raw <- renderPrint({
  #orig <- options(width = 1000)
  http_data() %>%
  tail(input$maxrows) %>%
  remove_empty(which = c("cols")) %>%
  # select( timestamp,
  #         flow_id,
  #         in_iface,
  #         src_ip,
  #         src_port,
  #         dest_ip,
  #         dest_port, 
  #         proto,
  #         starts_with("http")) %>%
  print(row.names = FALSE)  
  
  #options(orig)
})



output$http.table <- renderDT({
  updateSliderTextInput(session,"data_refresh_rate",selected = 120) 
  
  http_data() %>% 
    mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE))  ) %>%
    select(-flow_id,-event_type,-host) %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
}, 
class = "display nowrap compact", # style
filter = "top", # location of column filters
options = list(scrollX = TRUE)
)

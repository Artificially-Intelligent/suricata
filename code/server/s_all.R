### all 

all_data <- alertData(event_stream, max_age_secs)
data$events_all <- all_data

all_request_count <- requestCount(event_stream)
all_destination_count <- destinationCount(event_stream)
all_bytes_total <-  totalBytes(event_stream)

output$all.rate <- renderValueBox({
  # The downloadRate is the number of rows in all_data since
  # either first_timestamp or max_age_secs ago, whichever is later.
  elapsed <- (data$events_all() %>%
    summarise('report_period' = max(timestamp_num) - min(timestamp_num)))[[1]]
    
  as.numeric(Sys.time()) - first_timestamp()
  download_rate <- nrow(data$events_all() ) / min(max_age_secs, elapsed)
  
  valueBox(
    value = formatC(download_rate, digits = 1, format = "f"),
    subtitle = paste("Events/s (last", round(min(max_age_secs, elapsed)/60,0), "min)"),
    icon = icon("area-chart")
    #,
    #color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
  )
})


output$all.report_period_text <- renderText({
  time_period <- data$events_all()  %>%
    summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
  
    format(round(time_period$max_timestamp - time_period$min_timestamp,2))
})


output$all.report_period <- renderValueBox({

    time_period <- data$events_all()  %>%
    summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
    
    period_text <- format(round(time_period$max_timestamp - time_period$min_timestamp,2))

  valueBox(
    period_text,
    "Current Report Period Length",
    icon = icon("clock")
  )
})

output$all.destinations <- renderValueBox({
  valueBox(
    all_destination_count(),
    "Unique all Desintations",
    icon = icon("desktop")
  )
})


# output$all.requests <- renderValueBox({
#   valueBox(
#     all_request_count(),
#     "Total Event Volume (Since Report Opened)",
#     icon = icon("window-restore")
#   )
# })


output$all.requests <- renderValueBox({
  valueBox(
    data$events_all()  %>%
      summarise('requests' = n()),
    #all_request_count(),
    "Total Event Volume",
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

output$all.destination.bubbleplot <- output$all.dest_ip.bubbleplot <- renderBubbles({
  if (nrow(data$events_all() ) == 0)
    return()
  
  order <- unique(data$events_all() $dest_ip)
  df <- data$events_all()  %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    # Just show the top 60, otherwise it gets hard to see
    head(60)
  
  bubbles(df$n, df$dest_ip, key = df$dest_ip)
})

output$all.destination.table <- output$all.dest_ip.table <- renderTable({
  data$events_all()  %>%
    group_by(dest_ip) %>%
    tally() %>%
    arrange(desc(n), tolower(dest_ip)) %>%
    mutate(percentage = n / nrow(data$events_all() ) * 100) %>%
    select("Destination IP" = dest_ip, "% of requests" = percentage) %>%
    as.data.frame() %>%
    head(15)
}, digits = 1, options = list(scrollX = TRUE))

output$all.event_count.table <- renderTable({
  data$events_all()  %>%
    group_by(event_type) %>%
    tally() %>%
    arrange(desc(n), tolower(event_type)) %>%
    mutate(percentage = n / nrow(data$events_all() ) * 100) %>%
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

output$all.table <- renderDT({
  updateSliderTextInput(session,"data_refresh_rate",selected = 120) 
  data$events_all()  %>%
    remove_empty(which = c("rows", "cols")) %>%
    as.data.frame()
  }, 
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(scrollX = TRUE)
)



output$all.download_csv <- downloadHandler(
  filename = "all.csv",
  
  content = function(file) {
      data$events_all()  %>%
      remove_empty(which = c("rows", "cols")) %>%
      # tail(input$maxrows) %>%
      write.csv(file)
  },
  contentType = "text/csv"
)
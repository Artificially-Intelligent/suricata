### FLOW 

# event_data <- alertData(event_stream, max_age_secs, event_type = "flow")
# 
# flow_request_count <- requestCount(event_stream,event_type='flow')
# flow_destination_count <- destinationCount(event_stream,event_type='flow')
# flow_bytes_total <-  totalBytes(event_stream,event_type='flow')



# output$flow.rate <- 
renderValueBox_rate <- function(event_data = all_data, event_type = "all"){
  renderValueBox({
    # The downloadRate is the number of rows in flow_data since
    # either first_timestamp or max_age_secs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - first_timestamp()
    download_rate <- nrow(event_data()) / min(max_age_secs, elapsed)
    
    print(paste("first timestamp:",first_timestamp(),"elapsed:",min(max_age_secs, elapsed)))
    
    valueBox(
      value = formatC(download_rate, digits = 1, format = "f"),
      subtitle = paste( event_type , "request per sec (last",max_age_minutes, "min)"),
      icon = icon("area-chart")
      # ,
      # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
}

# output$flow.destinations <-
renderValueBox_destinations <- function( event_stream = event_stream,event_type = "all"){
  destination_count <- destinationCount(event_stream, event_type)
   renderValueBox({
    valueBox(
      destination_count(),
      paste("Unique", event_type ,"Desintations"),
      icon = icon("desktop")
    )
  })
}

# output$flow.requests <- 
renderValueBox_requests <- function(event_stream = event_stream,  event_type = "all"){
  request_count <- requestCount(event_stream,event_type)
  renderValueBox({
    valueBox(
      request_count(),
      paste("Total", event_type ,"Volume (requests)"),
      icon = icon("window-restore")
    )
  })
}

# output$flow.bytes <- 
renderValueBox_requests <- function(event_stream = event_stream, event_type = "all"){
  bytes_total <-  totalBytes(event_stream,event_type)
  renderValueBox({
    valueBox(
      round(bytes_total()/(1024*1024),1),
      paste("Total",event_type , "Volume (MB)"),
      icon = icon("window-restore")
    )
  })
}

# output$flow.report_period_text <- 
renderText_report_period_text <- function(event_data = all_data, event_type = "all"){
  renderText({
    time_period <- event_data()  %>%
      summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
    
    format(round(time_period$max_timestamp - time_period$min_timestamp,2))
  })
}

# output$flow.report_period <- 
renderText_report_period <- function(event_data = all_data, event_type = "all"){
  renderValueBox({
    time_period <- event_data()  %>%
      summarise('min_timestamp' = as_datetime(min(timestamp_num)), 'max_timestamp' = as_datetime(max(timestamp_num)))
    period_text <- format(round(time_period$max_timestamp - time_period$min_timestamp,2))
    valueBox(
      period_text,
      "Current Report Period Length",
      icon = icon("clock")
    )
  })
}

# output$flow.destination.bubbleplot <- output$flow.dest_ip.bubbleplot <- 
renderBubbles_dest_ip <- function(event_data = all_data, event_type = "all"){
  renderBubbles({
    if (nrow(event_data()) == 0)
      return()
    
    order <- unique(event_data()$dest_ip)
    df <- flow_data() %>%
      group_by(dest_ip) %>%
      tally() %>%
      arrange(desc(n), tolower(dest_ip)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(40)
    
    bubbles(df$n, df$dest_ip, key = df$dest_ip)
  })
}

# output$flow.destination.table <- output$flow.dest_ip.table <- 
renderTable_dest_ip <- function(event_data = all_data, event_type = "all"){
  renderTable({
    event_data() %>%
      group_by(dest_ip) %>%
      tally() %>%
      arrange(desc(n), tolower(dest_ip)) %>%
      mutate(percentage = n / nrow(flow_data()) * 100) %>%
      select("Destination IP" = dest_ip, "% of requests" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1, options = list(scrollX = TRUE))
}

# output$flow.raw <- 
renderPrint_raw <- function(event_data = all_data, event_type = "all"){
  renderPrint({
    #orig <- options(width = 1000)
      event_data() %>%
      remove_empty(which = c("rows", "cols")) %>%
      # select( timestamp,
      #         flow_id,
      #         in_iface,
      #         src_ip,
      #         src_port,
      #         dest_ip,
      #         dest_port, 
      #         proto,
      #         starts_with(event_type)) %>%
      #tail(input$maxrows) %>%
      print(row.names = FALSE)  
    
    #options(orig)
  })
}



# output$flow.table <- 
renderDT_table <- function(event_data = all_data, event_type = "all"){
  renderDT({
    updateSliderTextInput(session,"data_refresh_rate",selected = 120) 
    
      event_data() %>% 
      mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE))
             # ,flow.start = as_datetime(flow.start, tz = Sys.timezone(location = TRUE))
             # ,flow.end = as_datetime(flow.end, tz = Sys.timezone(location = TRUE))  
      ) %>%
      select(-flow_id,-event_type,-host) %>%
      remove_empty(which = c("rows", "cols")) %>%
      as.data.frame()
    
    }, 
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(scrollX = TRUE)
  )
}

# output$flow.download_csv <- 
downloadHandler_csv <- function(event_data = all_data, event_type = "all"){
  downloadHandler(
    filename = paste(event_type,".csv"),
    
    content = function(file) {
        event_data() %>%
        remove_empty(which = c("rows", "cols")) %>%
        write.csv(file)
    },
    contentType = "text/csv"
  )
}

# output$flow.app_proto_server_bytes.barplot <- 
renderPlotly_app_proto_server_bytes.barplot <- function(event_data = all_data, event_type = "all"){
  renderPlotly({
    df <- event_data()
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
}

# output$flow.app_proto_client_bytes.barplot <- 
renderPlotly_app_proto_client_bytes.barplot <- function(event_data = all_data, event_type = "all"){
  renderPlotly({
    df <- event_data()
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
}

# output$flow.app_proto_server_bytes2.barplot <- 
renderPlotly_app_proto_server_averaged_bytes.barplot <- function(event_data = all_data, event_type = "all"){
  renderPlotly({
    df <- event_data()
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
}


# output$http_map_leaflet <- 
renderLeaflet_map_destination <- function(event_data = all_data, event_type = "all", color_column = 'event_type'){
  renderLeaflet({
    #user_settings <- user_settings()
    df <- mapData(event_data(),event_type = event_type, color_column = color_column)
    
    if(! is.null(df)){
      
      if(! is.null(isolate(u$last_lng_bounds)) && ! is.null(isolate(u$last_lat_bounds))){
        lat_bounds <- isolate(u$last_lat_bounds)
        lng_bounds <- isolate(u$last_lng_bounds)
      }else{
        lat_bounds <- c(max(c(df$lat)), min(c(df$lat)))
        lng_bounds <- c(max(c(df$long)), min(c(df$long)))
      }
      
      # if only one map item
      if(lat_bounds[1]-lat_bounds[2] == 0 || lng_bounds[1]-lng_bounds[2] == 0 ){
        print( 'only one map entry, expanding map bounds')
        lat_bounds <- lat_bounds + c(1,-1)
        lng_bounds <- lng_bounds + c(1,-1)
      }
      # browser()
      
      current_map_bounds_var <- paste('input$', event_type,'_map_leaflet_bounds',sep='')
      bounds <- isolate(eval(parse(text = current_map_bounds_var)))
      if(!is.null(bounds)){
        lat_bounds <- c(bounds$north,bounds$south)
        lng_bounds <- c(bounds$east,bounds$west)
      }
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addMapPane("top_circles", zIndex = 430) %>%
        
        #addOverlays_abs(overlay_groups, poa_shapes, abs_shapes) %>%
        fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2]) %>%
        addCircles_f(df) 
      
      m
    }else{
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addMapPane("top_circles", zIndex = 430)
    }
  })
}

leaflet_mapdata <- function(event_data = all_data, event_type = "all"){
  df <- event_data() %>%
    mutate(country_name = dest_country_name, country_code = dest_country_code, city = dest_city, 
                            ip = dest_ip, long = dest_long,lat = dest_lat)

    current_map_bounds_var <- paste('input$', event_type,'_map_leaflet_bounds',sep='')
    bounds <- eval(parse(text = current_map_bounds_var))
    if (is.null(bounds)){
      df
    } else {
      in_bounding_box(df, bounds)
    }
}

# 
# renderValueBox_mapvalue_count <- function(event_data = all_data, event_type = "all", value_column = 'event_type', value = ''){
#   renderValueBox({
#     dt <- leaflet_mapdata(event_data = event_data, event_type = event_type) 
#     
#     valueBox(
#       length(Filter(Negate(is.null), dt[,value_column])),
#       value_column,
#       icon = icon("chart-line")
#     )
#   })
#   
# }

renderValueBox_mapvalue_count <- function(event_data = all_data, event_type = "all", value_column = 'event_type', value = '', filter_column = '', filter_value = '', unique_count = FALSE, icon_name = "chart-line"){
  renderValueBox({
    actual_value_column <- value_column
    label <- value_column
    if(nchar(value) > 0)
      label <- paste(value,label)
    if(unique_count)
      label <- paste("unique",label)

    dt <- leaflet_mapdata(event_data = event_data, event_type = event_type)
    
    #dt <- Filter(Negate(is.null), dt[,c(value_column,filter_column)])
    
    if(nrow(dt) == 0){
      row_count <- 0
    }else{
      if(nchar(filter_column) > 0 && nchar(filter_value) > 0){
        dt <-  dt[,c(value_column,filter_column)]
        if(nrow(dt)>0)
          dt <- dt[dt[filter_column] == filter_value,] %>%
            remove_empty(which = c("rows", "cols")) 
        dt$value_count <- 1
      }else{
        dt <-  dt[,c(value_column,'flow_id')]
        dt$flow_id <- NA
        if(typeof(dt[,value_column]) == 'list'){
           # browser()
          dt <- dt[unlist(lapply(dt[,value_column], (function(x) { !is.null(x)}))),]
          dt$value_count <- unlist(lapply(dt[,value_column], (function(x) { if(!is.null(x)){nrow(x)}else{0}})))
          
          if(nrow(dt) > 0 && typeof(dt[,value_column]) == 'list')
            if(length(names(dt[,value_column][[1]])) > 0)
              actual_value_column <- names(dt[,value_column][[1]])[1]
        }else{
          dt <-  dt %>%
            remove_empty(which = c("rows"))
          dt$value_count <- 1
        }
        dt$flow_id <- NULL
        if(nrow(dt) > 0)
          colnames(dt) <- c(actual_value_column,"value_count")  
      }
      
      
      
      if(nchar(value) > 0)
        dt <- dt[dt[,actual_value_column] == value,]
      
      if(value_column != actual_value_column)
        label <- paste(label,actual_value_column,sep='.')
      
      if(unique_count){
        row_count <- length(unique(dt[,actual_value_column]))
      }else{
        if(nrow(dt)==0){
          row_count <- 0
        }else{
          row_count <- sum(dt[,"value_count"])
        }
      }
    }
    
    if(nchar(filter_column) > 0 && nchar(filter_value) > 0)
      label <- paste(label, filter_value)
    
    valueBox(
      row_count,
      label,
      icon = icon(icon_name)
    )
  })
  
}

renderTable_maptable_summary <- function(event_data = all_data, event_type = "all", value_column = 'event_type'){
  renderTable({
    
    dt <- leaflet_mapdata(event_data = event_data, event_type = event_type)
    dt <- Filter(Negate(is.null), dt[,value_column]) 
    
    dt <- data.frame(dt) %>%
      group_by(dt) %>%
      summarise(count = n())
    
    names(dt) <- c(value_column,'count')
    dt
    
    # browser()
    # dt$dns.rrtype
    # dt$dns.rrtype
    # dt$dns.answers.count <- length(dt[,'dns.answers'])
    # p <- ggplot(data = dt, aes(dt$dns.rrtype)) +
    #   geom_bar()
    #   # geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    #   # geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
    # 
    # ggplotly(p)
    # 
    
  }, 
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(scrollX = TRUE)
  )
}



renderDT_maptable_detail <- function(event_data = all_data, event_type = "all"){
  renderDT({
    dt <- leaflet_mapdata(event_data = event_data, event_type = event_type) 
    
    # browser()
    # dt$dns.rrtype
    # dt$dns.rrtype
    # dt$dns.answers.count <- length(dt[,'dns.answers'])
    # p <- ggplot(data = dt, aes(dt$dns.rrtype)) +
    #   geom_bar()
    #   # geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    #   # geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
    # 
    # ggplotly(p)
    # 
    
    dt  %>%
      mutate(timestamp = as_datetime(timestamp, tz = Sys.timezone(location = TRUE))
             # ,flow.start = as_datetime(flow.start, tz = Sys.timezone(location = TRUE))
             # ,flow.end = as_datetime(flow.end, tz = Sys.timezone(location = TRUE))  
      ) %>%
      select(-flow_id,-event_type,-host) %>%
      remove_empty(which = c("rows", "cols")) %>%
      as.data.frame()
    
  }, 
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(scrollX = TRUE)
  )
}

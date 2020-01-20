### EVENTS 



# generate_generic_outputs <- function(event_type = 'flow', event_data = event_data ,event_stream = event_stream, max_age_secs = max_age_secs){
#  browser()
#   # Dashboard
# 
#   output[[paste('output$',event_type,'.rate', sep = '')]] <-  renderValueBox_rate( event_type = event_type, event_data = event_data)
#   
#   
#   output[[paste('output$',event_type,'.destinations', sep = '')]] <- renderValueBox_destinations(event_stream = event_stream, event_type = event_type)
#    # assign(paste('output$',event_type,'.rate', sep = ''), renderValueBox_rate( event_type = event_type, event_data = event_data))
#    # assign(paste('output$',event_type,'.destinations', sep = ''), renderValueBox_destinations(event_stream = event_stream, event_type = event_type))
#    assign(paste('output$',event_type,'.requests', sep = ''), renderValueBox_requests(event_stream = event_stream, event_type = event_type))
#    assign(paste('output$',event_type,'.bytes', sep = ''), renderValueBox_requests(event_stream = event_stream, event_type = event_type))
#    assign(paste('output$',event_type,'.report_period', sep = ''), renderText_report_period(event_data = event_data, event_type = event_type))
#    assign(paste('output$',event_type,'.destination.bubbleplot', sep = ''), renderBubbles_destination(event_data = event_data, event_type = event_type))
#    assign(paste('output$',event_type,'.destination.table', sep = ''), renderTable_dest_ip(event_data = event_data, event_type = event_type))
#   
#   # Table
#    assign(paste('output$',event_type,'.table', sep = ''), renderDT_table(event_data = event_data, event_type = event_type))
#    assign(paste('output$',event_type,'.download_csv', sep = ''), downloadHandler_csv(event_data = event_data, event_type = event_type))
#   
#   # Map
#    assign(paste('output$',event_type,'_map_table_detail', sep = ''), renderDT_maptable_detail(event_data = event_data, event_type = event_type))
#   #  assign(paste('output$',event_type,'_map_leaflet', sep = ''), renderLeaflet_map_destination(event_data = event_data, event_type = event_type, color_column = 'dns.type'))
#   #  assign(paste('output$',event_type,'_map_table_summary', sep = ''), renderTable_maptable_summary(event_data = event_data, event_type = event_type, value_column = 'dns.type'))
#   #  assign(paste('output$',event_type,'_map.value.1', sep = ''), renderValueBox_mapvalue_count(event_data = event_data, event_type = event_type, value_column = 'dns.rrname', filter_column = 'dns.type' ,filter_value = 'query', icon_name = "question-circle"))
#   #  assign(paste('output$',event_type,'_map.value.2', sep = ''), renderValueBox_mapvalue_count(event_data = event_data, event_type = event_type, value_column = 'dns.answers',icon_name = "reply"))
#   #  assign(paste('output$',event_type,'_map.value.3', sep = ''), renderValueBox_mapvalue_count(event_data = event_data, event_type = event_type, value_column = 'dns.rrname', unique_count =  TRUE,icon_name = "reply"))
#   #  assign(paste('output$',event_type,'_map.value.4', sep = ''), renderValueBox_mapvalue_count(event_data = event_data, event_type = event_type, value_column = 'dns.answers', unique_count =  TRUE,icon_name = "reply"))
# }


# # output$event.rate <- 
# renderValueBox_rate <- function(event_count = all_count, event_type = "all"){
#   renderValueBox({
#     
#     # The downloadRate is the number of rows in event_data since
#     # either first_timestamp or max_age_secs ago, whichever is later.
#     
#     elapsed <- round(difftime(Sys.time(), first_timestamp(), units="secs"),1)
#     interval <- min(max_age_secs, elapsed)
#     download_rate <- event_count() / interval
#     
#      if(interval < 120){
#        label <- paste( event_type , "request per sec (last",interval, "sec)")
#      }else{
#        label <- paste( event_type , "request per sec (last",interval, "min)")
#      }
#     
#     print(paste("first timestamp:",first_timestamp(),"elapsed:",interval))
#     
#     valueBox(
#       value = formatC(download_rate, digits = 1, format = "f"),
#       subtitle = label,
#       icon = icon("area-chart")
#       # ,
#       # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
#     )
#   })
# }

# output$event.rate <- 
renderValueBox_rate <- function(event_data = all_data, event_type = "all"){
  # event_count <- latestRequestCount(event_stream, event_type)
  renderValueBox({
    event_count <- requestCount(event_data, event_type)
    
    # The downloadRate is the number of rows in event_data since
    # either first_timestamp or max_age_secs ago, whichever is later.
    elapsed <- round( difftime(last_timestamp(event_data), first_timestamp(event_data), units="secs"),1)
    interval <- min(max_age_secs, elapsed)
    download_rate <- event_count / interval
    
    if(interval < 120){
      label <- paste( event_type , "request per sec (last",interval, "sec)")
    }else{
      label <- paste( event_type , "request per sec (last",interval, "min)")
    }
    
    # print(paste("first timestamp:",first_timestamp(),"elapsed:",interval))
    
    valueBox(
      value = formatC(download_rate, digits = 1, format = "f"),
      subtitle = label,
      icon = icon("area-chart")
      # ,
      # color = if (download_rate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
}



# output$event.destinations <-
renderValueBox_value_count <- function(event_data = all_data, event_type = "all", value_column = 'dest_ip', icon_desc = 'desktop'){
  # destinationCount(event_stream, event_type)
  renderValueBox({
    unique_value_count <- nrow(valueCount(event_data = event_data, event_type = event_type, value_column = value_column)) 
    label <- paste("Unique", simple_cap(str_replace(value_column,'_', ' ')))
    valueBox(
      unique_value_count,
      label,
      icon = icon(icon_desc)
    )
  })
}
# 
# if(event_type == 'http')
#   return(memo + (df %>% 
#                    filter(event_type %in% e_type) %>% 
#                    summarise(total_bytes = sum(http.length)) 
#   )
#   )
# if(event_type == 'fileinfo')
#   return(memo + (df %>% 
#                    filter(event_type %in% e_type) %>% 
#                    summarise(total_bytes = sum(fileinfo.size )) 
#   )
#   )
# if(event_type == 'flow')
#   return(memo + (df %>% 
#                    filter(event_type %in% e_type) %>% 
#                    summarise(total_bytes = sum(flow.bytes_toclient) +  sum(flow.bytes_toserver))
#   )
#   )

renderValueBox_value_agg <- function(event_data = all_data, event_type = "all", value_column = 'dest_ip', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), icon_desc = 'desktop' , agg_function = 'sum'){
  # destinationCount(event_stream, event_type)
  renderValueBox({
    value_sum <- valueAgg(event_data = event_data, event_type = event_type, value_column = value_column, measure_column = measure_column, agg_function = agg_function) 
    
    total_sum <- sum(value_sum[,2:length(value_sum)]) %>% 
      comprss()
    label <- paste(simple_cap(str_replace_all(str_replace_all( paste(collapse = ' + ', measure_column),'_', ' '),"\\.", ' ')))
    valueBox(
      total_sum,
      label,
      icon = icon(icon_desc)
    )
  })
}

# output$event.requests <- 
renderValueBox_requests <- function(event_data = all_data,  event_type = "all"){
  
  renderValueBox({
    request_count <- requestCount(event_data,event_type)
    valueBox(
      request_count,
      paste("Total", event_type ,"Volume (requests)"),
      icon = icon("window-restore")
    )
  })
}

# output$event.bytes <- 
renderValueBox_bytes <- function(event_stream = event_stream, event_type = "all"){
  bytes_total <-  totalBytes(event_stream,event_type)
  renderValueBox({
    valueBox(
      # round(bytes_total()/(1024*1024),1),
      comprss(bytes_total()),
      paste("Total",event_type , "Volume (Bytes)"),
      icon = icon("window-restore")
    )
  })
}

# output$event.report_period_text <- 
renderText_report_period_text <- function(event_data = all_data){
  renderText({
    time_period <- last_timestamp(event_data)  - first_timestamp(event_data)
    period_text <- format(round(time_period,2))
    
    # time_period <- event_data()  %>%
    # period_text <- summarise('min_timestamp' = min(timestamp), 'max_timestamp' = max(timestamp))
    
    period_text
  })
  
}

# output$event.report_period <- 
renderText_report_period <- function(event_data = all_data){
  renderValueBox({
    time_period <- last_timestamp(event_data)  - first_timestamp(event_data)
    period_text <- format(round(time_period,2))
    
    # time_period <- event_data()  %>%
    #   summarise('min_timestamp' = min(timestamp), 'max_timestamp' = max(timestamp))
    # period_text <- format(round(time_period$max_timestamp - time_period$min_timestamp,2))
    valueBox(
      period_text,
      "Current Report Period Length",
      icon = icon("clock")
    )
  })
}

# output$event.destination.bubbleplot <- output$event.dest_ip.bubbleplot <- 
renderBubbles_value <- renderBubbles_destination <- function(event_data = all_data, event_type = "all", value_column = 'dest_ip'){
  renderBubbles({
    
    df <- valueCount(event_data = event_data, event_type = event_type, value_column = value_column)    %>%
      # Just show the top 30, otherwise it gets hard to see
      head(30)
    
    bubbles(df$n, df$value_column, key = df$value_column)
  })
}


renderTable_value <- function(event_data = all_data, event_type = "all", value_column = 'event_type'){
  renderTable({
    total_count <- requestCount(event_data, event_type)
    
    df <- valueCount(event_data = event_data, event_type = event_type, value_column = value_column) 
    column_heading_value = simple_cap(str_replace(value_column,'_', ' '))
    column_heading_measure = "count" 
    column_heading_measure_pct = paste("% by ",column_heading_measure) 
    df <- df %>%
      mutate(percentage = n / total_count * 100) %>%
      select(column_heading_value = value_column, column_heading_measure = n, column_heading_measure_pct = percentage) %>%
      as.data.frame() %>%
      head(15)
    colnames(df) <- c(column_heading_value,column_heading_measure,column_heading_measure_pct)
    df
  }, digits = 1, options = list(scrollX = TRUE))
}


# output$event.raw <- 
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



# output$event.table <- 
renderDT_table <- function(event_data = all_data, event_type = "all"){
  renderDT({
    # updateSliderTextInput(session,"data_refresh_rate",selected = 120) 
    
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

# output$event.download_csv <- 
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


# output$event.app_proto_server_bytes.barplot <- 
renderPlotly_value.barplot <-  function(event_data = all_data, event_type = "all", value_column = 'app_proto', measure_column = c('flow.bytes_toclient','flow.bytes_toserver'), agg_function = 'sum'){
  renderPlotly({
    
    if(! is.null(input$flow.value_picker))
      value_column <- input$flow.value_picker
    if(! is.null(input$flow.measure_picker))
      measure_column <- input$flow.measure_picker
    
    
    df <- valueAgg(event_data = event_data, event_type = event_type, value_column = c('timestamp',value_column), measure_column = measure_column, agg_function = agg_function)

    if (nrow(df) == 0)
      return()
    
    ylab <- simple_cap(paste(agg_function, str_replace_all(str_replace_all( paste(collapse = ' + ', measure_column),'_', ' '),"\\.", ' ')))
    xlab <- 'Time'
    serieslab <- simple_cap(paste(str_replace_all(str_replace_all( paste(collapse = ' + ', value_column),'_', ' '),"\\.", ' ')))
    
    
    unit = 'minutes'
    if(difftime(last_timestamp(event_data), first_timestamp(event_data), units="secs") < 600)
      unit = 'seconds'
    
    df$time <- df$timestamp %>% 
      as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
      floor_date(unit = unit)
    
    df$measure <- rowSums(df[,measure_column]) 
    
    fill_by <- sym(value_column)
    
    df <- df %>%
      group_by_at(c('time',value_column)) %>%
      summarise(measure = sum(measure))
    
    # names(df)[names(df) == value_column[1]] <- 'value_column'
    
    p <- df %>%
      #group_by(time,app_proto) %>%
      #summarise("Mbps" = round(sum(flow.bytes_toserver)/131072,),3) %>%
      ggplot( aes(x=time, y=measure
                  , fill= !!fill_by ,order= measure )) +
      
      #  geom_area(alpha=0.5) +
      # ylab(ylab) + xlab(xlab) +
      labs(fill = serieslab, x = xlab, y = ylab) +
      geom_bar(stat = "identity") +
    # geom_col() +
      theme_ipsum() +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::comma) +
      #scale_fill_manual(values=colour_pallets$cbPalette)
       scale_fill_brewer(palette="Spectral")
      # scale_fill_brewer(palette="Set1")
    p
  })
}

# output$event.app_proto_server_bytes.barplot <- 
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

# output$event.app_proto_client_bytes.barplot <- 
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

# output$event.app_proto_server_bytes2.barplot <- 
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

### Maps

observeEvent_map_button <- function(event_type = "all", leafletId = "_map_leaflet", buttonId = "_zoom_all_button"){
  leafletId <- paste( event_type, leafletId,sep="")
  button_inputId <- paste( "input$",leafletId, buttonId,sep="")
  
  observeEvent(eval(parse(text = button_inputId)), {
    lat <- 0
    lng <- 0
    my_zoom <- 1
    leafletProxy(leafletId) %>%
      flyTo(lng, lat, zoom = my_zoom)
  })
}
renderLeaflet_map_destination <- function(event_data = all_data, event_type = "all", color_column = 'event_type',group_by_src = FALSE){
  renderLeaflet({
    #user_settings <- user_settings()
    df <- mapData(event_data(),event_type = event_type, color_column = color_column,group_by_src = group_by_src)
    
    if(is.numeric(df$color_column)){
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = df$color_column
      )
    }else{
      pal <- colorFactor(
        palette = "YlGnBu",
        domain = as.factor(df$color_column)
      )
    }
    
    df_lat <- df[!is.na( df$lat ), c('lat')]
    df_long <- df[! is.na( df$long ), c('long')]
    
    if(! is.null(df) && nrow(df_lat) > 0 && nrow(df_long) > 0){
      lat_bounds <- c(max(df_lat$lat), min(df_lat$lat))
      lng_bounds <- c(max(df_long$long), min(df_long$long))
      # if only one map item
      if(max(df_lat$lat) == min(df_lat$lat) || max(df_lat$long) == min(df_lat$long) ){
        print( 'only one map entry, expanding map bounds')
        lat_bounds <- lat_bounds + c(1,-1)
        lng_bounds <- lng_bounds + c(1,-1)
      }
      
      current_map_bounds_var <- paste('input$', event_type,'_map_leaflet_bounds',sep='')
      bounds <- isolate(eval(parse(text = current_map_bounds_var)))
      if(!is.null(bounds)){
        lat_bounds <- c(bounds$north,bounds$south)
        lng_bounds <- c(bounds$east,bounds$west)
      }
      
      m <- leaflet() %>%
        # addTiles() %>%
        # addLayersControl(
        #   baseGroups = c("Hide overlays", "Agriculture Employee Ratio", "Mining Employee Ratio"),
        #   options = layersControlOptions(collapsed = TRUE)
        # ) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        # addPolylines(
        #   group = paste(event_type,"map_line", sep = '.'),
        #   lat = ~lat_vector,
        #   lng = ~long_vector,
        #   color = ~pal(gdp_md_est),
        #   fillColor = color,
        #   stroke = TRUE, 
        #   smoothFactor = 1, 
        #   fillOpacity = 0.2
        # ) %>%
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

renderValueBox_mapvalue <- function(event_data = all_data, event_type = "all", value_column = 'event_type', value = '', filter_column = '', filter_value = '', icon_name = "chart-line", opp = 'count', label = NULL){
  renderValueBox({
    auto_label <- value_column
    if(nchar(value) > 0)
      auto_label <- paste(value,auto_label)
    
    auto_label <- paste(opp,auto_label)
    
    dt <- leaflet_mapdata(event_data = event_data, event_type = event_type)
    
    #dt <- Filter(Negate(is.null), dt[,c(value_column,filter_column)])
    
    if(nrow(dt) == 0 || ! opp %in% c('count','unique_count','sum','max','min','mean','median') ){
      result <- 0
      if( ! opp %in% c('count','unique_count','sum','max','min','avg'))
        result <- 'invalid opp selected'
    }else{
      if(nchar(filter_column) > 0 && nchar(filter_value) > 0){
        dt <- dt[dt[filter_column] == filter_value,]
      }
      if(nchar(value) > 0)
        dt <- dt[dt[,value_column] == value,]
      
      value_Vector <- dt[,value_column]
      
      actual_value_column <- value_column
      
      if(typeof(value_Vector)== 'list'){
        value_Vector <- value_Vector[unlist(lapply(value_Vector, (function(x) { !is.null(x)})))]
        # value_Vector <- Filter(Negate(is.null), value_Vector)
        
        if(length(value_Vector) > 0)
          if(! is.null(names(value_Vector[[1]])))
            actual_value_column <- names(value_Vector[[1]])[1]
          
          value_Vector <- unlist(lapply(value_Vector, (function(x) { if(!is.null(x)){(x[1])}else{0}})))
          
      }
      
      if(value_column != actual_value_column)
        auto_label <- paste(auto_label,actual_value_column,sep='.')
      
      result <- switch(opp,
                       'count' = length(value_Vector),
                       'unique_count' = length(unique(value_Vector)),
                       'sum' = sum(as.numeric(value_Vector)),
                       'max' = max(as.numeric(value_Vector)),
                       'min' = min(as.numeric(value_Vector)),
                       'median' = median(as.numeric(value_Vector)),
                       'mean' = mean(as.numeric(value_Vector))
      ) %>% 
        comprss()
      
      
      if(is.null(label))
        label <- auto_label
    }
    
    if(nchar(filter_column) > 0 && nchar(filter_value) > 0)
      auto_label <- paste(auto_label, filter_value)
    
    valueBox(
      format(result, digits=5) ,
      label,
      icon = icon(icon_name)
    )
  })
  
}

# renderValueBox_mapvalue_count <- function(event_data = all_data, event_type = "all", value_column = 'event_type', value = '', filter_column = '', filter_value = '', unique_count = FALSE, icon_name = "chart-line"){
#   renderValueBox({
#     actual_value_column <- value_column
#     label <- value_column
#     if(nchar(value) > 0)
#       label <- paste(value,label)
#     if(unique_count)
#       label <- paste("unique",label)
# 
#     dt <- leaflet_mapdata(event_data = event_data, event_type = event_type)
#     
#     #dt <- Filter(Negate(is.null), dt[,c(value_column,filter_column)])
#     
#     if(nrow(dt) == 0){
#       row_count <- 0
#     }else{
#       if(nchar(filter_column) > 0 && nchar(filter_value) > 0){
#         dt <-  dt[,c(value_column,filter_column)]
#         if(nrow(dt)>0)
#           dt <- dt[dt[filter_column] == filter_value,] %>%
#             remove_empty(which = c("rows", "cols")) 
#         dt$value_count <- 1
#       }else{
#         dt <-  dt[,c(value_column,'flow_id')]
#         dt$flow_id <- NA
#         if(typeof(dt[,value_column]) == 'list'){
#            # browser()
#           dt <- dt[unlist(lapply(dt[,value_column], (function(x) { !is.null(x)}))),]
#           dt$value_count <- unlist(lapply(dt[,value_column], (function(x) { if(!is.null(x)){nrow(x)}else{0}})))
#           
#           if(nrow(dt) > 0 && typeof(dt[,value_column]) == 'list')
#             if(length(names(dt[,value_column][[1]])) > 0)
#               actual_value_column <- names(dt[,value_column][[1]])[1]
#         }else{
#           dt <-  dt %>%
#             remove_empty(which = c("rows"))
#           dt$value_count <- 1
#         }
#         dt$flow_id <- NULL
#         if(nrow(dt) > 0)
#           colnames(dt) <- c(actual_value_column,"value_count")  
#       }
#       
#       
#       
#       if(nchar(value) > 0)
#         dt <- dt[dt[,actual_value_column] == value,]
#       
#       if(value_column != actual_value_column)
#         label <- paste(label,actual_value_column,sep='.')
#       
#       if(unique_count){
#         row_count <- length(unique(dt[,actual_value_column]))
#       }else{
#         if(nrow(dt)==0){
#           row_count <- 0
#         }else{
#           row_count <- sum(dt[,"value_count"])
#         }
#       }
#     }
#     
#     if(nchar(filter_column) > 0 && nchar(filter_value) > 0)
#       label <- paste(label, filter_value)
#     
#     valueBox(
#       row_count,
#       label,
#       icon = icon(icon_name)
#     )
#   })
#   
# }
# 
# renderTable_maptable_summary <- function(event_data = all_data, event_type = "all", value_column = 'event_type'){
#   renderTable({
#     
#     dt <- leaflet_mapdata(event_data = event_data, event_type = event_type)
#     dt <- Filter(Negate(is.null), dt[,value_column]) 
#     
#     dt <- data.frame(dt) %>%
#       group_by(dt) %>%
#       summarise(count = n())
#     
#     names(dt) <- c(value_column,'count')
#     dt
#     
#     # browser()
#     # dt$dns.rrtype
#     # dt$dns.rrtype
#     # dt$dns.answers.count <- length(dt[,'dns.answers'])
#     # p <- ggplot(data = dt, aes(dt$dns.rrtype)) +
#     #   geom_bar()
#     #   # geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
#     #   # geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
#     # 
#     # ggplotly(p)
#     # 
#     
#   }, 
#   class = "display nowrap compact", # style
#   filter = "top", # location of column filters
#   options = list(scrollX = TRUE)
#   )
# }



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

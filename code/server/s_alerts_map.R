
#### Draw the map ####

suricata_data <- reactive({
  r <- redux::hiredis(host = 'unraiden.local')
  r$PING()
  total_entries <- r$LLEN(key='suricata')
  #raw <- r$LRANGE(key='suricata',1,total_entries)
  json_raw <- r$LRANGE(key='suricata',1,2000)
  tmp <- 'raw.json'
  write(unlist(json_raw),tmp)
  rm(json_raw)
  
  parse_json_line <- function(json){
    jsonlite::flatten(as.data.frame(jsonlite::parse_json(json[[1]])))
  }
  
  suricata <- as.data.table(jsonlite::flatten(jsonlite::stream_in(file(tmp))))
  unlink(tmp)
  
  suricata <- cbind(suricata, rgeolocate::ip2location(ips = suricata$dest_ip, file = iplookup_db_file, fields = c('country_code','city','lat','long')))
  
  df <- suricata
  rm(suricata)
  df
})



observe({
  print('refreshing alert data')
    suricata <- suricata_data()
    v$suricata_df <- suricata
      
    # v$netflow_df <- suricata[suricata$event_type == 'netflow',]
    # if(nrow(v$netflow_df) > 0){
    #   v$netflow_df <- v$netflow_df[,which(unlist(lapply(v$netflow_df, function(x)!all(is.na(x))))),with=F]
    # }
    # 
    # v$flow_df <- as.data.table(suricata[suricata$event_type == 'flow',])
    # if(nrow(v$flow_df) > 0){
    #   v$flow_df <-v$flow_df[,which(unlist(lapply(v$flow_df, function(x)!all(is.na(x))))),with=F]
    # }
    # 
    # v$alerts_df <- as.data.table(suricata[suricata$event_type == 'alert',])
    # if(nrow(v$alerts_df) > 0){
    #   v$alerts_df <- v$alerts_df[,which(unlist(lapply(v$alerts_df, function(x)!all(is.na(x))))),with=F]
    # }
    # 
    # v$drop_df <- as.data.table(suricata[suricata$event_type == 'drop',])
    # if( nrow(v$drop_df) > 0){
    #   v$drop_df <- v$drop_df[,which(unlist(lapply(v$drop_df, function(x)!all(is.na(x))))),with=F]
    #   drop <- drop[,which(unlist(lapply(drop, function(x)!all((x == 'NULL'))))),with=F]
    # }
    # 
    # v$dhcp_df <- as.data.table(suricata[suricata$event_type == 'dhcp',])
    # if(nrow(v$dhcp_df) > 0){
    #   v$dhcp_df <- v$dhcp_df[,which(unlist(lapply(v$dhcp_df, function(x)!all(is.na(x))))),with=F]
    #   v$dhcp_df <- v$dhcp_df[,which(unlist(lapply(v$dhcp_df, function(x)!all((x == 'NULL'))))),with=F]
    # }
    # 
    # v$dns_df <- as.data.table(suricata[suricata$event_type == 'dns',])
    # if(nrow(v$dns_df) > 0){
    #   v$dns_df <- v$dns_df[,which(unlist(lapply(v$dns_df, function(x)!all(is.na(x))))),with=F]
    #   v$dns_df <- v$dns_df[,which(unlist(lapply(v$dns_df, function(x)!all((x == 'NULL'))))),with=F]
    # }
    # 
    # v$fileinfo_df <- as.data.table(suricata[suricata$event_type == 'fileinfo',])
    # if(nrow(v$fileinfo_df) > 0){
    #   v$fileinfo_df <- v$fileinfo_df[,which(unlist(lapply(v$fileinfo_df, function(x)!all(is.na(x))))),with=F]
    #   v$fileinfo_df <- v$fileinfo_df[,which(unlist(lapply(v$fileinfo_df, function(x)!all((x == 'NULL'))))),with=F]
    # }
    # 
    # v$tls_df <- as.data.table(suricata[suricata$event_type == 'tls',])
    # if(nrow(v$tls_df) > 0){
    #   v$tls_df <- v$tls_df[,which(unlist(lapply(v$tls_df, function(x)!all(is.na(x))))),with=F]
    # }
    # 
    # v$http_df <- as.data.table(suricata[suricata$event_type == 'http',])
    # if(nrow(v$http_df) > 0){  
    #   v$http_df <- v$http_df[,which(unlist(lapply(v$http_df, function(x)!all(is.na(x) || is.null(x))))),with=F]
    # }
    # 
})

# starting map
output$alert_map <- renderLeaflet({
  print('rendering map step 1')
  #user_settings <- user_settings()
  df <- isolate(v$suricata_df) %>% 
    filter(event_type == "http") %>%
    remove_empty(which = c("rows", "cols"))
  
  print(paste('rows:',nrow(df)))
  
  lat_bounds <- c(max(df$lat), min(df$lat))
  lng_bounds <- c(max(df$long), min(df$long))
  
  #df <- df[df$lat %in% lat_bounds | df$lng %in% lng_bounds, ]
  print('rendering map step 2')
  
  m <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addMapPane("top_circles", zIndex = 430) %>%
    
    #addOverlays_abs(overlay_groups, poa_shapes, abs_shapes) %>%
    fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2]) %>%
    addCircles_f(df) 
  
  # addAwesomeMarkers(layerId = df$property_id,
  #                   #group = "property_markers",  
  #                   icon = ~map_icons[df$pt_domr],
  #                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(clusterJS)),
  #                   popup = popup_f(df) ,
  #                   popupOptions = popupOptions(closeButton = FALSE)
  #                   ) %>%
  # addLegend(position = "bottomright", 
  #           pal = legend_pal, 
  #           values = df$range_days_on_market,
  #           title = "Days on Market",
  #           opacity = 1) 
  
  
##  m <- m %>%
##    setView(lng = isolate(u$last_lng), 
##            lat = isolate(u$last_lat), 
##            zoom = isolate(u$last_zoom)
##    )
  print('rendering map step 3')
  m
})

#### Click on marker to select, map to deselect a property ####

#
# Map click to select a property
#

observeEvent(input$map_marker_click, {
  
  click<-input$map_marker_click
   is_click <- if(is.null(click)) "FALSE" else "TRUE"
   print(paste('clicked on a marker: ', is_click))
  
  if(is.null(click)) return()
  # leafletProxy("map") %>% remove_datatable_click_elements()
  # dataTableProxy("property_list")
  # TODO: select and scroll to row on table
  if(click$id == "alert_list_markers") return()
  
  v$selected_alert_id <- click$id
  
})

#
# Map click sets selected property to "none"
#

observe({
  click<-input$map_click
   is_click <- if(is.null(click)) "FALSE" else "TRUE"
   print(paste('clicked on a map: ', is_click))
  v$selected_alert_id <- "none"
  
})

observeEvent({
  print('saving user settings')
  input$map_zoom
  input$map_center
  print('saving user settings done')
}, {
  print('saving changed user settings')
  
  u$last_lng = input$map_center$lng
  u$last_lat = input$map_center$lat
  u$last_zoom <- input$map_zoom
#  save_user_settings(u)
  
  print(input$map_zoom)
  
  print('saving changed user settings done')
})


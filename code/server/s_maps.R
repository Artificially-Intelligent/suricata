
#### Draw the map ####

# starting map
output$http_map_leaflet <- renderLeaflet({
  
    print('rendering map step 1')
  #user_settings <- user_settings()
  df <-mapData(http_data(),event_type = 'http')
  
  if(nrow(df) > 0){
    
    lat_bounds <- c(max(c(df$lat)), min(c(df$lat)))
    lng_bounds <- c(max(c(df$long)), min(c(df$long)))
    
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

# starting map
output$flow_map_leaflet <- renderLeaflet({
  print('rendering map step 1')
  #user_settings <- user_settings()
  df <-mapData(flow_data(),event_type = 'flow')

  if(nrow(df) > 0){
    
    lat_bounds <- c(max(c(df$lat)), min(c(df$lat)))
    lng_bounds <- c(max(c(df$long)), min(c(df$long)))
    
    print(paste("map rows:",nrow(df)))
    
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addMapPane("top_circles", zIndex = 430) %>%
  
      #addOverlays_abs(overlay_groups, poa_shapes, abs_shapes) %>%
      fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2]) %>%
      addCircles_f(head(df,1000))
  
    m
  }else{
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addMapPane("top_circles", zIndex = 430)
  }
})


# starting map
output$netflow_map_leaflet <- renderLeaflet({

  print('rendering map step 1')
  #user_settings <- user_settings()
  
  df <-mapData(netflow_data(),event_type = 'netflow')

  if(nrow(df) > 0){
    
    lat_bounds <- c(max(c(df$lat)), min(c(df$lat)))
    lng_bounds <- c(max(c(df$long)), min(c(df$long)))
    
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



# starting map
output$alert_map_leaflet <- renderLeaflet({
  
  print('rendering map step 1')
  #user_settings <- user_settings()
  df <-mapData(alert_data(), event_type = 'alert')

  if(! is.null(df) && nrow(df) > 0){
    
  lat_bounds <- c(max(c(df$lat)), min(c(df$lat)))
  lng_bounds <- c(max(c(df$long)), min(c(df$long)))
  
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




#### Click on marker to select, map to deselect a property ####

#
# Map click to select a destination
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
  input$map_zoom
  input$map_center
}, {
  u$last_lng = input$map_center$lng
  u$last_lat = input$map_center$lat
  u$last_zoom <- input$map_zoom
  #  save_user_settings(u)
  
  print(input$map_zoom)
})


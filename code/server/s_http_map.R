
#### Draw the map ####

# starting map
output$alert_map <- renderLeaflet({
  print('rendering map step 1')
  #user_settings <- user_settings()
  df <- http_data()
  
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


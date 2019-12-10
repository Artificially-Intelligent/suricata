# Functions for producing plots from data and parameters

weather_plot_f <- function(df){
  dygraph(df, main = "Weather predictions for 2020") %>%
    dySeries("rainfall", axis = 'y2') %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
}

addCircles_f <- function(map, df){
df_grouped <- df
  map %>% addCircleMarkers(data = df_grouped, 
                           lng = df_grouped$dest_long,
                           lat = df_grouped$dest_lat,
                           layerId = df_grouped$dest_ip,
                           group = "dest_markers",
                           stroke = FALSE,
                           fillOpacity = 0.8,
                           radius = round(4 + (df_grouped$total_bytes_pct*16),0),
                           fillColor = df_grouped$dest_colour,
                           #icon = ~map_icons[df$pt_domr],
                           #clusterOptions = markerClusterOptions(iconCreateFunction=JS(clusterJS)) #,
                           popup = df_grouped$popup_html,
                           popupOptions = popupOptions(closeButton = FALSE), 
                           options = pathOptions(pane = "top_circles")
  )
}


popup_f <- function(df){
  # js <- "javascript:$('#map_view_btn').show();$('#sales_history_box').show();$('#table').show();$('#list_view_btn').hide();$('#map').hide();"
  paste(popup_html_f(df),
        # ,"<a href=", js,  " class ='more_btn'>more..</a>"
        
  )
}

popup_html_f <- function(df){
  summary <- df 
  paste('<div class="alert_details">
          <div class="alert_details_name"><h4>', summary$dest_city, " (", summary$dest_country_code , ')<h4></div>
          <div class="alert_details">', summary$attribute.list.1, '</div>
          <div class="alert_details">', summary$attribute.list.2, '</div>
          <div class="alert_details">', summary$attribute.list.3, '</div>
          <div class="alert_details">', summary$attribute.list.4, '</div>
          <div class="alert_details">IP Address', summary$dest_ip, '</div>
          <div class="alert_details">Requests:', summary$requests, '</div>
          <div class="alert_details">Bytes', summary$bytes, '</div>
          '
  )
  
}

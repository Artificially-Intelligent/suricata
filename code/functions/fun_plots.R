# Functions for producing plots from data and parameters

weather_plot_f <- function(df){
  dygraph(df, main = "Weather predictions for 2020") %>%
    dySeries("rainfall", axis = 'y2') %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
}

addCircles_f <- function(map, df){
  map %>% addCircleMarkers(data = df, 
                           # lng = df$lng,
                           # lat = df$lat,
                           layerId = df$property_id,
                           group = "property_markers",
                           stroke = FALSE,
                           fillOpacity = 0.8,
                           radius = 10,
                           fillColor = df$property_colour,
                           #icon = ~map_icons[df$pt_domr],
                           #clusterOptions = markerClusterOptions(iconCreateFunction=JS(clusterJS)) #,
                           popup = popup_f(df),
                           popupOptions = popupOptions(closeButton = FALSE), 
                           options = pathOptions(pane = "top_circles")
  )
}


popup_f <- function(df){
  js <- "javascript:$('#map_view_btn').show();$('#sales_history_box').show();$('#table').show();$('#list_view_btn').hide();$('#map').hide();"
  paste('<div class="prop_details">
          ',  popup_html_f(df),'
        </div>',
        "<a href=", js,  " class ='more_btn'>more..</a>"
        
  )
}

popup_html_f <- function(df){
  summary <-  df %>% 
      group_by(country_code, city, http.hostname, http.http_method, http.status, http.http_user_agent, dest_ip, dest_port) %>%
      summarize(requests = n(), bytes =sum(http.length))
  paste('<div class="alert_details">
          <div class="alert_details_name"><h4>', summary$city, " (", summary$country_code , ')<h4></div>
          <div class="alert_details">', summary$http.hostname, '</div>
          <div class="alert_details">', summary$http.http_method, '</div>
          <div class="alert_details">', summary$http.status, '</div>
          <div class="alert_details">', summary$http.http_user_agent, '</div>
          <div class="alert_details">', summary$dest_ip, '</div>
          <div class="alert_details">', summary$dest_port, '</div>
          <div class="alert_details">', summary$requests, '</div>
          <div class="alert_details">', summary$bytes, '</div>
          '
  )
  
}

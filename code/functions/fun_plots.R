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
                           lng = df_grouped$long,
                           lat = df_grouped$lat,
                           layerId = df_grouped$ip,
                           group = "dest_markers",
                           stroke = FALSE,
                           fillOpacity = 0.8,
                           radius =  df_grouped$radius,
                           fillColor = df_grouped$color,
                           #icon = ~map_icons[df$pt_domr],
                           #clusterOptions = markerClusterOptions(iconCreateFunction=JS(clusterJS)) ,
                           popup = df_grouped$popup_html,
                           popupOptions = popupOptions(closeButton = FALSE), 
                           options = pathOptions(pane = "top_circles")
  )
}


popup_f <- function(df){
  # js <- "javascript:$('#map_view_btn').show();$('#sales_history_box').show();$('#table').show();$('#list_view_btn').hide();$('#map').hide();"
  paste(popup_html_f(df)
         # ,"<a href=", js,  " class ='more_btn'>more..</a>"
  )
}

clusterJS <- "function (cluster) {    
                        var childCount = cluster.getChildCount(); 
                        var c = ' marker-custom-';  
                        if (childCount < 20) {  
                          c += 'small';  
                        } else if (childCount < 50) {  
                          c += 'medium';  
                        } else { 
                          c += 'large';  
                        }    
                        return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                    
                      }"

in_bounding_box <- function(data, bounds) {
  data %>%
    dplyr::filter(
        lat > bounds$south &
        lat < bounds$north &
        long < bounds$east & 
        long > bounds$west
    )
}

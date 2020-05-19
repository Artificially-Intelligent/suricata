# Functions for producing plots from data and parameters

weather_plot_f <- function(df){
  dygraph(df, main = "Weather predictions for 2020") %>%
    dySeries("rainfall", axis = 'y2') %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
}

addCircles_f <- function(map, df, group_by_column,  color_column, measure_column, location_columns_prefix = 'dest_', pal = pal){

  if(length(levels(factor(pal(df[,color_column])))) != length(levels(factor(df[,color_column]))) )
    browser()
  
  #add column for popup_html window
  df <- add_popup_html(df, location_columns_prefix)
  
  map %>% addCircleMarkers(data = df, 
                           lng = df[,paste(sep='', location_columns_prefix, 'long')],
                           lat = df[,paste(sep='', location_columns_prefix, 'lat')],
                           layerId = df[,group_by_column],
                           group = "dest_markers",
                           stroke = FALSE,
                           fillOpacity = 0.8,
                           radius =  round(4 + ( round(df[,measure_column]/100 , 2)) * 18),
                           fillColor = ~pal(df[,color_column]),
                           color = ~pal(df[,color_column]),
                           #icon = ~map_icons[df$pt_domr],
                           #clusterOptions = markerClusterOptions(iconCreateFunction=JS(clusterJS)) ,
                           popup = df[,"popup_html"],
                           popupOptions = popupOptions(closeButton = FALSE), 
                           options = pathOptions(pane = "top_circles")
  )  %>%  addLegend(position = 'topright'
            
              ,color = levels(factor(pal(df[,color_column])))
              ,labels = levels(factor(df[,color_column]))
              ,  group = "circles"
              )  %>%
    addLayersControl(position = 'topleft',overlayGroups = c("dest_markers"))
}

#return df with new columns added containing htlm formatted text
add_popup_html <- function(df, location_columns_prefix) {
  df_mod <- df %>%
    mutate(
      popup_html_location = paste(
        '<div class="alert_details_name"><h4>',
        df[[paste(sep = "", location_columns_prefix, "city")]],
        " (",
        df[[paste(sep = "", location_columns_prefix, "country_code")]],
        ')<h4></div>'
      ),
      
      popup_html_ip = paste('<div class="alert_details">IP Address(s)',
                            df[[paste(sep = "", location_columns_prefix, "ip")]],
                            '</div>'),
      
      popup_html_other = ""
    )
 
  df_other <- as.data.frame(lapply(names(df[, substr(names(df), 1, nchar(location_columns_prefix)) != location_columns_prefix]),
                                   function(x) {
                                     paste(
                                       sep = "",
                                       '<div class="',
                                       janitor::make_clean_names(x),
                                       '">',
                                       janitor::make_clean_names(x, case = "title"),
                                       ': ',
                                       case_when(
                                         typeof(df[[x]]) %in% c("double", "numeric") ~ format(round(as.numeric(df[[x]]), 2)),
                                         TRUE ~ as.character(df[[x]])
                                       ),
                                       '</div>'
                                     )
                                   }))
  # create a new column `popup_html_other` with the all columns collapsed together
  df_mod$popup_html_other <-
    apply(df_other[, names(df_other)] , 1 , paste , collapse = "")
  df_mod <- df_mod %>%
    mutate(popup_html = paste(popup_html_location,
                              popup_html_ip,
                              popup_html_other))
  return(df_mod)
  
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

in_bounding_box <- function(data, bounds, prefix = '') {
  
  data %>%
    dplyr::filter(
      eval(parse(text = paste(sep = '', prefix , 'lat'))) > bounds$south &
      eval(parse(text = paste(sep = '', prefix , 'lat'))) < bounds$north &
      eval(parse(text = paste(sep = '', prefix , 'long'))) < bounds$east & 
      eval(parse(text = paste(sep = '', prefix , 'long'))) > bounds$west
    )
}

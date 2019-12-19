source('server/s_data.R', local = TRUE)


source_df <- (get_redis_list(redis_host = redis_host, redis_key = redis_key))$new_lines %>% 
  format_redis_to_df(data_row_template = data_row_template)  

event_type <- 'http'
color_column <- 'http.status'
group_by_src <- 'false'

df <- mapData(alrtData = source_df,event_type = event_type, color_column = color_column,group_by_src = group_by_src)

df$polyline

if(is.numeric(df[,color_column])){
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = df[,color_column]
  )
}else{
  pal <- colorFactor(
    palette = "YlGnBu",
    domain = df[,color_column]
  )
}

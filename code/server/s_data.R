
# An empty data_row_template of the data frame we want to create
# data_row_template <- data.frame(date = character(), time = character(),
#                         size = numeric(), r_version = character(), r_arch = character(),
#                         r_os = character(), package = character(), version = character(),
#                         country = character(), ip_id = character(), received = numeric())

data_load_status <- reactiveValues(
  redis_index_last_loaded = 0,
  redis_timestamp_last_loaded = 0
)

redis_conn <- safe_redis(host = redis_host)

alertStream <- function(session) {
  redis_conn <- safe_redis(host = redis_host) 
  # redis_conn$PING()
  # Returns new lines
  newLines <- reactive({
    
    # Check if redis server can be connected
    if (is.null(safe_redis(host = redis_host))){
      retry_seconds <- 5
      print(paste("Redist db on host'",redis_host,"'is unavailable, retrying connection in", retry_seconds, "seconds" ))
      invalidateLater(1000 * retry_seconds, session)
      return(data_row_template)
    }
    
    redis_index_last_loaded <- isolate(data_load_status$redis_index_last_loaded)
    redis_timestamp_last_loaded <- isolate(data_load_status$redis_timestamp_last_loaded)
    
    redis_results <- get_redis_list(host = redis_host
                                    ,key = redis_key
                                    ,index_last_loaded = redis_index_last_loaded
                                    ,timestamp_last_loaded = redis_timestamp_last_loaded)
    
    new_lines <- redis_results$new_lines
    if(!is.null(redis_results$index_last_loaded)){
      data_load_status$redis_index_last_loaded <- redis_results$index_last_loaded
      data_load_status$redis_timestamp_last_loaded <-  redis_results$timestamp_last_loaded
    }
    if(is.null(input$data_refresh_rate) || ! is.numeric(input$data_refresh_rate) ||  redis_results$index_last_loaded  < redis_results$index_end ){
      invalidateLater(1000 * 5, session)
    }else{
      invalidateLater(1000 * isolate(input$data_refresh_rate), session)
    }
    
    if(is.null(new_lines))
      return(data_row_template)
    return(new_lines)
  })
  
  # Parses newLines() into data frame
  reactive({
    new_lines <- newLines()
    
    df <- format_redis_to_df(new_lines = new_lines, 
                      data_row_template = data_row_template)
    
    data_load_status$redis_timestamp_last_loaded <- (df[nrow(df),]$timestamp)
    
    return(df)
  })
}


# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
eventData <- function(alrtStream, timeWindow, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df,e_type = event_type) {
    
    if(!is.null(df)){
      # if(nrow(df)  < default_load_size){
      #   hide_waiter()
      # }
      # df <- new_lines
      if(e_type == '' || e_type == 'all')
        e_type <- event_types
      
      data_out <- df %>%
        filter(event_type %in%  e_type ) %>%
        select( names(data_row_template) ) %>%
        rbind(memo) %>%
        filter(timestamp > (Sys.time()) - timeWindow)
    }else{
      data_out <- memo
    }
    hidden_columns <- c('dest_ip','src_ip','http.hostname','http.http_port','http.url','http.http_user_agent','dhcp.client_mac','dhcp.assigned_ip','dns.rrname','dns.authorities','dns.answers','dns.grouped.AAAA','dns.grouped.PTR','dns.grouped.CNAME','dns.grouped.A','src_port','dest_port','payload','payload_printable','tls.subject','tls.sni','tls.fingerprint','tls.serial','tls.issuerdn')
    hidden_columns <- intersect(hidden_columns,names(data_row_template))
    
    user_roles <- isolate(u$user_roles)
    if(! is.null(user_roles)){
      if( user_roles$name %in% c('Global Admin','Suricata Admin','Suricata User')){
        hidden_columns <- c()
      }
    }
    if(interactive() && !enable_oauth_dev)
      hidden_columns <- c()
    
    if(length(hidden_columns) > 0 && nrow(data_out) > 0)
      data_out[,hidden_columns] <- 'hidden'
    
    
    if(nrow(data_out) > 0){
      hide_waiter()  
    }else{
      if(last_timestamp() < (Sys.time() - timeWindow)){
        hide_waiter()
        show_waiter(
          tagList(
            spin_wandering_cubes(),
            div(class='loading-text',paste(sep = '', "No traffic in source database in reporting window(", (Sys.time() - timeWindow), "-now). Waiting for system to aquire new traffic data..."))
          )
        )
        return(data_row_template)
      }
    }
    return(data_out)
    
    
  }, data_row_template)
}

# Count the total nrows of event_data
requestCount <- function(event_data = all_data, event_type = "") {
    if(event_type == '' || event_type == 'all')
      event_type <- event_types

    df <- event_data() %>% 
              filter(event_type %in% event_type) %>% 
              tally()
    df$n
}

latestRequestCount <- function(alrtStream, event_type = "") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    
    if(e_type == '' || e_type == 'all')
      e_type <- event_types
    
    df <- df %>% 
      filter(event_type %in% e_type) %>% 
      tally()
    
    memo + df$n
    
    # memo + nrow(filter(df,event_type %in% e_type))
  }, 0)
}


# 
# httpSuccessCount <- function(alrtStream){
#   shinySignals::reducePast(alrtStream, function(memo, df) {
#     if (is.null(df))
#       return(memo)
#     
#     http_status_success <- df %>% 
#       filter((round(as.numeric(http.status)/100,1) == 2),
#              event_type == 'http') %>%
#       nrow()
#     memo + http_status_success
#   }, 0)
# }

# Count the total nrows of distinct alrtStream$dest_ip
# destinationCount <- function(alrtStream, event_type = "") {
#   shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
#     if (is.null(df))
#       return(memo)
#     
#     if(e_type == '' || e_type == 'all')
#       e_type <- event_types
#     
#     memo + (df %>% 
#               filter(event_type %in% e_type) %>% 
#               group_by(dest_ip) %>% 
#               summarise() %>% 
#               nrow()
#     )
#   }, 0)
# }

valueCount <- function(event_data = all_data, event_type = event_type, value_column = value_column, bounds = NULL){
  
  if(event_type == '' || event_type == 'all')
    event_type <- event_types
  
  # If no data in scope return table with no rows
  if (nrow(event_data()) == 0){
    df <- data.frame(lapply(1:(1+length(value_column)),function(x){character()}))
    colnames(df) <- c(value_column,"n")
    return(df)    
  }
  
  df <- event_data()

  
  # filter value to bounds provided by map
  if (! is.null(bounds)){
    df <- in_bounding_box(df, bounds, prefix = 'dest_')
  }
  
  order <- unique(df[value_column[1],])
  
  df <- df %>%
    filter(event_type %in% event_type) %>% 
    group_by_at(value_column) %>%
    tally() %>%
    arrange_at(value_column) %>%
    arrange(desc(n))
  df
}

valueAgg <- function(event_data = all_data, event_type = event_type, value_column = 'event_type', measure_column = '' , agg_function = 'sum', bounds = NULL){
  
  if(event_type == '' || event_type == 'all')
    event_type <- event_types
  
  df <- event_data()
  
  # If no data in scope return table with no rows
  if (nrow(event_data()) == 0){
    browser()
    df <- data.frame(lapply(1:(length(measure_column) +length(value_column)),function(x){character()}))
    colnames(df) <- c(value_column,measure_column)
    return(df)    
  }
   
  # filter value to bounds provided by map
  if (! is.null(bounds)){
    df <- in_bounding_box(df, bounds, prefix = 'dest_')
  }
  
  # length(measure_column)
  # for(measure in measure_column){
  #   measure
  # }
 
  # df$value_column <- df[,value_column[1]]
  
  
  df$measure_column <- as.numeric(df[,measure_column[1]])
  if(length(measure_column) >= 2){
    df$measure_column2 <- as.numeric(df[,measure_column[2]])
  }else{
    df$measure_column2 <- NA
  }
  if(length(measure_column) >= 3){
    df$measure_column3 <- as.numeric(df[,measure_column[3]])
  }else{
    df$measure_column3 <- NA
  }
  if(length(measure_column) >= 4){
    df$measure_column4 <- as.numeric(df[,measure_column[4]])
  }else{
    df$measure_column4 <- NA
  }
  df <- df %>%
    filter(event_type %in% event_type) %>% 
    group_by_at(value_column) %>%
    summarise(measure_column = sum(measure_column)
              ,measure_column2 = sum(measure_column2)
              ,measure_column3 = sum(measure_column3)
              ,measure_column4 = sum(measure_column4)
              ) %>% 
    filter_at(value_column, all_vars(!is.na(.)) ) %>%
    replace(is.na(.), 0) %>%
    arrange_at(value_column) %>%
    arrange(desc(measure_column))
    
  df <- df[,1:(length(value_column) + length(measure_column))]
  colnames(df) <- c(value_column, measure_column)

  df
}



dynamic_valueAgg <- function(event_data = all_data, event_type = "all", tab_name_suffix = '', leafletId_suffix = '', value_column = 'event_type', group_by_columns = c(), measure_column = '' , agg_function = 'sum', isolate = FALSE){
  tab_name <- paste(event_type, tab_name_suffix, sep = '')
  leafletId <- paste( tab_name, leafletId_suffix,sep="")
  current_map_bounds_var <- paste('input$', leafletId, '_bounds', sep='')
  bounds <- NULL
  
  if(!isolate){
    bounds <- eval(parse(text = current_map_bounds_var))
  
    dynamic_measure_column  <- eval(parse(text = paste('input$',tab_name ,'.measure_picker', sep = '')))
    if(! is.null(dynamic_measure_column))
      measure_column <- dynamic_measure_column
    
    dynamic_value_column    <- eval(parse(text = paste('input$',tab_name ,'.value_picker', sep = '')))
    if(! is.null(dynamic_value_column))
      value_column <- dynamic_value_column
  }
  combined_value_columns <- c(group_by_columns,value_column )

  if(measure_column == 'count'){
    column_heading_measure = measure_column
  }else{
    column_heading_measure = paste(agg_function, measure_column)
  }
  column_heading_value = simple_cap(str_replace(value_column,'_', ' '))
  column_heading_measure_pct = paste("% by ",column_heading_measure) 
  
  #If no matching source data exists row with 0 values
  # if(nrow(event_data()) == 0){
  #   df <- data.frame(event_type,0,0)
  #   colnames(df) <- c(column_heading_value,column_heading_measure,column_heading_measure_pct)
  #   return(df)
  # }
  
  if(measure_column == 'count'){

    df <- valueCount(event_data = event_data
                     # leaflet_mapdata(event_data = event_data, event_type = event_type, tab_name_suffix = tab_name_suffix)                 
                     , event_type = event_type
                     , value_column = combined_value_columns
                     , bounds = bounds) %>%
      mutate(count = n)
    measure_total <- requestCount(event_data, event_type)
  }else{
    df <- valueAgg(event_data = event_data
                   , event_type = event_type
                   , value_column = combined_value_columns
                   , measure_column = measure_column
                   , agg_function = agg_function
                   , bounds = bounds) 
    
    measure_total <- sum(df[,length(combined_value_columns)+1:length(df)])
  
  }
  
  df <- df %>%
    # mutate_at(.vars = vars('percentage'), .funs = pct_calc) %>%
    select(group_by_columns, column_heading_value = value_column, column_heading_measure = measure_column) %>%
    mutate(column_heading_measure_pct = column_heading_measure / measure_total * 100) %>%
    as.data.frame()
  
  colnames(df)[(length(colnames(df)) - 2) : length(colnames(df))] <- c(column_heading_value,column_heading_measure,column_heading_measure_pct)
  
  return(df)
}




timeSpreadValueAgg <- function(event_data = all_data, event_type = event_type, value_column = value_column, measure_column = measure_column, agg_function = agg_function, bounds = NULL){

  new_value_column <- value_column
    
  if(event_type == 'flow'){
    new_value_column <- c(value_column,'flow.start','flow.end')
  }
  
  if(event_type == 'netflow'){
    new_value_column <- c(value_column,'netflow.start','netflow.end')
  }
  
  df <- valueAgg(event_data, event_type, new_value_column, measure_column, agg_function, bounds = bounds)

  
    
  min_timestamp <- min(df$timestamp)
  
  if(measure_column == 'flow.bytes_toserver'){
    df_single <- df[((df$flow.end - df$flow.start) < 1),]
    df_multi  <- df[((df$flow.end - df$flow.start) >= 1),]
    
    require(data.table)
    df_multi_spread  <- setDT(df_multi)[ , list(
      timestamp = seq(flow.start, flow.end, by = "sec"),
      event_type = event_type,
      flow.bytes_toserver = flow.bytes_toserver / ceiling(as.numeric(difftime(flow.end, flow.start, units = "secs")))
      , flow.start = flow.start
      , flow.end = flow.end
      ,    seconds = ceiling(as.numeric(difftime(flow.end, flow.start, units = "secs")))
      ), by = 1:nrow(df_multi)] %>%
      as.data.frame()
    
    df <- rbind(df_single[,c(value_column,measure_column)], df_multi_spread[,c(value_column,measure_column)] )
  }
  
  if(measure_column == 'flow.bytes_toclient'){
    df_single <- df[((df$flow.end - df$flow.start) < 1),]
    df_multi  <- df[((df$flow.end - df$flow.start) >= 1),]
    
    
    require(data.table)
    df_multi_spread  <- setDT(df_multi)[ , list(
      timestamp = seq(flow.start, flow.end, by = "sec"),
      event_type = event_type,
      flow.bytes_toclient = flow.bytes_toclient / ceiling(as.numeric(difftime(flow.end, flow.start, units = "secs")))
      , flow.start = flow.start
      , flow.end = flow.end
      ,    seconds = ceiling(as.numeric(difftime(flow.end, flow.start, units = "secs")))
    ), by = 1:nrow(df_multi)] %>%
      as.data.frame()
    
    df <- rbind(df_single[,c(value_column,measure_column)], df_multi_spread[,c(value_column,measure_column)] )
  }
  
  if(measure_column %in% c('netflow.bytes')){
    df_single <- df[((df$netflow.end - df$netflow.start) < 1),]
    df_multi  <- df[((df$netflow.end - df$netflow.start) >= 1),]
    
    require(data.table)
    df_multi_spread  <- setDT(df_multi)[ , list(
      timestamp = seq(netflow.start, netflow.end, by = "sec"),
      event_type = event_type,
      netflow.bytes = netflow.bytes / ceiling(as.numeric(difftime(netflow.end, netflow.start, units = "secs")))
      , netflow.start = netflow.start
      , netflow.end = netflow.end
      , seconds = ceiling(as.numeric(difftime(netflow.end, netflow.start, units = "secs")))
    ), by = 1:nrow(df_multi)] %>%
      as.data.frame()
    
    df <- rbind(df_single[,c(value_column,measure_column)], df_multi_spread[,c(value_column,measure_column)] ) 
  }
  
  df %>% filter(timestamp > min_timestamp)
}

# Count the total nrows of distinct alrtStream$dest_ip
first_timestamp <- firstTimestamp <- function(event_data = all_data, event_type = '') {
  if(event_type == '' || event_type == 'all')
    return((event_data() %>%
       summarise(timestamp = min(timestamp)))$timestamp
    )

  (event_data() %>%
    filter(event_type %in% event_type) %>% 
    summarise(timestamp = min(timestamp)))$timestamp
}

# Count the total nrows of distinct alrtStream$dest_ip
last_timestamp <- lastTimestamp <- function(event_data = all_data, event_type = '') {
  if(event_type == '' || event_type == 'all')
    return((event_data() %>%
       summarise(timestamp = max(timestamp)))$timestamp
    )
  
  (event_data() %>%
      filter(event_type %in% event_type) %>% 
      summarise(timestamp = max(timestamp)))$timestamp
}

# Count the total nrows of distinct alrtStream.dest_ip
totalBytes <- function(alrtStream, event_type = "") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    
    if (is.null(df))
      return(memo)
    
    if(e_type == '' || e_type == 'all')
      e_type <- event_types
    
    if(event_type == 'http')
      return(memo + (df %>% 
                       filter(event_type %in% e_type) %>% 
                       summarise(total_bytes = sum(http.length)) 
      )
      )
    if(event_type == 'fileinfo')
      return(memo + (df %>% 
                       filter(event_type %in% e_type) %>% 
                       summarise(total_bytes = sum(fileinfo.size )) 
      )
      )
    if(event_type == 'flow')
      return(memo + (df %>% 
                       filter(event_type %in% e_type) %>% 
                       summarise(total_bytes = sum(flow.bytes_toclient) +  sum(flow.bytes_toserver))
      )
      )
    return(memo)
  }, 0)
}


event_count_template <- data.frame(event_type =  event_types,
                                    event_count = 0
) %>% mutate(event_type = as.character(event_type))


eventCount <- function(alrtStream, timeWindow, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    e_count <- df %>% 
      group_by(event_type = event_type) %>% 
      summarise(event_count = n()) %>% 
      as.data.frame() 
    
    dplyr::bind_rows(e_count, memo ) %>% 
      group_by(event_type) %>% 
      summarise(event_count = sum(event_count)) %>%
      as.data.table()
    
  }, event_count_template
  )
}

mapData <- function(alrtData, event_type = '' , color_column  = 'event_type',group_by_src = FALSE) {

  if(event_type == ''){
    event_type <- event_types
  }
  e_type <- event_type
  
  # e_type = 'flow'  
  # df <- (get_redis_list(host = redis_host, key = redis_key))$new_lines %>% 
  #   format_redis_to_df(data_row_template = data_row_template)  %>%
  #   filter(event_type %in% e_type)
  

  # df <- formatted_lines[names(data_row_template)]
  df <- alrtData  %>%
    filter(event_type %in% e_type)
  
  if (is.null(df) || nrow(df) == 0)
      return()
  # invalidateLater(1000 * 60, session)
  # df <- formatted_lines[names(data_row_template)]
  
  print(paste('map rows:', nrow(df)))

  color_factor <- as.factor( df[,color_column] )
  
  if(is.null(df[color_column]) 
     # || nlevels(color_factor) < 2
     ){
    print('defaulting color_column to event_type')
    color_column <- 'app_proto'
  }
  
  color_factor <- as.factor( df[,color_column] )
  color_set <- data.frame(levels(color_factor),levels(color_factor),
                          color = I(brewer.pal(nlevels(color_factor), name = 'Dark2')[1:nlevels(color_factor)]))
  names(color_set) <- c(color_column,'color_column','color')
  
  total_requests <- nrow(df)
  
  
  total_bytes <- max(sum( df$http.length, na.rm=T ),
                     (sum(df$flow.bytes_toclient, na.rm=T) + sum(df$flow.bytes_toserver, na.rm=T)),
                     sum(df$netflow.bytes, na.rm=T))
  
  if(group_by_src){
    df <-  df %>%
      mutate(country_name = src_country_name, country_code = src_country_code, city = src_city, 
             ip = src_ip, long = src_long,lat = src_lat)  
  }else{
    df <-  df %>% 
      mutate(country_name = dest_country_name, country_code = dest_country_code, city = dest_city, 
           ip = dest_ip, long = dest_long,lat = dest_lat,
           polyline = encodeCoordinates(c(src_long, dest_long),c(src_lat, dest_lat)) 
           )
  }
  
  df_grouped <- merge.data.frame(df, color_set) %>% 
    filter(event_type %in% e_type) %>%
    group_by(country_name, country_code, city, 
            # ip, 
             long, lat, color,polyline, color_column) %>%
     summarise( 
    # popup_html = (paste(
    #   '<div class="alert_details_name"><h4>', unique(city), " (", unique(country_code) , ')<h4></div>',
    #   '<div class="alert_details">IP Address(s)', paste(Filter(Negate(is.na),unique(dest_ip)),collapse =',\n'), '</div>',
    #   '<div class="alert_details">Event Type(s):',paste(Filter(Negate(is.na),unique( event_type)),collapse =',\n'), '</div>',
    #   '<div class="alert_details">Traffic Type(s):',paste(Filter(Negate(is.na),unique( app_proto)),collapse =',\n'), '</div>',
    #   '<div class="alert_details">Requests:', n(), '</div>',
    #   (if('flow' %in% event_type) paste(
    #     '<div class="alert_details">Alert Triggered:', ('TRUE' %in% flow.alerted) , '</div>',
    #     '<div class="alert_details">Bytes to Client:', sum(flow.bytes_toclient), '</div>',
    #     '<div class="alert_details">Bytes to Server:', sum(flow.bytes_toserver), '</div>',
    #     '<div class="alert_details">Packets to Client:', sum(flow.pkts_toclient), '</div>',
    #     '<div class="alert_details">Packets to Server:', sum(flow.pkts_toclient), '</div>'
    #     )
    #   ),(if('netflow' %in% event_type) paste(
    #     '<div class="alert_details">Bytes:', sum(netflow.bytes), '</div>',
    #     '<div class="alert_details">Packets:', sum(netflow.pkts), '</div>'
    #     )
    #   ),
    #   (if('http' %in% event_type) paste(
    #     '<div class="alert_details">HTTP Hostname(s):',paste(Filter(Negate(is.na),unique( http.hostname)),collapse =',\n'), '</div>',
    #     '<div class="alert_details">HTTP Server(s):', paste(Filter(Negate(is.na),unique( http.server)),collapse =', '), '</div>',
    #     '<div class="alert_details">HTTP Method(s):', paste(Filter(Negate(is.na),unique( http.http_method)),collapse =', '), '</div>',
    #     '<div class="alert_details">HTTP Bytes:', sum(http.length, na.rm=T ), '</div>'
    #   )),
    #   (if('dns' %in% event_type) paste(
    #     '<div class="alert_details">DNS Resource Record(s):',paste(Filter(Negate(is.na),unique( dns.rrname)),collapse =',\n'), '</div>',
    #     '<div class="alert_details">DNS Message Type(s):', paste(Filter(Negate(is.na),unique( dns.type)),collapse =', '), '</div>'
    #   )),
    #   (if('alert' %in% event_type) paste(
    #     '<div class="alert_details">Alert Category(s):',paste(Filter(Negate(is.na),unique( alert.category)),collapse =',\n'), '</div>',
    #     '<div class="alert_details">Alert Action(s):', paste(Filter(Negate(is.na),unique( alert.action)),collapse =', '), '</div>',
    #     '<div class="alert_details">Alert Signature(s):', paste(Filter(Negate(is.na),unique( alert.signature)),collapse =', '), '</div>',
    #     '<div class="alert_details">Alert Severity(s):', paste(Filter(Negate(is.na),unique( alert.severity)),collapse =', '), '</div>'
    #   )),
    #   '</div>',
    # sep = ' ')
    # ),
    requests = n(), 
    bytes = max(sum(http.length, na.rm=T ),
                (sum(flow.bytes_toclient, na.rm=T ) + sum(flow.bytes_toserver, na.rm=T )),
                sum(netflow.bytes, na.rm=T )),
    packets = max(sum(flow.pkts_toclient, na.rm=T ) + sum(flow.pkts_toserver, na.rm=T ),
                  sum(netflow.pkts, na.rm=T )),
  ) %>%
    mutate(total_bytes_pct = round(bytes/total_bytes,2)
           ,radius = round(4 + ( round(requests/total_requests, 2)) * 16)
           # ,radius = round(4 + ( round(bytes/total_bytes, 2)) * 16)
    )
  return(
    df_grouped
  )
}


# mapData_sql <- function(alrtData, event_type = '' , color_column  = 'event_type',group_by_src = FALSE) {
#   
#   if(event_type == ''){
#     event_type <- event_types
#   }
#   e_type <- event_type
#   
#   db_pool <- safe_pool()
#   events_db <- tbl(db_pool, "lnd_events")
#   totals <- events_db %>%
#     filter(event_type %in% e_type) %>%
#     summarise(requests = n() , 
#               http_bytes = sum( http.length, na.rm=T ),
#               netflow_bytes = sum(netflow.bytes, na.rm=T ),
#               flow_toclient_bytes = sum(flow.bytes_toclient, na.rm=T ),
#               flow_toserver_bytes = sum(flow.bytes_toserver, na.rm=T )
#     )
#   
#   if(group_by_src){
#     events_mutated <-  events_db %>%
#       filter(timestamp > 0) %>%
#       filter(event_type %in% e_type) %>%
#       # group_by(src_city, src_country_code, src_country_name  
#       #          ,src_lat,src_long 
#       #          # ,polyline
#       #          ,color_column
#       # ) %>%
#       mutate(country_name = src_country_name, country_code = src_country_code, city = src_city
#              , long = src_long,lat = src_lat
#              ,ip = src_ip
#               # ,polyline = encodeCoordinates(c(src_long, dest_long),c(src_lat, dest_lat))
#             )
#   }else{
#     events_mutated <-  events_db %>% 
#       filter(timestamp > 0) %>%
#       filter(event_type %in% e_type) %>%
#       # group_by(dest_city, dest_country_code, dest_country_name  
#       #          ,dest_lat,dest_long 
#       #          # ,polyline
#       #          ,color_column
#       # ) %>%
#       mutate(country_name = dest_country_name, country_code = dest_country_code, city = dest_city 
#              ,long = dest_long,lat = dest_lat
#              ,ip = dest_ip
#              # ,polyline = encodeCoordinates(c(src_long, dest_long),c(src_lat, dest_lat))
#              # ,color_column = eval(parse(text = color_column)) 
#       )
#   }
#   
#   df_grouped <- events_mutated %>%
#     group_by(city, country_code, country_name
#              ,lat,long
#              ,color_column
#      ) %>%
#     summarise(
#       ip = str_flatten(distinct(ip),collapse =',\n')
#       , event_type = str_flatten(distinct(event_type),collapse =',\n')
#       , app_proto = str_flatten(distinct(app_proto),collapse =',\n')
#       , requests = n()
#       , flow.alerted = str_flatten(distinct(flow.alerted),collapse =',\n')
#       , flow.bytes_toclient = sum(flow.bytes_toclient, na.rm=T )
#       , flow.bytes_toserver = sum(flow.bytes_toserver, na.rm=T )
#       , flow.pkts_toclient = sum(flow.pkts_toclient, na.rm=T )
#       , flow.pkts_toserver = sum(flow.pkts_toserver, na.rm=T )
#       , netflow.bytes = sum(netflow.bytes)
#       , netflow.pkts = sum(netflow.pkts)
#       , http.hostname = str_flatten(distinct(http.hostname),collapse =',\n')
#       , http.server = str_flatten(distinct(http.server),collapse =', ')
#       , http.http_method = str_flatten(distinct( http.http_method),collapse =', ')
#       , http.length = sum(http.length)
#       , dns.rrname = str_flatten(distinct(dns.rrname),collapse =',\n')
#       , dns.type = str_flatten(distinct(dns.type),collapse =', ')
#       , alert.category = str_flatten(distinct(alert.category),collapse =',\n')
#       , alert.action = str_flatten(distinct(alert.action),collapse =', ') 
#     #  , alert.signature = str_flatten(distinct(alert.signature),collapse =', ')
#       , alert.severity = str_flatten(distinct(alert.severity),collapse =', ')
#     ) 
#     
#   df_loaded <- df_grouped %>%
#     as.data.frame()
#   
#   df_formatted <- df_grouped %>%
#     mutate( popup_html = paste(  
#       '<div class="alert_details_name"><h4>', city, " (", country_code , ')<h4></div>'
#     
#       ,'<div class="alert_details">IP Address(s)', ip, '</div>'
#       ,'<div class="alert_details">Event Type(s):', event_type, '</div>'
#       ,'<div class="alert_details">Traffic Type(s):',  app_proto, '</div>'
#       ,'<div class="alert_details">Requests:', n(), '</div>'
#       , sep = ' ')
#     )
#   if('flow' %in% e_type)
#     df_formatted <- df_formatted %>%
#       mutate(popup_html = paste( popup_html,
#         '<div class="alert_details">Alert Triggered:', flow.alerted , '</div>'
#         ,'<div class="alert_details">Bytes to Client:', flow.bytes_toclient, '</div>'
#         ,'<div class="alert_details">Bytes to Server:', flow.bytes_toserver, '</div>'
#         ,'<div class="alert_details">Packets to Client:', flow.pkts_toclient, '</div>'
#         ,'<div class="alert_details">Packets to Server:', flow.pkts_toclient, '</div>'
#         , sep = ' ')
#       )
#   if('netflow' %in% e_type)
#     df_formatted <- df_formatted %>%
#     mutate(popup_html = paste( popup_html,
#         '<div class="alert_details">Bytes:', netflow.bytes, '</div>'
#         ,'<div class="alert_details">Packets:', netflow.pkts, '</div>'
#         , sep = ' '))
#   
#   if('http' %in% e_type)
#     df_formatted <- df_formatted %>%
#     mutate(popup_html = paste( popup_html,
#         '<div class="alert_details">HTTP Hostname(s):', http.hostname, '</div>'
#         ,'<div class="alert_details">HTTP Server(s):',  http.server, '</div>'
#         ,'<div class="alert_details">HTTP Method(s):',   http.http_method, '</div>'
#         ,'<div class="alert_details">HTTP Bytes:', http.length, '</div>'
#         , sep = ' '))
#   
#   if('dns' %in% e_type)
#     df_formatted <- df_formatted %>%
#     mutate(popup_html = paste( popup_html,
#         '<div class="alert_details">DNS Resource Record(s):', dns.rrname, '</div>'
#         ,'<div class="alert_details">DNS Message Type(s):',  dns.type, '</div>'
#         , sep = ' '))
#   
#   if('alert' %in% e_type)
#     df_formatted <- df_formatted %>%
#     mutate(popup_html = paste( popup_html
#         ,'<div class="alert_details">Alert Category(s):', alert.category, '</div>'
#         ,'<div class="alert_details">Alert Action(s):',  alert.action, '</div>'
#         ,'<div class="alert_details">Alert Signature(s):',  alert.signature, '</div>'
#         # ,'<div class="alert_details">Alert Severity(s):',  alert.severity, '</div>'
#         , sep = ' ')
#     )
#   df_formatted <- df_formatted %>%
#     mutate(popup_html = paste( popup_html , '</div>'))
#   
#   df_mapdata <- df_formatted  %>%
#     mutate(bytes =  if((flow.bytes_toclient + flow.bytes_toserver) > 0){(flow.bytes_toclient + flow.bytes_toserver)} else{
#       if((netflow.bytes) > 0){(netflow.bytes)} else{
#         if((http.length) > 0){(http.length)} else{
#           0 
#         }  
#       }
#     }
#     )
#   #     http.length ,
#   #               (flow.bytes_toclient + flow.bytes_toserver),
#   #               sum(netflow.bytes, na.rm=T )),
#   # packets = max(sum(flow.pkts_toclient, na.rm=T ) + sum(flow.pkts_toserver, na.rm=T ),
#   #               sum(netflow.pkts, na.rm=T )),
#   # ) %>%
#   # mutate(total_bytes_pct = round(bytes/total_bytes,2)
#   #        ,radius = round(4 + ( round(requests/total_requests, 2)) * 16)
#   #        
# 
#   # df_grouped %>%
#   #   mutate(total_bytes_pct = round(bytes/total_bytes,2)
#   #          ,radius = round(4 + ( round(requests/total_requests, 2)) * 16)
#   #          # ,radius = round(4 + ( round(bytes/total_bytes, 2)) * 16)
#   #   )
#   
#   # color_factor <- as.factor( df[,color_column] )
#   # 
#   # if(is.null(df[color_column]) 
#   #    # || nlevels(color_factor) < 2
#   # ){
#   #   print('defaulting color_column to event_type')
#   #   color_column <- 'event_type'
#   # }
#   # 
#   # color_factor <- as.factor( df[,color_column] )
#   # color_set <- data.frame(levels(color_factor),levels(color_factor),
#   #                         color = I(brewer.pal(nlevels(color_factor), name = 'Dark2')[1:nlevels(color_factor)]))
#   # names(color_set) <- c(color_column,'color_column','color')
# 
#   show_query(df_grouped)
#   
#   qry_start <- Sys.time()
#   df_grouped
#   Sys.time() - qry_start
#   return(
#     show_query(df_grouped)
#   )
# }

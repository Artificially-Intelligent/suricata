# An empty data_row_template of the data frame we want to create
# data_row_template <- data.frame(date = character(), time = character(),
#                         size = numeric(), r_version = character(), r_arch = character(),
#                         r_os = character(), package = character(), version = character(),
#                         country = character(), ip_id = character(), received = numeric())

data_load_status <- reactiveValues(
  redis_index_last_loaded = 0,
  redis_timestamp_last_loaded = 0
)

alertStream <- function(session) {
  redis_conn <- safe_redis(host = redis_host, 'redis_conn') 
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
    
    data_load_status$redis_timestamp_last_loaded <- (df[nrow(df),]$timestamp_num)
    
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
      if(e_type == '')
        e_type <- event_types
      
      data_out <- df %>%
        filter(event_type %in%  e_type ) %>%
        select( names(data_row_template) ) %>%
        rbind(memo) %>%
        filter(timestamp_num > as.numeric(Sys.time()) - timeWindow)
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
    
    if(nrow(data_out)>0)
      hide_waiter()
    
    return(data_out)
    
    
  }, data_row_template)
}

# Count the total nrows of alrtStream
requestCount <- function(alrtStream, event_type = "") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    
    if(e_type == '')
      e_type <- event_types
    
    memo + nrow(filter(df,event_type %in% e_type))
  }, 0)
}

httpSuccessCount <- function(alrtStream){
  shinySignals::reducePast(alrtStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    
    http_status_success <- df %>% 
      filter((round(as.numeric(http.status)/100,1) == 2),
             event_type == 'http') %>%
      nrow()
    memo + http_status_success
  }, 0)
}

# Count the total nrows of distinct alrtStream$dest_ip
destinationCount <- function(alrtStream, event_type = "") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    
    if(e_type == '')
      e_type <- event_types
    
    memo + (df %>% 
              filter(event_type %in% e_type) %>% 
              group_by(dest_ip) %>% 
              summarise() %>% 
              nrow()
    )
  }, 0)
}


# Count the total nrows of distinct alrtStream$dest_ip
firstTimestamp <- function(alrtStream, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    
    if(e_type == '')
      e_type <- event_types
    
    if(nrow(df) == 0)
      return(memo)
    (df %>%
        filter(event_type %in% e_type) %>% 
        summarise(timestamp = min(timestamp_num))
    )[[1]]
  }, 0)
}

# Count the total nrows of distinct alrtStream$dest_ip
lastTimestamp <- function(alrtStream, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    
    if(e_type == '')
      e_type <- event_types
    
    if(nrow(df) == 0)
      return(memo)
    (df %>%
        filter(event_type %in% e_type) %>% 
        summarise(timestamp = max(timestamp_num))
    )[[1]]
  }, 0)
}

# Count the total nrows of distinct alrtStream.dest_ip
totalBytes <- function(alrtStream, event_type = "") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    
    if (is.null(df))
      return(memo)
    
    if(e_type == '')
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
    
    bind_rows(e_count, memo ) %>% 
      group_by(event_type) %>% 
      summarise(event_count = sum(event_count)) %>%
      as.data.table()
    
  }, event_count_template
  )
}

mapData <- function(alrtData, event_type = '' , color_column  = 'app_proto', group_by_src = FALSE) {
  if(event_type == ''){
    event_type <- event_types <- c('flow','netflow')
  }
  
  q_end <- now(tzone = "UTC")
  q_start <- now(tzone = "UTC") - 86400
  
  grouping_fields <- c(paste(sep ='','suricata.eve.', color_column ,'.keyword'),'source.geo.latitude','source.geo.longitude','destination.geo.latitude','destination.geo.longitude') 
  
  measure_fields <- NULL
  
  if('netflow' %in% event_type)
    measure_fields <- c(measure_fields,'suricata.eve.netflow.bytes','suricata.eve.netflow.pkts')
  
  if('flow' %in% event_type)
    measure_fields <- c(measure_fields,'suricata.eve.flow.bytes_toclient','suricata.eve.flow.bytes_toserver'
                        ,'suricata.eve.flow.pkts_toclient','suricata.eve.flow.pkts_toserver')
  
  # if('http' %in% event_type)
  #   measure_fields <- c(measure_fields,'suricata.eve.http.content_length')
  
  df <- get_elastic_data(measure_fields = measure_fields
     ,slicer_fields = grouping_fields
     ,date_interval = '1y'
     ,filter_fields = 'suricata.eve.event_type.keyword'
     ,filter_values = event_type
     ,date_start = q_start
     ,date_end = q_end
  )
  
  
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
    color_column <- 'event_type'
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
    summarise( popup_html = (paste(
      '<div class="alert_details_name"><h4>', unique(city), " (", unique(country_code) , ')<h4></div>',
      '<div class="alert_details">IP Address(s)', paste(Filter(Negate(is.na),unique(dest_ip)),collapse =',\n'), '</div>',
      '<div class="alert_details">Event Type(s):',paste(Filter(Negate(is.na),unique( event_type)),collapse =',\n'), '</div>',
      '<div class="alert_details">Traffic Type(s):',paste(Filter(Negate(is.na),unique( app_proto)),collapse =',\n'), '</div>',
      '<div class="alert_details">Requests:', n(), '</div>',
      (if('flow' %in% event_type) paste(
        '<div class="alert_details">Alert Triggered:', ('TRUE' %in% flow.alerted) , '</div>',
        '<div class="alert_details">Bytes to Client:', sum(flow.bytes_toclient), '</div>',
        '<div class="alert_details">Bytes to Server:', sum(flow.bytes_toserver), '</div>',
        '<div class="alert_details">Packets to Client:', sum(flow.pkts_toclient), '</div>',
        '<div class="alert_details">Packets to Server:', sum(flow.pkts_toclient), '</div>'
        )
      ),(if('netflow' %in% event_type) paste(
        '<div class="alert_details">Bytes:', sum(netflow.bytes), '</div>',
        '<div class="alert_details">Packets:', sum(netflow.pkts), '</div>'
        )
      ),
      (if('http' %in% event_type) paste(
        '<div class="alert_details">HTTP Hostname(s):',paste(Filter(Negate(is.na),unique( http.hostname)),collapse =',\n'), '</div>',
        '<div class="alert_details">HTTP Server(s):', paste(Filter(Negate(is.na),unique( http.server)),collapse =', '), '</div>',
        '<div class="alert_details">HTTP Method(s):', paste(Filter(Negate(is.na),unique( http.http_method)),collapse =', '), '</div>',
        '<div class="alert_details">HTTP Bytes:', sum(http.length, na.rm=T ), '</div>'
      )),
      (if('dns' %in% event_type) paste(
        '<div class="alert_details">DNS Resource Record(s):',paste(Filter(Negate(is.na),unique( dns.rrname)),collapse =',\n'), '</div>',
        '<div class="alert_details">DNS Message Type(s):', paste(Filter(Negate(is.na),unique( dns.type)),collapse =', '), '</div>'
      )),
      (if('alert' %in% event_type) paste(
        '<div class="alert_details">Alert Category(s):',paste(Filter(Negate(is.na),unique( alert.category)),collapse =',\n'), '</div>',
        '<div class="alert_details">Alert Action(s):', paste(Filter(Negate(is.na),unique( alert.action)),collapse =', '), '</div>',
        '<div class="alert_details">Alert Signature(s):', paste(Filter(Negate(is.na),unique( alert.signature)),collapse =', '), '</div>',
        '<div class="alert_details">Alert Severity(s):', paste(Filter(Negate(is.na),unique( alert.severity)),collapse =', '), '</div>'
      )),
      '</div>',
    sep = ' ')
    ),
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
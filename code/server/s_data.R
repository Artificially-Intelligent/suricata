
# An empty data_row_template of the data frame we want to create
# data_row_template <- data.frame(date = character(), time = character(),
#                         size = numeric(), r_version = character(), r_arch = character(),
#                         r_os = character(), package = character(), version = character(),
#                         country = character(), ip_id = character(), received = numeric())

data_load_status <- reactiveValues(
  redis_index_last_loaded = 0,
  redis_timestamp_last_loaded = 0
)
layered_data_row_template <- data.frame(
  timestamp = as_datetime(x = integer(0)),
  timestamp_num = numeric(), 
    flow_id = character(),
  in_iface = character(),
  event_type = character(),
  src_ip = character(),
  src_port = numeric(),
  src_country_name = character(),
  src_country_code = character(),
  src_city = character(),
  src_lat = numeric(),
  src_long = numeric(),
  dest_ip = character(),
  dest_port = numeric(),
  dest_country_name = character(),
  dest_country_code = character(),
  dest_city = character(),
  dest_lat = numeric(),
  dest_long = numeric(),
  proto = character(),
  host = character(),
  app_proto = character(),
  tx_id = character(), 
  payload = character(),
  payload_printable = character(),
  stream = character(),
  packet = character(),
  icmp_type = character(),
  icmp_code = character(),
alert = character(),
dhcp = character(),
drop = character(),
dns = character(),
fileinfo = character(),
flow = character(),
http = character(),
netflow = character(),
packet_info = character(), 
tcp = character(),
tls = character()
)
data_row_template <- data.frame(
  timestamp = as_datetime(x = integer(0)),
  timestamp_num = numeric(), 
  flow_id = character(),
  in_iface = character(),
  event_type = character(),
  src_ip = character(),
  src_port = numeric(),
  src_country_name = character(),
  src_country_code = character(),
  src_city = character(),
  src_lat = numeric(),
  src_long = numeric(),
  dest_ip = character(),
  dest_port = numeric(),
  dest_country_name = character(),
  dest_country_code = character(),
  dest_city = character(),
  dest_lat = numeric(),
  dest_long = numeric(),
  proto = character(),
  host = character(),
  app_proto = character(),
  tx_id = character(), 
  payload = character(),
  payload_printable = character(),
  stream = character(),
  packet = character(),
  icmp_type = character(),
  icmp_code = character(),
  app_proto_ts = character(), 
  timestamp_num = numeric(),
  
  dns.type = character(),
  dns.id = character(),
  dns.rrname = character(),
  dns.rrtype = character(),
  dns.tx_id = character(),
  dns.version = character(),
  dns.flags = character(),
  dns.qr = character(),
  dns.rcode = character(),
  dns.authorities = character(),
  dns.aa = character(),
  dns.answers = character(), 
  dns.rd = character(),
  dns.ra = character(),
  dns.tc = character(),
  dns.grouped.AAAA = character(),
  dns.grouped.PTR = character(),
  dns.grouped.CNAME = character(),
  dns.grouped.A = character(),
  tls.subject = character(), 
  tls.issuerdn = character(),
  tls.serial = character(),
  tls.fingerprint = character(),
  tls.sni = character(), 
  tls.version = character(),
  tls.notbefore = character(),
  tls.notafter = character(),
  tls.session_resumed = character(),
  tls.ja3 = character(),
  http.hostname = character(),
  http.http_port = character(),
  http.url = character(),
  http.http_user_agent = character(),
  http.http_content_type = character(),
  http.http_method = character(),
  http.protocol = character(),
  http.status = character(), 
  http.length = numeric(),
  http.connection = character(), 
  http.content_length = numeric(),
  http.content_type = character(), 
  http.date = character(), 
  http.accept_encoding = character(), 
  http.accept_language = character(), 
  http.content_encoding = character(), 
  http.server = character(), 
  http.transfer_encoding = character(), 
  http.vary = character(), 
  http.accept = character(),
  http.set_cookie = character(),
  http.age = character(),
  http.last_modified = character(),
  http.http_response_body_printable = character(),
  http.http_response_body = character(),
  http.te = character(),
  fileinfo.filename = character(),
  fileinfo.gaps = character(),
  fileinfo.state = character(),
  fileinfo.stored = character(),
  fileinfo.size = numeric(),
  fileinfo.tx_id = character(),
  alert.action = character(),
  alert.gid = character(),
  alert.signature_id = character(),
  alert.rev = character(),
  alert.signature = character(), 
  alert.category = character(),
  alert.severity = character(),
  alert.metadata.updated_at = character(),
  alert.metadata.created_at = character(),
  alert.metadata.former_category = character(),
  flow.pkts_toserver = numeric(),
  flow.pkts_toclient = numeric(),
  flow.bytes_toserver = numeric(),
  flow.bytes_toclient = numeric(),
  flow.start = character(),
  packet_info.linktype = character(),
  flow.end = character(), 
  flow.age = character(), 
  flow.state = character(), 
  flow.reason = character(), 
  flow.alerted = character(), 
  netflow.pkts = numeric(),
  netflow.bytes = numeric(),
  netflow.start = numeric(),
  netflow.end = character(), 
  netflow.age = numeric(),
  netflow.min_ttl = numeric(),
  netflow.max_ttl = numeric(),
  metadata.flowints.applayer.anomaly.count = numeric(),
  metadata.flowints.http.anomaly.count = numeric(),
  metadata.flowints.tcp.retransmission.count = numeric(),
  metadata.flowbits = numeric(),
  tcp.tcp_flags = character(), 
  tcp.tcp_flags_ts = character(), 
  tcp.tcp_flags_tc = character(), 
  tcp.syn = character(), 
  tcp.rst = character(), 
  tcp.psh = character(), 
  tcp.ack = character(), 
  tcp.state = character(), 
  tcp.fin = character(), 
  tcp.ecn = character(), 
  tcp.cwr = character(),
  icmp_type = character(),
  icmp_code = character(),
  dhcp.type = character(),
  dhcp.id = character(),
  dhcp.client_mac = character(),
  dhcp.assigned_ip = character(),
  dhcp.dhcp_type = character()
)


alertStream <- function(session) {
  # Connect to data source
  
  
  if (! redux::redis_available(host = 'unraiden.local'))
    return(data_row_template)
  
  r <- redux::hiredis(host = 'unraiden.local')
  r$PING()
  
  # redis_index_end <- r$LLEN(key='suricata')
  # redis_index_last_loaded <- 0
  # 
  # Returns new lines
  newLines <- reactive({
    
    input$data_refresh_rate  
    
    redis_index_end <- r$LLEN(key='suricata')
    redis_index_last_loaded <- isolate(data_load_status$redis_index_last_loaded)
    redis_timestamp_num_last_loaded <- isolate(data_load_status$redis_timestamp_last_loaded)
    
    print(paste("last load index_end:", redis_index_last_loaded, "current index_end:",redis_index_end ))
    
    redis_index_load_start <- redis_index_last_loaded
    
    if(redis_index_last_loaded == 0 || (redis_index_last_loaded + initial_history_load_size) < redis_index_end ){
      new_entries <- initial_history_load_size
      if(redis_index_last_loaded == 0)
        redis_index_load_start <- redis_index_end -  (initial_history_load_size * 1)
    }else{
      if(redis_index_last_loaded > redis_index_end){
        new_entries <- min(redis_index_end,default_load_size)
        redis_index_load_start <- redis_index_end - new_entries
      }else{
        if((redis_index_end - redis_index_last_loaded) > max_history_load_size)
          redis_index_load_start <- redis_index_end -  max_history_load_size
        new_entries <- redis_index_end - redis_index_last_loaded
      }
    }
    
    print(paste("load index_start:", redis_index_load_start, "new entries to load:",new_entries,  "current index_end:",redis_index_end ))
    
    data_load_status$full_load <- TRUE
    
    if(is.null(input$data_refresh_rate) || ! is.numeric(input$data_refresh_rate) ||  (redis_index_last_loaded + new_entries) < redis_index_end )
      data_load_status$full_load <- FALSE
    
    # future(
      {
        new_lines <- ''
          
        if(new_entries > 0){
          tryCatch(
            {
                {
                  # location_fields <- c('country_name','country_code','city','lat','long')
                  # redis_timestamp_num_last_loaded <- 0
                  new_json <- r$LRANGE(key='suricata',redis_index_load_start,redis_index_load_start + new_entries -1) 
                  
                  new_lines <- fromJSON(paste('[', paste(new_json,collapse = ','),']')) %>%
                    mutate(timestamp = as_datetime(timestamp)) %>%
                    mutate(timestamp_num = as.numeric(timestamp)) %>%
                    filter(timestamp_num > redis_timestamp_num_last_loaded)
                    # mutate('times' = as_datetime(timestamp ,format = '%Y-%m-%dT%R:%OS %z'))
                  
                    data_load_status$redis_index_last_loaded <- redis_index_load_start + new_entries
                  
                  #  new_lines <- new_json %>%
                  #           lapply(function(x){
                  #              parsed_json <- jsonlite::fromJSON(x)
                  #              parsed_json$tcp =  (if(is.null(parsed_json$tcp)){NA}else{parsed_json$tcp})
                  #              parsed_json$timestamp_time = as_datetime(parsed_json$timestamp ,format = '%Y-%m-%dT%R:%OS %z')
                  #              parsed_json$timestamp_num = as.numeric(parsed_json$timestamp_time)
                  #              if(parsed_json$timestamp_num > redis_timestamp_num_last_loaded)
                  #               return(unlist(parsed_json))
                  #              else
                  #                return(NA)
                  #             })
                  # # )
                  
                  # Filter out old entries that had been replaced with NA
                  # new_lines <- Filter(Negate(is.na),new_lines )
                }
              #query_time <- r$TIME
              
            },
            error = function(c) {
              new_entries <- 0
              msg <- conditionMessage(c)
              message(c)
              invisible(structure(msg, class = "try-error"))
            }
          )
        } 
        print(paste("new_lines:",nrow(new_lines)))
        
        if(! isolate(data_load_status$full_load)){
          invalidateLater(1000 * 1, session)
        }else{
          invalidateLater(1000 * isolate(input$data_refresh_rate), session)
        }
        
        new_lines
      }
     # )
  })
  
  # Parses newLines() into data frame
  reactive({
    new_lines <- newLines()
    
    if(is.null(new_lines) || nrow(new_lines) == 0)
      return(data_row_template)
    
    location_fields <- c('country_name','country_code','city','lat','long')
    location_dest <- rgeolocate::ip2location(ips = new_lines$dest_ip
                                             , file = iplookup_db_file
                                             , fields = location_fields
    )
    names(location_dest) <- paste("dest", location_fields,sep="_")

    location_src <- rgeolocate::ip2location(ips = new_lines$src_ip
                                             , file = iplookup_db_file
                                             , fields = location_fields
    )
    names(location_src) <- paste("src", location_fields,sep="_")
    
    
    
    formatted_lines <- cbind(new_lines, location_dest,location_src) %>%
      jsonlite::flatten()
    
    print(paste("Columns not in data_row_template:", 
                paste(collapse = ",",setdiff(names(formatted_lines),names(data_row_template)))))
    
    # Find names of missing columns and add them filled with NA's
    missing <- setdiff(names(data_row_template), names(formatted_lines))  
    if(length(missing)>0){formatted_lines[missing] <- NA }
      
    print(paste("last timestamp:",formatted_lines[nrow(formatted_lines),]$timestamp))
    data_load_status$redis_timestamp_last_loaded <- (formatted_lines[nrow(formatted_lines),]$timestamp_num)
    
    # Return result values for columns in data_row_template in a desired order 
    return(formatted_lines[names(data_row_template)])
  })
}


# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
alertData <- function(alrtStream, timeWindow, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df,e_type = event_type) {
    if(!is.null(df)){
      # if(nrow(df)  < default_load_size){
      #   hide_waiter()
      # }
      # df <- new_lines
      if(e_type == '')
        e_type <- event_types
      
      hide_waiter()
      
      df %>%
        filter(event_type %in%  e_type ) %>%
        select( names(data_row_template) ) %>%
        rbind(memo) %>%
        filter(timestamp_num > as.numeric(Sys.time()) - timeWindow)
    }else{
      memo
    }
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

mapData <- function(alrtData, event_type = '') {
  df <- alrtData  
  if (is.null(df))
      return()
  # invalidateLater(1000 * 60, session)
  # df <- formatted_lines[names(data_row_template)]
  
  if(event_type == ''){
    event_type <- event_types
  }
  e_type <- event_type
  
  total_bytes <- max(sum( df$http.length, na.rm=T ),
                     (sum(df$flow.bytes_toclient, na.rm=T) + sum(df$flow.bytes_toserver, na.rm=T)),
                     sum(df$netflow.bytes, na.rm=T))
  
  df_grouped <- df %>% 
    filter(event_type %in% e_type) %>% 
    group_by(dest_country_name, dest_country_code, dest_city, dest_ip,dest_long,dest_lat,
           src_country_name, src_country_code, src_city, src_ip,src_long,src_lat) %>%
    summarise( popup_html = (paste(
      '<div class="alert_details_name"><h4>', unique(dest_city), " (", unique(dest_country_code) , ')<h4></div>',
      '<div class="alert_details">IP Address', unique(dest_ip), '</div>',
      '<div class="alert_details">Traffic Type(s):',paste(Filter(Negate(is.na),unique( app_proto)),collapse =',\n'), '</div>',
       # '<div class="alert_details">Requests:', n(), '</div>',
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
      )
      ),
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
    mutate(total_bytes_pct = round(bytes/total_bytes,2))
  
  return(
    df_grouped
  )
}

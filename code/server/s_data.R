
# An empty prototype of the data frame we want to create
# prototype <- data.frame(date = character(), time = character(),
#                         size = numeric(), r_version = character(), r_arch = character(),
#                         r_os = character(), package = character(), version = character(),
#                         country = character(), ip_id = character(), received = numeric())

dash_values <- reactiveValues(
  redis_last_total = 0,
  redis_last_timestamp_num = 0
)

prototype <- data.frame(
  timestamp = as_datetime(x = integer(0)),
  flow_id = character(),
  in_iface = character(),
  event_type = character(),
  src_ip = character(),
  src_port = character(),
  dest_ip = character(),
  dest_port = character(), 
  proto = character(),
  host = character(),
  app_proto = character(),
  tx_id = character(), 
  payload = character(),
  payload_printable = character(),
  stream = character(),
  packet = character(),
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
  flow.pkts_toserver = numeric(),
  flow.pkts_toclient = numeric(),
  flow.bytes_toserver = numeric(),
  flow.bytes_toclient = numeric(),
  flow.start = character(),
  packet_info.linktype = character(),
  app_proto_ts = character(), 
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
  dhcp.dhcp_type = character(),
  country_code = character(),
  city = character(),
  lat = numeric(),
  long = numeric(),
  received = numeric(),
  timestamp_num = numeric()
)




alertStream <- function(session) {
  # Connect to data source
  
  if (! redux::redis_available(host = 'unraiden.local'))
    return()
  
  r <- redux::hiredis(host = 'unraiden.local')
  r$PING()
  
  total_entries <- r$LLEN(key='suricata')
  
  # Returns new lines
  newLines <- reactive({
    
    last_total_entries <- isolate(dash_values$redis_last_total)
    total_entries <- r$LLEN(key='suricata')
    print(paste("last total:", last_total_entries, "new total:",total_entries ))
    if(last_total_entries == 0 || (last_total_entries + initial_history_load_size) < total_entries ){
      new_entries <- initial_history_load_size
      if(last_total_entries == 0)
        last_total_entries <- total_entries -  (initial_history_load_size * 1)
    }else{
      if(last_total_entries > total_entries){
        new_entries <- default_load_size
        last_total_entries <- total_entries - default_load_size
      }else{
        if((total_entries - last_total_entries) > max_history_load_size)
          last_total_entries <- total_entries -  max_history_load_size
        new_entries <- total_entries - last_total_entries
      }
    }
        
    print(paste("new lines to load:",new_entries))
    if(is.null(input$data_refresh_rate) || (last_total_entries + new_entries) < total_entries ){
      invalidateLater(1000 * 3, session)
    }else{
      if(is.numeric(input$data_refresh_rate)){
        invalidateLater(1000 * input$data_refresh_rate, session)
      }
    }
    
    
    # parse_json_line <- function(json){
    #   jsonlite::flatten(as.data.frame(jsonlite::parse_json(json[[1]])))
    # }
    
    newlines <- ""
    if(new_entries > 0){
      tryCatch(
        {
          #query_time <- r$TIME
          new_json <- r$LRANGE(key='suricata',last_total_entries,last_total_entries + new_entries -1)
          
          write(unlist(new_json),tmp_json_file)
          new_lines <- jsonlite::stream_in(file(tmp_json_file)) %>%
                jsonlite::flatten() 
          #  unlink(tmp_json_file)
          
        },
        error = function(c) {
          new_entries <- 0
          msg <- conditionMessage(c)
          message(c)
          invisible(structure(msg, class = "try-error"))
        }
      )
      
      dash_values$redis_last_total <- last_total_entries + new_entries
    }
    new_lines
    
  })
  
  # Parses newLines() into data frame
  reactive({
    
    redis_last_timestamp_num <- isolate(dash_values$redis_last_timestamp_num)
    
    #new_lines <- future(newLines()) %>%
    new_lines <- newLines()
    
    # result <- new_lines %>%
    #   lapply(jsonlite::parse_json) %>%
    #   lapply(unlist) 
    # 
    
    if (length(new_lines) == 0)
      return()
    
    tryCatch(
      {
        result <- new_lines %>%
          subset_colclasses( colclasses = c("numeric","character","factor", "integer")) %>%
          mutate(timestamp = as_datetime(timestamp)) %>%
          mutate(timestamp_num = as.numeric(timestamp)) %>%
          iplookup %>%
          mutate(received = as.numeric(Sys.time())) %>%
          filter(timestamp_num > redis_last_timestamp_num) %>%
          as.data.frame()
        
        #result <- result[,!sapply(result,is.list)]
        
        print(paste("Columns not in prototype:", 
                    paste(collapse = ",",setdiff(names(result),names(prototype)))))
        
        if(nrow(result) == 0)
          return()
        
        # Find names of missing columns and add them filled with NA's
        missing <- setdiff(names(prototype), names(result))  
        if(length(missing)>0){result[missing] <- NA }
        
      }, 
      error = function(c) {
        
        msg <- conditionMessage(c)
        message(c)
        #invisible(structure(msg, class = "try-error"))
        # return(prototype)
        
      }
    )
    
    
    print(paste("last timestamp:",result[nrow(result),]$timestamp))
    dash_values$redis_last_timestamp_num <- result[nrow(result),]$timestamp_num
    
    # Return result values for columns in prototype in a desired order 
    return(result[names(prototype)])
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
      
      if(e_type == '')
        e_type <- event_types
      
      hide_waiter()
      
      df %>%
        filter(event_type %in%  e_type ) %>%
        select( names(prototype) ) %>%
        rbind(memo) %>%
        filter(timestamp_num > as.numeric(Sys.time()) - timeWindow)
    }else{
      memo
    }
  }, prototype)
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
    
    df <- df %>% 
      filter((round(as.numeric(http.status)/100,1) == 2),
             event_type == 'http')
    memo + nrow(df)
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

# Count the total nrows of distinct alrtStream$dest_ip
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


event_count_prototype <- data.frame(event_type =  event_types,
                                    event_count = 0
) %>% mutate(event_type = as.character(event_type))


eventCount <- function(alrtStream, timeWindow, event_type = '') {
  shinySignals::reducePast(alrtStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    e_count <- df %>% group_by(event_type = event_type) %>% summarise(event_count = n()) %>% as.data.frame() 
    bind_rows(e_count, 
              memo ) %>% 
      group_by(event_type) %>% 
      summarise(event_count = sum(event_count)) %>%
      as.data.table()
    
  }, event_count_prototype
  )
}

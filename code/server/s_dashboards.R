
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
  timestamp = character(),
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
  dns.aa = character(),dns.answers = character(), 
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
  country_code = character(),
  city = character(),
  lat = numeric(),
  long = numeric(),
  received = numeric(),
  timestamp_num = numeric()
)




alertStream <- function(session) {
  # Connect to data source
  r <- redux::hiredis(host = 'unraiden.local')
  r$PING()
  
  
  total_entries <- r$LLEN(key='suricata')
  
  # Returns new lines
  newLines <- reactive({
    
    last_total_entries <- isolate(dash_values$redis_last_total)
    total_entries <- r$LLEN(key='suricata')
    print(paste("last total:", last_total_entries, "new total:",total_entries ))
    if(last_total_entries <= total_entries && (total_entries - last_total_entries)  < 5000){
      new_entries <- total_entries - last_total_entries
    }else{
      new_entries <- 5000
      # if((total_entries - last_total_entries)  > 25000){
      #   last_total_entries <- total_entries - 25000
      # }
    }
    
    print(paste("new lines to load:",new_entries))
    invalidateLater(1000 * 10, session)
    
    newlines <- ""
    if(new_entries > 0){
      tryCatch(
        newlines <- r$LRANGE(key='suricata',last_total_entries,last_total_entries + new_entries), 
        error = function(c) {
          new_entries <- 0
          msg <- conditionMessage(c)
          message(c)
          invisible(structure(msg, class = "try-error"))
        }
      )
      
      dash_values$redis_last_total <- last_total_entries + new_entries
    }
    newlines
    
  })
  
  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    
    write(unlist(newLines()),tmp_json_file)
    
    parse_json_line <- function(json){
      jsonlite::flatten(as.data.frame(jsonlite::parse_json(json[[1]])))
    }
    
    
    iplookup <- function(x){
      iplookup_db_file <- iplookup_db_file
      cbind(x, rgeolocate::ip2location(ips = x$dest_ip, file = iplookup_db_file, fields = c('country_code','city','lat','long')))
    }

    result <- as.data.table(jsonlite::flatten(jsonlite::stream_in(file(tmp_json_file)))) %>%
      mutate(timestamp_num = as.numeric(as_datetime(timestamp))) %>%
      filter(timestamp_num > isolate(dash_values$redis_last_timestamp_num)) %>%
      iplookup %>%
      mutate(received = as.numeric(Sys.time()))
    #unlink(tmp)
    
    dash_values$redis_last_timestamp_num <- result[nrow(result),]$timestamp_num
    
    #print(names(result))
    
    missing <- setdiff(names(prototype), names(result))  # Find names of missing columns
    
    print(paste("missing cols:",missing ))
    if(length(missing)>0){result[missing] <- NA }                  # Add them, filled with NA's
    result <- result[names(prototype)]       # Put columns in desired order    
        
    result
  })
}


# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
alertData <- function(alrtStream, timeWindow, event_type = "http") {
  shinySignals::reducePast(alrtStream, function(memo, value,e_type = event_type) {
    print(paste(event_type , 'results:', nrow(filter(value, event_type == e_type))))
    print(paste('filtered', e_type , 'results:', nrow(filter(value, event_type == e_type) %>%
                                                   select( names(prototype) ) %>%
                                                   rbind(memo) %>%
                                                   filter(timestamp_num > as.numeric(Sys.time()) - timeWindow)
                                                 )))
    filter(value, event_type == e_type) %>%
    select( names(prototype) ) %>%
    rbind(memo) %>%
    filter(timestamp_num > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}


# Count the total nrows of alrtStream
requestCount <- function(alrtStream, event_type = "http") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    memo + nrow(df %>% filter(event_type == e_type))
  }, 0)
}

# Count the total nrows of distinct alrtStream$dest_ip
destinationCount <- function(alrtStream, event_type = "http") {
  shinySignals::reducePast(alrtStream, function(memo, df, e_type = event_type) {
    if (is.null(df))
      return(memo)
    memo + (df %>% filter(event_type == e_type)%>% group_by(dest_ip) %>% summarise() %>% nrow())
  }, 0)
}

# Functions including SQL scripts

## returns either a valid connection or NULL for failure
safe_redis <- function(host = redis_host) {
  vaild_connection <- FALSE
  
  if (!redux::redis_available(host = host)){
    print(paste(Sys.time() ,'Error- Unavailable redist host:',host ))
    redis_conn <- NULL
  }
  
  if(exists('redis_conn')){
    tryCatch({
      redis_conn$PING()
      vaild_connection <- TRUE
      },
      error = function(c) {
        new_entries <- 0
        msg <- paste('redis ping error:',conditionMessage(c))
         message(c)
      }
    )
  }
  
  if(! vaild_connection ){
    print(paste(Sys.time() ,'Creating new connection to Redist host:',host ))
    redis_conn <- NULL
    tryCatch({
      redis_conn <- redux::hiredis(host = host)
      redis_conn$PING()
      vaild_connection <- TRUE
    },
      error = function(c) {
        new_entries <- 0
        msg <- paste('new redis connection failed to ping:',conditionMessage(c))
        message(c)
      }
    )
    
  }
  if(vaild_connection){
    return(redis_conn)
  }else{
    print(paste(Sys.time() ,'Error - unable to establish redis connection to:',host ))
    
    return(NULL)
  }
}


get_redis_json <- function(host = redis_host, 
                           key = redis_key, 
                           index_last_loaded = 0, 
                           timestamp_last_loaded = 0){
  
  # Check if redis server connection exists, create if not
  redis_conn <- safe_redis(host)
  
  index_last_loaded <- as.integer(index_last_loaded)
  index_end <- as.integer(redis_conn$LLEN(key=key))
  
  print(paste(Sys.time() ,"last load index_end:", index_last_loaded, "current index_end:",index_end ))
  
  index_load_start <- index_last_loaded
  index_load_end <- index_end
  if(index_last_loaded == 0){
    index_load_start <- as.integer(index_end - min(initial_history_load_size,index_end))
    index_load_end <- as.integer(min(initial_history_load_size,index_end,max_load_size))
  }else{
    if(index_last_loaded > index_end){
      index_load_start <- 0
      index_load_end <- as.integer(min(index_end,max_load_size))
    }else{
      # if((index_end - index_last_loaded) > max_load_size)
      # index_load_start <- index_end -  max_load_size
      index_load_start <- index_last_loaded + 1
      index_load_end <- as.integer(index_last_loaded + min(index_end - index_last_loaded, max_load_size))
    }
  }
  
  print(paste(Sys.time() ,"load index_start:", index_load_start, "load index_end:", index_load_end,  "current index_end:",index_end ))
  
  new_json <- NULL
  
  if (redux::redis_available(host = host) ||  index_end > 0 || new_entries > 0){
    tryCatch(
      {
        {
          new_json <- redis_conn$LRANGE(key=key,as.integer(index_load_start),as.integer(index_load_end)) 
          
          index_last_loaded <- index_load_end

        }
      },
      error = function(c) {
        new_entries <- 0
        msg <- conditionMessage(c)
        message(c)
        invisible(structure(msg, class = "try-error"))
      }
    )
  }
  
  if(! is.null(new_json) && length(new_json) > 0){
    
    min_timestamp <- (
      fromJSON((new_json[1][[1]]))$timestamp %>%
        as_datetime()
    )
    
    if(is.double(min_timestamp))
      timestamp_first_loaded <- min_timestamp
    
    max_timestamp <- (
      fromJSON((new_json[length(new_json)])[[1]])$timestamp %>%
        as_datetime()
    )
    
    if(is.double(max_timestamp))
      timestamp_last_loaded <- max_timestamp
    
    print(paste(Sys.time() ,"new_lines:",length(new_json), "timestamp_last_loaded:", timestamp_last_loaded))
  }else{
    print(paste(Sys.time() ,"no new_lines,", "timestamp_last_loaded:", timestamp_last_loaded))
  }
  
  list(new_json = new_json, index_last_loaded = as.integer(index_last_loaded), index_end = as.integer(index_end)
       , timestamp_first_loaded = timestamp_first_loaded, timestamp_last_loaded = timestamp_last_loaded)
}


get_redis_list <- function(host = redis_host, 
                           key = redis_key, 
                           index_last_loaded = 0, 
                           timestamp_last_loaded = 0){
  
  # Check if redis server connection exists, create if not
  redis_conn <- safe_redis(host)
  
  index_last_loaded <- as.integer(index_last_loaded)
  index_end <- redis_conn$LLEN(key=key)
  
  print(paste(Sys.time() ,"last load index_end:", index_last_loaded, "current index_end:",index_end ))
  
  index_load_start <- index_last_loaded
  index_load_end <- index_end
  if(index_last_loaded == 0){
    index_load_start <- index_end - min(initial_history_load_size,index_end)
    # index_load_end <- min(initial_history_load_size,index_end,max_load_size)
  }else{
    if(index_last_loaded > index_end){
      index_load_start <- 0
      index_load_end <- min(index_end,max_load_size)
    }else{
      # if((index_end - index_last_loaded) > max_load_size)
        # index_load_start <- index_end -  max_load_size
      index_load_start <- index_last_loaded + 1
      index_load_end <- index_last_loaded + min(index_end - index_last_loaded, max_load_size)
    }
  }
  
  print(paste(Sys.time() ,"load index_start:", index_load_start, "load index_end:", index_load_end,  "current index_end:",index_end ))
  
  new_lines <- NULL
  
  if (redux::redis_available(host = host) && index_load_end - index_load_start > 0){
    tryCatch(
      {
        {
          new_json <- redis_conn$LRANGE(key=key,as.integer(index_load_start),as.integer(index_load_end)) 
          if(length(new_json) == 0){
            print(paste(now(), 'Error - redis query returned no data'))
          }else{
            new_lines <- fromJSON(paste('[', paste(new_json,collapse = ','),']')) %>%
              mutate(timestamp = as_datetime(timestamp)) %>%
              filter(timestamp >= timestamp_last_loaded) %>%
              mutate(timestamp_num = as.numeric(timestamp - round(unclass(timestamp)))) 
            
            index_last_loaded <- index_load_end
          }
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
        }
      },
      error = function(c) {
        new_entries <- 0
        msg <- conditionMessage(c)
        message(c)
        invisible(structure(msg, class = "try-error"))
      }
    )
  }
  
  if(! is.null(new_lines) && nrow(new_lines) > 0){
    max_timestamp <- max(new_lines$timestamp)
    if(is.double(max_timestamp))
      timestamp_last_loaded <- max(new_lines$timestamp)
    print(paste(Sys.time() ,"new_lines:",nrow(new_lines), "timestamp_last_loaded:", timestamp_last_loaded))
  }else{
    print(paste(Sys.time() ,"no new_lines,", "timestamp_last_loaded:", timestamp_last_loaded))
  }
  
  list(new_lines = new_lines, index_last_loaded = index_last_loaded, index_end = index_end, timestamp_last_loaded = timestamp_last_loaded)
}
  
# Parses new_lines into data frame
format_redis_to_df <- function(new_lines = new_lines, 
                              data_row_template = data_row_template){
  
    if(is.null(new_lines) || nrow(new_lines) == 0 )
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
    
    missing_columns_from_template <- setdiff(names(formatted_lines),names(data_row_template))
    print(paste(Sys.time() ,"Columns not in data_row_template:", 
                paste(collapse = ",",missing_columns_from_template)))
    
    # Find names of missing columns and add them filled with NA's
    missing <- setdiff(names(data_row_template), names(formatted_lines))  
    if(length(missing)>0){formatted_lines[missing] <- NA }
    
    print(paste(Sys.time() ,"last timestamp:",formatted_lines[nrow(formatted_lines),]$timestamp))
    
    # Return result values for columns in data_row_template in a desired order 
    return(formatted_lines[names(data_row_template)])
}


load_history_from_rds <- function(path = history_folder,hours_to_load = 9){
  load_start_time <- Sys.time()
  history_files <- as.data.table(cbind(file = list.files( path, recursive = TRUE, pattern = "\\.RDS$", full.names = TRUE) ) ) %>%
    filter( as.numeric(substr(file, start= nchar(file) - 13, stop = nchar(file)-3)) >=  as.numeric(Sys.time() - (hours_to_load  * 60 * 60)))
  history <- lapply(history_files$file,readRDS)
  load_duration = Sys.time() - load_start_time
  print(paste(Sys.time() ,'files: ', nrow(history_files),'; duration: ',round(load_duration,2), 's; time per file: ', round(load_duration / nrow(history_files),2), 's', sep = ''))
  hd <- rbind(history)
}

## Data row template

# http.http_refer,http.dnt,http.cookie,http.cache_control,http.pragma,http.location,http.redirect,tftp.packet,tftp.file,tftp.mode
# http.dnt 
# 

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
  app_proto = factor(),
  app_proto_tc = factor(),
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
  alert.metadata.signature_severity = character(),
  alert.metadata.tag = character(),
  alert.metadata.deployment = character(),
  alert.metadata.attack_target = character(),
  alert.metadata.affected_product = character(),
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
  dhcp.dhcp_type = character(),
  ikev2.version_major = character(),
  ikev2.version_minor = character(),
  ikev2.exchange_type = character(),
  ikev2.message_id = character(),
  ikev2.init_spi = character(),
  ikev2.resp_spi = character(),
  ikev2.role = character(),
  ikev2.errors = character(),
  ikev2.payload = character(),
  ikev2.notify = character(),
  ikev2.alg_enc = character(),
  ikev2.alg_auth = character(),
  ikev2.alg_prf = character(),
  ikev2.alg_dh = character(),
  ikev2.alg_esn = character()
)


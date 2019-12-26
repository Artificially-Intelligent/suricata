source('global.R', local = TRUE)

hours_to_keep = 24
load_interval <- 300
run_load_loop <- TRUE

initial_history_load_size <- 10000000

## create db connection
db_pool <- safe_pool()
redis_conn <- safe_redis(redis_host)

redis_index_last_loaded = 0
redis_timestamp_last_loaded = get_timestamp_last_loaded(as.numeric(0))
load_start_time <- Sys.time()

load_complete = TRUE
while(run_load_loop){
  loop_start_time <- Sys.time()
  
  if(load_complete){
    load_batch_start_time <- loop_start_time
    print(paste(load_batch_start_time ,'batch load started'))
  }
  load_complete = FALSE
  
  print(paste(loop_start_time ,'interval load started'))
  
  
  db_write_result = FALSE
  
  meta_table_exists <- dbExistsTable(conn = db_pool, 'meta_lnd_suricata')
  if(redis_timestamp_last_loaded == 0 && meta_table_exists == TRUE){
    mysql_timestamp_last_loaded <- as.numeric( dbReadTable(conn = db_pool,'meta_lnd_suricata') %>%
                                                 filter(! is.na(timestamp_num_max)) %>%
                                                 summarise(mysql_timestamp_last_loaded = max(timestamp_num_max))
    )
    if(! is.null(mysql_timestamp_last_loaded) && ! is.na(mysql_timestamp_last_loaded))
      redis_timestamp_last_loaded = mysql_timestamp_last_loaded
  }
  
  if(is.null(safe_redis(redis_host))){  
    print(paste('Unable to connect to redis db "',redis_host,'" waiting', load_interval , 'seconds and then retrying...'))
    Sys.sleep(load_interval)
  }else{
    redis_json <- get_redis_json(host = redis_host, 
                                    key = redis_key, 
                                    index_last_loaded = redis_index_last_loaded, 
                                    timestamp_last_loaded = redis_timestamp_last_loaded)
    
    print(paste(Sys.time() ,'redis query finished'))
    
    redis_index_end <- redis_json$index_end
    
    json_tbl <- as.data.table(unlist(redis_json$new_json))
    names(json_tbl) <- 'eve_log'
    
    db_pool <- safe_pool()
    
    db_write_result = FALSE
    if(nrow(json_tbl) > 0)
      db_write_result <- dbWriteTable(conn = db_pool, 
                                    name = "lnd_suricata", 
                                    value = json_tbl, 
                                    append = TRUE,
                                    
                                    row.names = FALSE)
    
    
    loop_end_time <- Sys.time()
    
    if(db_write_result){
      redis_index_last_loaded <- redis_json$index_last_loaded
      redis_timestamp_last_loaded <- redis_json$timestamp_last_loaded
      
      if(nrow(json_tbl) > 0){
        print(paste(Sys.time() ,'writing metadata to db'))
        timestamp_min = min(redis_json$timestamp_first_loaded)
        timestamp_max = max(redis_json$timestamp_last_loaded)
        
        
        load_metadata <- as.data.frame(cbind(batch_start = load_batch_start_time, 
                                             start= loop_start_time, end = loop_end_time
                                             , duration = as.numeric(loop_end_time - loop_start_time)
                                             , rows = nrow(json_tbl)
                                             , timestamp_num_min = timestamp_min
                                             , timestamp_num_max = timestamp_max
                                             , interval =  timestamp_max - timestamp_min
                                              
        ))
        
        dbWriteTable(conn =db_pool, 
                     name = "meta_lnd_suricata", 
                     value = load_metadata, 
                     append = TRUE,
                     row.names = FALSE)
      }
    }
    print(paste(loop_end_time ,'interval load finished'))
  }
  
  if(redis_index_end == redis_index_last_loaded){
    print(paste(Sys.time() ,'batch load finished'))
    load_complete = TRUE
    
    ## remove writen to db entries from redis
    if(redis_conn$LLEN(redis_key) >= redis_index_last_loaded){
      print(paste(Sys.time() ,'removing batch load from redis'))
      redis_conn$LTRIM(redis_key,redis_index_last_loaded,-1)
      redis_index_last_loaded <- 0
    }
    
    batch_duration <- as.numeric(Sys.time() - load_batch_start_time, units = "secs")
    sleep_interval <- load_interval - batch_duration
    if(sleep_interval > 0){
      print(paste(Sys.time() ,'load batch duration', round(batch_duration,2), 'seconds, waiting',round(sleep_interval,0), 'seconds' ))
      Sys.sleep(sleep_interval)
    }
  }
}


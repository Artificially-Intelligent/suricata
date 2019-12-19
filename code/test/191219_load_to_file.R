source('global.R', local = TRUE)

hours_to_keep = 24
load_interval <- 300
run_load_loop <- TRUE

initial_history_load_size <- 10000000

redis_index_last_loaded <- 0
redis_timestamp_last_loaded <- 0
load_start_time <- Sys.time()

parentfolder <- file.path('..','data',redis_key)
outfolder <- file.path(parentfolder,format(load_start_time, '%s'))

# unlink(parentfolder, recursive = TRUE)
dir.create(parentfolder, showWarnings = TRUE)
dir.create(outfolder, showWarnings = TRUE)

duration_outfile <- file.path(outfolder,'load_duration.csv')

durations <- c() 

load_complete = TRUE
while(run_load_loop){
  loop_start_time <- Sys.time()
  if(load_complete)
    load_batch_start_time <- Sys.time()
  load_complete = FALSE
  outfile <- file.path(outfolder
                       , paste(format(loop_start_time, '%s')
                       ,'.RDS', sep = '')
  )
    
  redis_results <- get_redis_list(host = redis_host, 
    key = redis_key, 
    index_last_loaded = redis_index_last_loaded, 
    timestamp_last_loaded = redis_timestamp_last_loaded)

  redis_index_end <- redis_results$index_end
  redis_index_last_loaded <- redis_results$index_last_loaded
  redis_timestamp_last_loaded <- redis_results$timestamp_last_loaded
  
  redis_results$new_lines %>%
    format_redis_to_df(data_row_template = data_row_template) %>%
    saveRDS(file = outfile)
  

  loop_end_time <- Sys.time()
  
  duration_row <- cbind(batch_start = load_batch_start_time, 
                        start= loop_start_time, end = loop_end_time
                    , duration = as.numeric(loop_end_time - loop_start_time)
                    , rows = nrow(redis_results$new_lines)) 
  durations <- c(durations,duration_row)
  print(paste('interval load finished'))
  
  if(redis_index_end == redis_index_last_loaded){
    print(paste('full load finished'))
    load_complete = TRUE
    write.csv2(durations, file = duration_outfile)

    old_files <- as.data.frame(cbind(file=  list.files(outfolder, pattern = "\\.RDS$") )) %>%
      filter( as.numeric(substr(file, start= 1, stop = 10)) <  as.numeric(Sys.time() - (hours_to_keep  * 60 * 60)))
    
    if(nrow(old_files) > 0){
      print(paste('removing', nrow(old_files), 'old RDS files'))
      file.remove(file.path(outfolder,old_files$file))
    }
      
    sleep_interval <- load_interval - as.numeric(loop_end_time - load_batch_start_time)
    if(sleep_interval > 0){
      print(paste('waiting',round(sleep_interval,0), 'seconds' ))
      Sys.sleep(sleep_interval)
    }
  }
}
  

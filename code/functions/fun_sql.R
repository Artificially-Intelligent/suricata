# Functions including SQL scripts

## create db connection
if(! exists('db_pool'))
  db_pool <- pool::dbPool(drv = RMySQL::MySQL(), host = mysql_db$host , dbname = mysql_db$dbname, 
                        user = mysql_db$user, password = mysql_db$password)


safe_pool <- function() {
  tryCatch({
    # try a fast query
    dbExecute(db_pool, "SELECT 1")
  }, error = function(e) {
    if (e$message %in% c("RS-DBI driver: (could not run statement: no connection to the server/n)"
                         ,"This pool is no longer valid. Cannot fetch new objects."
      )) {
      # create a new pool in enclosing environment
      try(poolClose(db_pool))
      
      db_pool <<- 
        dbPool(drv = RMySQL::MySQL(), host = mysql_db$host , dbname = mysql_db$dbname, 
                     user = mysql_db$user, password = mysql_db$password)
      #   dbPool(
      #   drv = PostgreSQL(),
      #   host = Sys.getenv("DB_HOST"),
      #   port = 5432,
      #   dbname = Sys.getenv("DB_NAME"),
      #   user = Sys.getenv("DB_USER"),
      #   password = Sys.getenv("DB_PASSWORD")
      # )
    } else {
      # there was an unknown error
      stop(e)
    }
  })
  
  db_pool
}

onStop(function() {
  try(
    poolClose(db_pool),
    silent = TRUE
  )
})


get_timestamp_last_loaded <- function(redis_timestamp_last_loaded = 0){
  meta_table_exists <- dbExistsTable(conn = db_pool, 'meta_lnd_events')
  if(redis_timestamp_last_loaded == 0 && meta_table_exists == TRUE){
    mysql_timestamp_last_loaded <- as.numeric((dbReadTable(conn = db_pool,'meta_lnd_events') %>%
                                                 filter(! is.na(timestamp_num_max)) %>%
                                                 summarise(mysql_timestamp_last_loaded = max(timestamp_num_max))
    )
    )
    if(! is.null(mysql_timestamp_last_loaded) && ! is.na(mysql_timestamp_last_loaded))
      redis_timestamp_last_loaded = mysql_timestamp_last_loaded
  }
  redis_timestamp_last_loaded
}

update_location_db <- function(iplookup_db_csv = iplookup_db_csv, ip2proxy_table ="ip2proxy_px8", overwrite = FALSE){
  iplookup_db <- read.csv(iplookup_db_csv)
  names(iplookup_db) <- c('ip_from','ip_to','proxy_type','country_code','country_name','region_name'
  ,'city_name','isp','domain','usage_type','asn','as','last_seen')
  
  if(dbExistsTable(db_pool, name = ip2proxy_table) && ! overwrite){
    print('table exists and overwrite not selected. Leaving as is.')
  }else{
    if(! dbExistsTable(db_pool, name = ip2proxy_table)){
      dbExecute(db_pool,
        paste(sep = '',
            'CREATE TABLE `', ip2proxy_table ,'`(
          	`ip_from` INT(10) UNSIGNED,
          	`ip_to` INT(10) UNSIGNED,
          	`proxy_type` VARCHAR(3),
          	`country_code` CHAR(2),
          	`country_name` VARCHAR(64),
          	`region_name` VARCHAR(128),
          	`city_name` VARCHAR(128),
          	`isp` VARCHAR(256),
          	`domain` VARCHAR(128),
          	`usage_type` VARCHAR(11),
          	`asn` INT(10),
          	`as` VARCHAR(256),
          	`last_seen` INT(10) UNSIGNED,
          	PRIMARY KEY (`ip_from`, `ip_to`)
          ) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;'
        )
      )
    }else{
      dbExecute(db_pool,
                paste(sep = '',
                      'Truncate TABLE `', ip2proxy_table ,'` ; '
                )
      )
    }
    dbWriteTable(conn = db_pool, 
                 name = "ip2proxy_px8", 
                 value = iplookup_db, 
                 overwrite = TRUE,
                 row.names = FALSE)
  }
}

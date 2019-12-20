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
    if (e$message %in% c("RS-DBI driver: (could not run statement: no connection to the server\n)"
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
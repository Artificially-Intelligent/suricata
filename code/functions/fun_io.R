# Functions for connecting, loading, saving data (only include generic SQL)

## Constants 
data_path <- "data/"

## Use RDS Data 
load_data_sheet <- function(saved_file){
  data_sheet_path <- paste0(data_path, saved_file, '.rds')
  df <- readRDS(data_sheet_path)
  return(df)
}

## Use MariaDB 
db_connection <- function(db_name){
  dbConnect(MySQL(),
            user="user", password="password",
            dbname=db_name, 
            host="localhost", 
            port = 3306
  )
}


close_mysql_connections <- function(){
  lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect)
}

get_mysql_data <- function(db_name, sql_string){
  close_mysql_connections()
  rs = dbSendQuery(db_connection(db_name), sql_string)
  df <- fetch(rs, n=-1)
  close_mysql_connections()
  df  
}

put_mysql_data <- function(df, table_name, db_name){
  close_mysql_connections()
  write_csv(df, "temp.csv", col_names = FALSE)
  sql_string <- paste(
    "LOAD DATA LOCAL INFILE 'temp.csv' INTO TABLE", 
    table_name, 
    "FIELDS TERMINATED by ',' ENCLOSED BY '\"' LINES TERMINATED BY '\\n'"
  )
  dbSendQuery(db_connection(db_name), sql_string)
  close_mysql_connections()
}


update_refresh_rate <- function( output_type = 'dash', inputId = "data_refresh_rate", refresh_rates_df = refresh_rates){
   
}

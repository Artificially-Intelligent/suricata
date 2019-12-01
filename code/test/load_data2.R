install.packages("tidyjson")
library('rredis')
library('redux')
library('jsonify')
library("rjson")
library("tidyjson")
library("dplyr")
library(data.table)
library(rgeolocate)




rredis::redisConnect(host = 'unraiden.local')
size <- rredis::redisDBSize()
rredis::redisExists('suricata')
rredis::redisHVals('suricata')
rredis::redisInfo()
rredis::
#redisSet('foo', runif(10))
#bar <- redisMGet('suricata') 
#bar


library(jsonlite)

library(purrr)

library(data.table)
r <- redux::hiredis(host = 'unraiden.local')
r$PING()
total_entries <- r$LLEN(key='suricata')
#raw <- r$LRANGE(key='suricata',1,total_entries)
json_raw <- r$LRANGE(key='suricata',1,2000)
tmp <- 'raw.json'
write(unlist(json_raw),tmp)

parse_json_line <- function(json){
  jsonlite::flatten(as.data.frame(jsonlite::parse_json(json[[1]])))
}

suricata <- as.data.table(jsonlite::flatten(jsonlite::stream_in(file(tmp))))
unlink(tmp)


json_list <- map(json_raw, fromJSON )
json_list_flat <- map(json_list, flatten)
json_list_flat <- map(json_list_flat, flatten)
json_list_flat <- map(json_list_flat, flatten)


df_list <- map(json_list_flat, as.data.table)
df <- rbindlist(df_list, fill = TRUE)

rm(raw)
rm(json_list)
rm(df_list)

levels( factor( df$event_type))

ip2location_db <- 'C:/Users/Stuart/Documents/Development/GitHub/shiny_suricata/IP2LOCATION-LITE-DB9.BIN'
df <- cbind(df, rgeolocate::ip2location(ips = df$dest_ip, file = ip2location_db, fields = c('country_code','city','lat','long')))

netflow <- as.data.table(df[df$event_type == 'netflow',])
netflow <- netflow[,which(unlist(lapply(netflow, function(x)!all(is.na(x))))),with=F]

flow <- as.data.table(df[df$event_type == 'flow',])
flow <-flow[,which(unlist(lapply(flow, function(x)!all(is.na(x))))),with=F]

alert <- as.data.table(df[df$event_type == 'alert',])
alert <- alert[,which(unlist(lapply(alert, function(x)!all(is.na(x))))),with=F]

drop <- as.data.table(df[df$event_type == 'drop',])
drop <- drop[,which(unlist(lapply(drop, function(x)!all(is.na(x))))),with=F]
drop <- drop[,which(unlist(lapply(drop, function(x)!all((x == 'NULL'))))),with=F]

dhcp <- as.data.table(df[df$event_type == 'dhcp',])
dhcp <- dhcp[,which(unlist(lapply(dhcp, function(x)!all(is.na(x))))),with=F]
dhcp <- dhcp[,which(unlist(lapply(dhcp, function(x)!all((x == 'NULL'))))),with=F]

dns <- as.data.table(df[df$event_type == 'dns',])
dns <- dns[,which(unlist(lapply(dns, function(x)!all(is.na(x))))),with=F]
dns <- dns[,which(unlist(lapply(dns, function(x)!all((x == 'NULL'))))),with=F]

fileinfo <- as.data.table(df[df$event_type == 'fileinfo',])
fileinfo <- fileinfo[,which(unlist(lapply(fileinfo, function(x)!all(is.na(x))))),with=F]
fileinfo <- fileinfo[,which(unlist(lapply(fileinfo, function(x)!all((x == 'NULL'))))),with=F]

tls <- as.data.table(df[df$event_type == 'tls',])
tls <- tls[,which(unlist(lapply(tls, function(x)!all(is.na(x))))),with=F]

http <- as.data.table(df[df$event_type == 'http',])
http <- http[,which(unlist(lapply(http, function(x)!all(is.na(x) || is.null(x))))),with=F]

rm(df)







values <- lapply(values_json,jsonlite::fromJSON, simplifyDataFrame = TRUE)


dd <- as.data.frame(t(as.data.frame(values)))
do.call(bind, values)

dd  <-  as.data.frame(matrix(unlist(values), nrow=length(unlist(values[1]))))

values_json <- paste("[",paste(unlist(values_json),collapse = ','),"]", sep='')


# Give the input file name to the function.

#df <- fromJSON(values_json)


df <- jsonlite::fromJSON(values_json, simplifyDataFrame = TRUE)
rm(values_json)

netflow <- as.data.table(df[df$event_type == 'netflow',])
netflow <- netflow[,which(unlist(lapply(netflow, function(x)!all(is.na(x))))),with=F]

flow <- as.data.table(df[df$event_type == 'flow',])
flow <-flow[,which(unlist(lapply(flow, function(x)!all(is.na(x))))),with=F]

rm(df)


#values_json %>%                  # %>% is the magrittr pipeline operator 
#  gather_array %>%          # gather (stack) the array by index
#  spread_values(            # spread (widen) values to widen the data.frame
#    name = jstring("flow.start"), # value of "name" becomes a character column
#    age = jnumber("flow.end")    # value of "age" becomes a numeric column
#  )


from_json(unlist(values))
values -> jsonify : :from_json()

unlist(values)
typeof(unlist(values))

df <- data.frame(matrix(unlist(values), nrow=length(values), byrow=T))

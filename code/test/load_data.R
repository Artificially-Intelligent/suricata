install.packages("tidyjson")
library('rredis')
library('redux')
library('jsonify')
library("rjson")
library("tidyjson")
library("dplyr")
library(data.table)




rredis::redisConnect(host = 'unraiden.local')
size <- rredis::redisDBSize()
rredis::redisExists('suricata')
rredis::redisHVals('suricata')
rredis::redisInfo()

#redisSet('foo', runif(10))
#bar <- redisMGet('suricata') 
#bar


library(jsonlite)
library(purrr)
library(data.table)

r <- redux::hiredis(host = 'unraiden.local')
r$PING()
total_entries <- r$LLEN(key='suricata')
raw <- r$LRANGE(key='suricata',1,total_entries)
#raw <- r$LRANGE(key='suricata',1,10000)

json_list <- map(raw, fromJSON)
df_list <- map(json_list, as.data.table)
df <- rbindlist(df_list, fill = TRUE)

rm(raw)
rm(json_list)
rm(df_list)

levels( factor( df$event_type))

netflow <- as.data.table(df[df$event_type == 'netflow',])
netflow <- netflow[,which(unlist(lapply(netflow, function(x)!all(is.na(x))))),with=F]

flow <- as.data.table(df[df$event_type == 'flow',])
flow <-flow[,which(unlist(lapply(flow, function(x)!all(is.na(x))))),with=F]

alert <- as.data.table(df[df$event_type == 'alert',])
alert <- alert[,which(unlist(lapply(alert, function(x)!all(is.na(x))))),with=F]

dhcp <- as.data.table(df[df$event_type == 'dhcp',])
dhcp <- dhcp[,which(unlist(lapply(dhcp, function(x)!all(is.na(x))))),with=F]

dns <- as.data.table(df[df$event_type == 'dns',])
dns <- dns[,which(unlist(lapply(dns, function(x)!all(is.na(x))))),with=F]

fileinfo <- as.data.table(df[df$event_type == 'fileinfo',])
fileinfo <- fileinfo[,which(unlist(lapply(fileinfo, function(x)!all(is.na(x))))),with=F]

tls <- as.data.table(df[df$event_type == 'tls',])
tls <- tls[,which(unlist(lapply(tls, function(x)!all(is.na(x))))),with=F]

http <- as.data.table(df[df$event_type == 'http',])
http <- http[,which(unlist(lapply(http, function(x)!all(is.na(x))))),with=F]

rm(df)

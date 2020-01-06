library(elastic)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

get_elastic_data <- function(
  host = 'unraiden.local'
  ,index = ""
  ,date_field = '@timestamp'
  ,date_interval = '1h'
  ,date_start = now(tzone = "UTC") - (86400/4)
  ,date_end = now(tzone = "UTC")
  ,slicer_fields = c('source.geo.latitude'
                     ,'source.geo.longitude'
                     ,'destination.geo.latitude'
                     ,'destination.geo.longitude')
  ,slicer_top_n = '10'
  ,filter_fields = 'suricata.eve.event_type.keyword'
  ,filter_values = ''
  ,measure_fields = c('suricata.eve.netflow.bytes','suricata.eve.netflow.pkts')
  ,tzone = 'Australia/Melbourne'
  ,event_types = c('http','flow')
){
  if(right(slicer_field,8) != '.keyword'){
    slicer_field = paste(slicer_field , '.keyword',sep = '')
  }
  
  es <- elastic::connect(host = host)
  
  
  date_end <- format(date_end, "%Y-%m-%dT%H:%M:00.000Z")
  date_start <- format(date_start, "%Y-%m-%dT%H:%M:00.000Z")


aggs <- paste(sep = '',
              '{
  "aggs": {
    "3": {
      "date_histogram": {
          "field": "', date_field, '",
          "calendar_interval": "', date_interval, '",
          "time_zone": "',tzone,'",
          "min_doc_count": 1
        },
      "aggs": {
        ' , 
        
        paste(sep = '', collapse = ',',  '"', slicer_fields ,'": {
          "terms": {
            "field": "', slicer_fields, '",
            "order": {
              "1": "desc"
            },
            "size": ', slicer_top_n , '
          },
          
          "aggs": {
                        ', 
              paste(sep = '', collapse = ',',  '"',1:length(measure_fields) ,'": {
                          "sum": {
                            "field": "', measure_fields ,'"
                          }
                        }'
                )
              )
              , paste(sep = '', collapse = '', rep('}',length(slicer_fields)*2)),'
      }
    }
  },
  "size": 0,
  "_source": {
    "excludes": []
  },
  "stored_fields": [
    "*"
  ],
  "script_fields": {},
  "docvalue_fields": [
    {
      "field": "@timestamp",
      "format": "date_time"
    },
    {
      "field": "received_at",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.flow.end",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.flow.start",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.netflow.end",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.netflow.start",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.timestamp",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.tls.notafter",
      "format": "date_time"
    },
    {
      "field": "suricata.eve.tls.notbefore",
      "format": "date_time"
    }
  ],
  "query": {
    "bool": {
      "must": [],
      "filter": [
        {
          "match_all": {}
        }' , 
              if(nchar(filter_values)>0 && nchar(filter_fields)>0){
                paste(sep ='',
                      ',
        {
          "terms": {
            "' , filter_fields ,'" : ["',paste(sep = '', collapse ='","', filter_values) ,'"]
            }
        }')
              }else{''}
              , '
          ,{
            "range": {
              "', date_field, '": {
                "format": "strict_date_optional_time",
                "gte": "' ,date_start, '",
                "lte": "' ,date_end, '"
              }
            }
        }
      ],
      "should": [],
      "must_not": []
    }
  }
}'
)

write(aggs, file= 'agg.json')

  if(nchar(index) == 0){
    out <- Search(es 
                  ,body = aggs
                  ,size = 0
    )
  }else{
    if(index_exists(es, index = index)){
      out <- Search(es 
                  # type=type 
                  # ,index = index
                  ,body = aggs
                  # ,fields = fields
                  # ,df = 'suricata.eve.event_type'
                  # ,q = '(alert)'
                  # ,fields = fields
                  ,size = 0
                  # ,from =100
      )
    }
  }

  parse_list <- function(x, level = 0){
    results_list <- x[sapply(x,typeof) == 'list']
    
    main_result_count <- sum(sapply(results_list,length) > 1)
    
    main_result <- results_list[sapply(results_list,length) > 1]
    
    
    if(main_result_count == 1){
      main_result <- main_result[[1]]
    }
    
    if(main_result_count > 1){
      main_result <- main_result[[length(main_result)]][[1]]
    }
    
    if(main_result_count == 0){
      main_result <- results_list
    }
    
    # x <- main_result$buckets[[1]]
    # names(x)
        
    if(! is.null(main_result$buckets) ){
      if(length(main_result$buckets) == 0)
        return(NULL)
      
      level = level + 1

      values <- lapply(main_result$buckets, function(x){parse_list(x, level = level)})
      # browser()
      values <- dplyr::bind_rows(values)
      values[,slicer_fields[level]] <- x$key
      return(values)
    }else{
      values <- data.frame(main_result)
      colnames(values) <- measure_fields
      
      values[,'count'] <- x$doc_count
      values[,slicer_fields[level+1]] <- x$key
      return(values)
    }
  }
  
  slicer_fields <- c('timestamp',slicer_fields)
  
  # x <- out$aggregations[[1]]$buckets[[1]]$suricata.eve.app_proto.keyword$buckets[1]
  # x <- out$aggregations[[1]]$buckets[1]
  x <- out$aggregations[[1]]$buckets
  df <- dplyr::bind_rows(lapply(x, function(x){parse_list(x, level = 0)}))
  failures <- paste(collapse = ',', out$`_shards`$failures)
  if(nchar(failures) > 0 )
    print(paste(now(), 'Error - Elasticsearch query error:',failures))
  if( nrow(df) > 0 && is.double(df$timestamp)){
     df$timestamp <- as_datetime(as.integer(df$timestamp/1000) , origin = lubridate::origin, tz = tzone)
    return(df[,c(slicer_fields,'count',measure_fields)])
  }
  return(df)
}
# 

# df <- get_elastic_data(measure_fields = c('suricata.eve.netflow.bytes','suricata.eve.netflow.pkts')
#                  ,slicer_fields = c('suricata.eve.app_proto.keyword','source.geo.latitude','source.geo.longitude','destination.geo.latitude','destination.geo.longitude') 
#                  ,filter_fields = c('suricata.eve.event_type.keyword')
#                  ,filter_values = c('netflow','http')
#                  # ,date_interval = '1m'
#                  # ,date_start = '2020-01-05T12:45:00.000Z'
#                  # ,date_end = '2020-01-05T13:05:00.000Z'
#                  )

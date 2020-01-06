library(elastic)

get_elastic_data <- function(
  host = 'unraiden.local'
  ,index = "pf-2020.01.04"
  ,date_field = 'suricata.eve.netflow.start'
  ,date_interval = '1m'
  ,date_start = '2020-01-04T06:00:00.000Z'
  ,date_end = '2020-01-04T12:00:00.000Z'
  ,slicer_field = 'suricata.eve.app_proto.keyword'
  ,slicer_top_n = '10'
  ,measure_field = 'suricata.eve.netflow.bytes'
){

  es <- elastic::connect(host = host)
  
  aggs <- paste(sep = '', '{
    "aggs": {
      "2": {
        "date_histogram": {
          "field": "', date_field, '",
          "calendar_interval": "', date_interval, '",
          "time_zone": "Australia/Sydney",
          "min_doc_count": 1
        },
        "aggs": {
          "3": {
            "terms": {
              "field": "', slicer_field, '",
              "order": {
                "1": "desc"
              },
              "size": ', slicer_top_n , '
            },
            "aggs": {
              "1": {
                "sum": {
                  "field": "', measure_field, '"
                }
              }
            }
          }
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
        "field": "suricata.eve.flow.start",
        "format": "date_time"
      },
      {
        "field": "suricata.eve.netflow.start",
        "format": "date_time"
      },
      {
        "field": "suricata.eve.timestamp",
        "format": "date_time"
      }
    ],
    "query": {
      "bool": {
        "must": [],
        "filter": [
          {
            "match_all": {}
          },
          {
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
  }')
  
  if(index_exists(es, index = index)){
    out <- Search(es 
               # type=type 
               ,index = index
               ,body = aggs
               # ,fields = fields
               # ,df = 'suricata.eve.event_type'
               # ,q = '(alert)'
               # ,fields = fields
               ,size = 0
               # ,from =100
               )
  
  dplyr::bind_rows(lapply(out$aggregations$`2`$buckets, 
     function(x){
       data_rows_list <- lapply(x$`3`$buckets, data.frame, stringsAsFactors = FALSE)
         if(length(data_rows_list) > 0){
            data_rows <- dplyr::bind_rows((data_rows_list))
            data_rows$timestamp <- lubridate::as_datetime(x$key_as_string)   
            data_rows
         }
       }
     ))
  }
}

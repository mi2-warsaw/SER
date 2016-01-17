#Instalacja i połączenie
install.packages("elastic")
library('elastic')
connect(url = 'http://127.0.0.1', es_port = 9200)
connection()
ping()
cluster_health()
cat_indices()
cat_health()
cat_nodes()

# Data
data(HairEyeColor)
hec <- as.data.frame(HairEyeColor)
head(hec)
nrow(hec)

#Documents
docs_bulk(hec, index ="haireyecolor", type = "haireyecolorentity")
alias_create(index = "haireyecolor", "hec")
count(index = "hec")
r0 <- docs_get(index = "hec", type = "haireyecolorentity", id = 1)
r0$`_source`
r1 <- docs_mget(index = "hec", type = "haireyecolorentity",
                        ids = c(1,2,3,4,5,6,7,8,9), raw = TRUE) # JSON
es_parse(r1)
# Search(bool query)
body<-'
  {"query":{
    "bool" : {
      "must" : {
        "term" : { "Sex" : "male" }
      },
      "filter": {
        "term" : { "Eye" : "blue"  }
      },
      "must_not" : {
        "range" : {
          "Freq" : { "from" : 0, "to" : 10 }
        }
      },
      "should" :
      {
        "term" : { "Hair" : "blond" }
      }
    }
  }
}'
Search(index="hec", body=body, asdf = TRUE)

#Agregacje
agg <- '{
  "aggs": {
    "by_hairs": {
      "terms": {
        "field": "Hair"
      },
      "aggs": {
        "count": {
          "sum": {
            "field": "Freq"
          }
        }
      }
    }
  }
}'
Search(index = "hec", body = agg, asdf = TRUE)

# kliki i showsy w teście a w sekcji news
'{
  "query" : {
    "filtered" : {
      "filter" : {
        "and" : {
          "filters" : [ {
            "terms" : {
              "test.raw" : [ "test:a" ]
            }
          }, {
            "terms" : {
              "section" : [ "news" ]
            }
          }, {
            "range" : {
              "@timestamp" : {
                "from" : "now-2h",
                "to" : "now"
              }
            }
          } ]
        }
      }
    }
  },
  "aggregations" : {
    "bycluster" : {
      "terms" : {
        "field" : "cluster",
        "size" : 10000
      },
      "aggregations" : {
        "bylink" : {
          "terms" : {
            "field" : "link.raw",
            "size" : 10000
          },
          "aggregations" : {
            "sumclicks" : {
              "sum" : {
                "field" : "clicks"
              }
            },
            "sumshows" : {
              "sum" : {
                "field" : "shows"
              }
            }
          }
        }
      }
    }
  }
}'

#Fuzzy queries
body <- '{"query": {"match" : {"Hair" : "Blond"}}}'
Search(index = "hec", body = body)

fuzzy_body <- '{
  "query": {
    "fuzzy": {
      "Hair": {
        "value": "Blend",
        "fuzziness": 2
      }
    }
  }
}'
Search(index = "hec", type = "haireyecolorentity", body = fuzzy_body, asdf = TRUE)

# Scrolls
res <- Search(index = "hec" , q="*", scroll="1m", search_type = "scan")
out <- list()
hits <- 1
while(hits != 0){
  res <- scroll(scroll_id = res$`_scroll_id`)
  hits <- length(res$hits$total)
  if(hits > 0)
    out <- c(out, res$hits$hits)
}
length(out)

# Percolators
percolator_body <- '{
  "query" : {
    "match" : {
      "Sex": "Female"
    }
  }
}'
percolate_register(index = "hec", id = 1, body = percolator_body)
doc <- '{
  "doc": {
    "Hair": "Dark",
    "Sex": "Female"
  }
}'
percolate_match(index = "hec", type = "my", body = doc)
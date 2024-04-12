library(mongolite)
library(data.table)
library(dplyr)


url = paste0("mongodb://backend:mc5HhbPp2UPgrjg2YsC9@mg7.sgrouples.com:27017/", 
              "?readPreference=secondary&directConnection=true&authSource=admin&sockettimeoutms=20000000")
url_notifications = paste0("mongodb://backend:mc5HhbPp2UPgrjg2YsC9@mg-notification3.sgrouples.com:27017/", 
                            "?readPreference=secondary&directConnection=true&authSource=admin&sockettimeoutms=20000000")
url_socgraph = paste0("mongodb://backend:mc5HhbPp2UPgrjg2YsC9@mg-socgraph3.sgrouples.com:27017/", 
                       "?readPreference=secondary&directConnection=true&authSource=admin&sockettimeoutms=2000000")
url_emojis = "mongodb://backend:mc5HhbPp2UPgrjg2YsC9@mg-emoji3.sgrouples.com:27017/?readPreference=secondary&directConnection=true&authSource=admin"
url_chats = "mongodb://backend:mc5HhbPp2UPgrjg2YsC9@mg-chat2.sgrouples.com:27017/?readPreference=secondary&directConnection=true&authSource=admin&sockettimeoutms=2000000"

mongo_users = mongo(db = "sgrouples", collection = "users", url = url)
mongo_groups = mongo(db = "sgrouples", collection = "privategroups", url = url)
mongo_sharedposts = mongo(db = "sgrouples", collection = "sharedposts", url = url)
mongo_mpostcomments = mongo(db = "sgrouples", collection = "mpostcomments", url = url)
mongo_membership = mongo(db = "sgrouples", collection = "memberships", url = url_socgraph)
mongo_mcontacts = mongo(db = "sgrouples", collection = "mcontacts", url = url_socgraph)
mongo_followrequests = mongo(db = "sgrouples", collection = "followrequests", url = url_socgraph)
mongo_minvitations = mongo(db = "sgrouples", collection = "minvitations", url = url_socgraph)
mongo_entityemoji = mongo(db = "sgrouples-emoji", collection = "entityemojis", url = url_emojis)
mongo_useremojis = mongo(db = "sgrouples-emoji", collection = "useremojis", url = url_emojis)
mongo_dsnp = mongo(db = "sgrouples", collection = "dsnpuser", url = url)
mongo_groupchatmessages = mongo(db = "sgrouples", collection = "groupchatmessages", url = url_chats)
mongo_userchatmessages = mongo(db = "sgrouples", collection = "userchatmessages", url = url_chats)
mongo_usertracing = mongo(db = "sgrouples", collection = "usertracing", url = url)


get_fields_query = function(fields) {
  if(is.null(fields))
    return("{}")
  res = paste('{',
              paste(paste0('"', fields, '":1'), collapse = ", "),
              '}')
  return(res)
}

timestamp_to_oid = function(x) {
  res = as.numeric(x) %>%
    floor %>%
    as.hexmode  %>%
    stringr::str_pad(width = 24, pad = "0", side = "right")
  return(res)
}

query_in = function(field, values) {
  paste0('{"',  field, '" :{"$in": [', paste0('{"$oid":"', values, '"}', collapse = ", "), ']}}')
}

query_oid_dates = function(d1 = NULL, d2 = NULL, id = "_id") {
  # assertthat::assert_that(is.null(d1) & is.null(d2))
  
  if (!is.null(d1))
    d1 = timestamp_to_oid(d1)
  if (!is.null(d2))
    d2 = timestamp_to_oid(d2)
  
  if (is.null(d1))
    return(paste0('{"', id, '": {"$lt":  {"$oid":"', d2, '"}}}'))
  
  if (is.null(d2))
    return(paste0('{"', id, '" : {"$gte": {"$oid":"', d1, '"}}}'))
  
  if (!is.null(d2) & !is.null(d1)) {
    query = paste0('{"', id, '" : {"$gte": {"$oid":"', d1, '"},
                            "$lt":  {"$oid":"', d2, '"}}}')
    return(query)
  }
  
  stop("d1 or d2 must be non null")
}


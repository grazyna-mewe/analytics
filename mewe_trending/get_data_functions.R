library(mongolite)
library(data.table)
source("scripts/mongo_most_popular.R")

get_mongo_data = function(collection,
                          query, 
                          fields,
                          db,
                          url, 
                          ...) {
  
  m = mongo(collection = collection,
            db = db,
            url = url)
  t1 = Sys.time()
  res = m$find(query, fields = get_fields_query(fields), ...)
  t2 = Sys.time()
  cat("Fetched", nrow(res), "rows in", t2 - t1)
  # res = tryCatch({
  #   as.data.table(res)
  # }, error = function(e) {
  #   warning(e)
  #   # print("Didn't convert to data table:", e)
  #   return(res)
  #   }
  # )
  return(res)
}

#id, nr of posts, nr of comments, nr of emojis, nr of members
get_mongo_groups = function(query = "{}",
                            fields = c("_id", "createdAt", "name", "description", "groupModelType",
                                       "groupThematicType", "membersCount", "showInPublicDirectory", "userPk",
                                       "locale", "isSpamDetected", "isBannedAsOffensive"), 
                            ids = NULL,
                            ...) {
  if (query == "{}" & !is.null(ids)) {
    query = query_in("_id", ids)
  }
  res = get_mongo_data("privategroups", query = query, fields = fields, db = "sgrouples", url = url, ...)
  # setnames(res, "_id", "groupId")
  # setkey(res, "groupId")
  # name levels groupType / grouopmodelType
  return(res)
}



get_mongo_users = function(query = "{}",
                           fields = c("_id", "createdAt", "lastName", "firstName", 
                                      "mainContinent", "timezone", "locale",
                                      "public", "publicLinkId", "nsfw", "banned"), 
                           ids = NULL,
                           ...) {
  if (query == "{}" & !is.null(ids)) {
    query = query_in("_id", ids)
  }
  res = get_mongo_data("users", query = query, fields = fields, db = "sgrouples", url = url, ...)
  
  # add if / trycatch
  # setnames(res, "_id", "userPk", skip_absent = T)
  # setkey(res, "userPk")
  return(res)
}

get_mongo_sharedposts = function(query = "{}", 
                                 fields = c("_id", "createdAt", "userPk", "groupRefId",
                                            "postType", "system", "detectedLanguage",
                                            "cachedTotalCommentsCount", 
                                            "text", "hashTags"), 
                                 d1 = NULL, 
                                 d2 = NULL,
                                 ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("sharedposts", query = query, fields = fields, db = "sgrouples", url = url, ...)
  return(res)
}
get_mongo_reportedpost = function(query = "{}", 
                                 fields = NULL, 
                                 d1 = NULL, 
                                 d2 = NULL,
                                 ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("reportedpost", query = query, fields = fields, db = "sgrouples", url = url, ...)
  return(res)
}
 
get_mongo_mpostcomments = function(query = "{}", fields = NULL, d1 = NULL, d2 = NULL, ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("mpostcomments", query = query, fields = fields, db = "sgrouples", url = url, ...)
  return(res)
}
get_mongo_membership = function(query = "{}", fields = NULL, ...) {
  res = get_mongo_data("membership", query = query, fields = fields, db = "sgrouples", url = url_socgraph, ...)
  return(res)
}

get_mongo_entityemojis = function(query = "{}", fields = NULL, d1 = NULL, d2 = NULL, ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("entityemojis",
                       query = query, 
                       fields = fields, 
                       db = "sgrouples-emoji",
                       url = url_emojis, ...)
  return(res)
}

get_mongo_useremojis = function(query = "{}", fields = NULL, d1 = NULL, d2 = NULL, ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2, id = "_id.o")
  }
  res = get_mongo_data("useremojis",
                       query = query, 
                       fields = fields, 
                       db = "sgrouples-emoji",
                       url = url_emojis, ...)
  setnames(res, c("X_id.o", "X_id.u"), c("objectId", "userPk"), skip_absent = T)

  return(res)
}

get_mongo_groupchatmessages = function(query = "{}", fields = NULL, d1 = NULL, d2 = NULL, ...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("groupchatmessages", query = query, fields = fields, db = "sgrouples", url = url_chat, ...)
  return(res)
}

get_mongo_userchatmessages = function(query = "{}", fields = NULL, d1 = NULL, d2 = NULL,...) {
  if (query == "{}" & (!is.null(d1) | !is.null(d2))) {
    query = query_oid_dates(d1, d2)
  }
  res = get_mongo_data("usermessages", query = query, fields = fields, db = "sgrouples", url = url_chat, ...)
  return(res)
}

# scripts
# get_data_from_mongo
# get_data_from_mixpanel
# get_data_from_postgres
# get_data_

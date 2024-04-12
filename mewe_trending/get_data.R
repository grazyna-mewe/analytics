library(mongolite)
library(data.table)
source("analytics/mewe_trending/mongo_most_popular.R")
source("analytics/mewe_trending/get_data_functions.R")
# d1 = Sys.time() - 7*86400
d1 = as.POSIXct("2024-04-01 11:53:53 UTC")

useremojis = get_mongo_useremojis(d1 = d1)
saveRDS(useremojis, "analytics/mewe_trending/data/useremojis.rds")

sharedposts = data.table(get_mongo_sharedposts(d1 = d1,
                                               fields = c("_id", "createdAt", "userPk", 
                                                          "groupRefId", "postType", "system", 
                                                          "cachedTotalCommentsCount", "textEncoded")))
saveRDS(sharedposts, "analytics/mewe_trending/data/sharedposts.rds")


entityemojis = data.table(get_mongo_entityemojis(d1 = d1))
saveRDS(entityemojis, "analytics/mewe_trending/data/entityemojis.rds")

rm(entityemojis)
gc()
Sys.time()

library(mongolite)
library(data.table)
source("analytics/mongo_most_popular.R")
source("analytics/mewe_trending/get_data_functions.R")
sharedposts = readRDS("~/analytics/mewe_trending/data/sharedposts.rds")
entityemojis = readRDS("~/analytics/mewe_trending/data/entityemojis.rds")

user_ids = unique(sharedposts$userPk)
group_ids = unique(sharedposts$groupRefId)

users = data.table(get_mongo_users(ids = user_ids, 
                                   fields = c("_id", "createdAt", "lastName", "firstName", 
                                              "mainContinent", "timezone", "locale",
                                              "public", "publicLinkId", "nsfw", "banned")))
groups = data.table(get_mongo_groups(ids = group_ids[!is.na(group_ids)],
                                     fields = c("_id", "createdAt", "name", "description",
                                                "groupModelType", "groupThematicType", "membersCount", 
                                                "userPk", "locale", "isBannedAsOffensive")))

# followers numbers

mongo_contactsfeedsettings = mongo("contactsfeedsettingss", db = "sgrouples", url = url)
followers = mongo_contactsfeedsettings$find(query_in("userPk", user_ids),
                                            fields = get_fields_query(c("userPk", "counters.following", "counters.followers")))
followers$`_id` = NULL
followers = data.table(followers) %>%
  setnames(c("counters.followers", "counters.following"), c("followers", "following"))

sharedposts_all = sharedposts |>
  merge(users, all.x = T, by.x = "userPk", by.y = "_id", suffixes = c("", "_user")) |>
  merge(groups, all.x = T, by.x = "groupRefId", by.y = "_id", suffixes = c("", "_group")) |>
  merge(entityemojis, all.x = T, by = "_id") |>
  merge(followers, all.x = T, by = "userPk")
saveRDS(sharedposts_all, "analytics/mewe_trending/data/sharedposts_all.rds")

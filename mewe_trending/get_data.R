library(mongolite)
library(data.table)
source("scripts/mongo_most_popular.R")
source("mewe_trending/get_data_functions.R")
# d1 = Sys.time() - 7*86400
d1 = as.POSIXct("2024-03-29 09:51:58 UTC")

useremojis = get_mongo_useremojis(d1 = d1)
saveRDS(useremojis, "mewe_trending/data/useremojis.rds")

sharedposts = data.table(get_mongo_sharedposts(d1 = d1,
                                               fields = c("_id", "createdAt", "userPk", 
                                                          "groupRefId", "postType", "system", 
                                                          "cachedTotalCommentsCount", "textEncoded")))
saveRDS(sharedposts, "mewe_trending/data/sharedposts.rds")


entityemojis = data.table(get_mongo_entityemojis(d1 = d1))
saveRDS(entityemojis, "mewe_trending/data/entityemojis.rds")

rm(entityemojis)
gc()
Sys.time()

library(mongolite)
library(data.table)
source("scripts/mongo_most_popular.R")
source("mewe_trending/get_data_functions.R")
sharedposts = readRDS("~/mewe_trending/data/sharedposts.rds")
entityemojis = readRDS("~/mewe_trending/data/entityemojis.rds")

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

sharedposts_all = sharedposts |>
  merge(users, all.x = T, by.x = "userPk", by.y = "_id", suffixes = c("", "_user")) |>
  merge(groups, all.x = T, by.x = "groupRefId", by.y = "_id", suffixes = c("", "_group")) |>
  merge(entityemojis, all.x = T, by = "_id")
saveRDS(sharedposts_all, "mewe_trending/data/sharedposts_all.rds")

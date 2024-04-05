library(mongolite)
library(data.table)
library(dplyr)
library(tidytext)
library(stringr)
source("scripts/mongo_most_popular.R")
source("mewe_trending//get_data_functions.R")
source("mewe_trending//get_data_store.R")

countries_from_token <- readRDS("~/data/countries_from_token.rds")
sharedposts_all <- readRDS("~/mewe_trending/data/sharedposts_all.rds")
useremojis <- readRDS("~/mewe_trending/data/useremojis.rds")


# popularity scores -------------------------------------------------------

# top20_locations = countries_from_token[, .N, by =. (location)][order(-N)][1:20, location]
top20_locations =  c("United States", "Malaysia", "India", "Hong Kong", "United Kingdom", 
                     "Australia", "Canada", "Germany", "South Korea", "Japan", "Italy", 
                     "France", "Mexico", "Indonesia", "Brazil", "Saudi Arabia", "Sweden", 
                     "Spain", "The Netherlands", "Belgium")
countries_from_token[!location %in% top20_locations, location := "Other"]

useremojis = data.table(useremojis)
setnames(useremojis, c("X_id.o", "X_id.u"), c("objectId", "userPk"))
useremojis[countries_from_token, on = "userPk", location := i.location]
# ids_premium = get_premium_users()
# useremojis[, premium := userPk %in% ids_premium]

popularity_scores = useremojis[, .(n = uniqueN(userPk)), by = .(location, objectId)]
setkey(popularity_scores, "objectId")

popularity_scores = popularity_scores[order(location, -n)]
popularity_scores = popularity_scores[objectId %in% sharedposts_all$`_id`]
popularity_scores = popularity_scores[!is.na(location)]
feather::write_feather(popularity_scores, "~/mewe_trending/data/popularity_scores.feather")

# popularity_scores[sharedposts_all, userPk := i.userPk, on = "objectId"]
# popularity_scores[sharedposts_all, groupRefId := i.groupRefId, on = "objectId"]
# feather::write_feather(popularity_scores, path = 
#                          "mewe_trending/data/popularity_scores2.feather")

# keywords ----------------------------------------------------------------
sharedposts_texts = tibble(sharedposts_all[, .(`_id`, groupRefId, userPk, textEncoded)])
sharedposts_texts = sharedposts_texts %>%
  unnest_tokens(output = word, input = textEncoded)
stop_words = rbindlist(
  lapply(stopwords::stopwords_getlanguages(source = "stopwords-iso"),
         function(x) data.table(word = stopwords::stopwords(language = x, source = "stopwords-iso"),
                                language = x)
  ))

sharedposts_texts = sharedposts_texts %>%
  anti_join(stop_words)

frequency_by_group <- sharedposts_texts %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(groupRefId, word) %>%
  group_by(groupRefId) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n)
frequency_by_group = as.data.table(frequency_by_group)[order(groupRefId, -proportion)]
keywords_groups = frequency_by_group[, .(keywords = paste(word[!is.na(word)][1:min(10, .N)], collapse = ", ")), 
                                     by = .(groupRefId)]

frequency_by_user <- sharedposts_texts %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(userPk, word) %>%
  group_by(userPk) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n)
frequency_by_user = as.data.table(frequency_by_user)[order(userPk, -proportion)]
keywords_users = frequency_by_user[, .(keywords = paste(word[!is.na(word)][1:min(10, .N)], collapse = ", ")),
                                   by = .(userPk)]

# # frequency by location
# frequency_by_location <- sharedposts_texts %>% 
#   left_join(countries_from_token) %>%
#   mutate(word = str_extract(word, "[a-z']+")) %>%
#   count(location, word) %>%
#   group_by(location) %>%
#   mutate(proportion = n / sum(n)) %>% 
#   select(-n)
# frequency_by_location = as.data.table(frequency_by_location)[order(location, -proportion)]
# frequency_by_location = frequency_by_location[!is.na(word)][!word %in% c("https", "www", "http")][!nchar(word) == 1]
# keywords_location = frequency_by_location[, .(keywords = paste(word[!is.na(word)][1:min(30, .N)], collapse = ", ")), by = .(location)]



# emojis ------------------------------------------------------------------
sharedposts_all[, emojis := unlist(lapply(sharedposts_all[, e], function(x) paste(x, collapse = "")))]

emoji_by_user = sharedposts_all[order(-u), .(e = unlist(e)), by = userPk][
  , .N, by = .(userPk, e)][order(-N)][
    , .(emojis = paste(setdiff(e[1:5], NA), collapse = ",")), by = .(userPk)]
# userPk          emojis
# 1: 63e2fa08e66f5333cb729e4b   ❤,👍,💋,🤩,🔥
# 2: 5c560817fe294b61f8b9e629   ✡,💣,😡,🇵🇸,🐀
emoji_by_group = sharedposts_all[!is.na(groupRefId), .(e = unlist(e)), by = groupRefId][
  , .N, by = .(groupRefId, e)][order(-N)][
    , .(emojis = paste(setdiff(e[1:5], NA), collapse = ",")), by = .(groupRefId)]


# results ------------------------------------------------------------


posts_res = sharedposts_all[order(-u), .(objectId = `_id`,
                                         userPk,
                                         createdAt,
                                         publicLinkId,
                                         firstName,
                                         lastName,
                                         mainContinent,
                                         locale,
                                         timezone,
                                         groupRefId,
                                         groupModelType,
                                         groupThematicType,
                                         name,
                                         description,
                                         postType, 
                                         system,
                                         emojis,
                                         text = textEncoded,
                                         user_public = public, 
                                         user_nsfw = nsfw, 
                                         group_banned = isBannedAsOffensive,
                                         user_emojied = u)]
setkey(posts_res, "objectId")
# sharedposts_texts <- readRDS("~/mewe_trending/data/sharedposts_texts.rds")
# posts_res[sharedposts_texts, text := textEncoded, on = c(objectId = "_id")]  

# Add emojis, keywords, location
posts_res[emoji_by_group[!is.na(groupRefId)], on = "groupRefId", group_receiving_emojis := i.emojis]
posts_res[keywords_groups[!is.na(groupRefId)], on = "groupRefId", group_keywords := i.keywords]
posts_res[emoji_by_user, on = "userPk", user_receiving_emojis := i.emojis]
posts_res[keywords_users, on = "userPk", user_keywords := i.keywords]
posts_res[countries_from_token, on = "userPk", author_location := i.location]
posts_res[is.na(author_location), author_location := "Unknown"]
posts_res = posts_res[order(-user_emojied)]

feather::write_feather(posts_res, "mewe_trending/data/posts_res.feather")
feather::write_feather(posts_res[, c("objectId", "userPk", "createdAt", "mainContinent", "locale", 
                                     "timezone", "groupRefId", "groupModelType", "groupThematicType", 
                                     "postType", "system", "user_public", "user_nsfw", "group_banned", 
                                     "user_emojied", "author_location"), with = F],
                       "mewe_trending/data/posts_res_summary.feather")
# Users and groups
users_res = posts_res[, 
                      .(total_user_emojied = sum(user_emojied, na.rm = T),
                        total_user_emojied_per_post = round(sum(user_emojied, na.rm = T) / uniqueN(objectId), 1),
                        total_nr_posts = uniqueN(objectId),
                        total_nr_group_posts = uniqueN(objectId[system == "group"])),
                      by = .(userPk, user_public, user_nsfw, 
                             firstName, lastName, user_keywords, user_receiving_emojis,
                             publicLinkId, mainContinent, locale, timezone)][order(-total_user_emojied)]

groups_res = posts_res[!is.na(groupRefId),
                       .(total_user_emojied = sum(user_emojied, na.rm = T),
                         total_nr_posts = uniqueN(objectId), 
                         group_keywords = group_keywords[1]),
                       by = .(groupRefId, name, groupModelType, groupThematicType,
                              group_banned)][order(-total_user_emojied)]

feather::write_feather(groups_res, "mewe_trending/data/groups_res.feather")
feather::write_feather(users_res, "mewe_trending/data/users_res.feather")


# Select 200 most popular posts by each filter
select_posts = popularity_scores[posts_res[, .(gr = is.na(groupRefId), group_banned, user_nsfw, objectId)], on = "objectId"][
  order(-n)][, .SD[1:200], by=.(location, gr, group_banned, user_nsfw)][!is.na(location)]
posts_res_select = posts_res[objectId %in% select_posts$objectId]

feather::write_feather(posts_res_select, "mewe_trending/data/posts_res_select.feather")


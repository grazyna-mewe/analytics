library(mongolite)
library(data.table)
source("scripts/mongo_most_popular.R")
source("scripts/popup_segments_functions.R")


# include 
users = mongo(db = "sgrouples", collection = "users", url = url)
users_mau = data.table(users$find('{"lastLogin": {"$gte": {"$date" : "2024-03-16T08:00:00Z"}}}',
                       fields = '{"_id":1}'))

# exclude
refreshtokens = mongo(db = "sgrouples", collection = "refreshtokens", url = url)
exclude_locations = c("Canada", "Cuba", "Iran", "North Korea", "Russia", "Syria",  "Ukraine")
# query_in(location.countryName, exclude_locations
location = data.table(refreshtokens$find(fields = '{"userPk":1, "location.countryName":1, "_id":0}'))

segment = users_mau[!`_id` %in% location[location %in% exclude_locations, userPk]]

segment_name = "segment_wefunder_v1"
fwrite(segment[, .(`_id`)], paste0("data//", segment_name, ".csv"))

file.create(paste0("data//", segment_name, ".txt"))
cat(paste(segment$`_id`, collapse = ","),
    file = paste0("data//", segment_name, ".txt"), append = FALSE)

library(mongolite)

segment = mongo_membership$find(query_in("groupId", "6415583f5469074e22355a99"), fields = get_fields_query(c("userPk", "userName")))
segment_name = "mewe_employers"


exclude = c("59af0c6bbd211d2b9f95c7c6",
"642f3f2c7433f629fffe561e",
"6389171e6c1e7a3f867e465d",
"63dacacedbbab570b84acbe0",
"5d8c0233cbd95104a5a13788",
"638a75493d885153d5028cf6",
"62bb2beef3eb1f0e622c5208",
"5cd60de0fb445a2326310518",
"4f4cb34f0cf2617e838b6eb1")

segment_ids = setdiff(segment$userPk, exclude)
file.create(paste0("/home/rstudio/data//", segment_name, ".txt"))
cat(paste(segment_ids, collapse = ","),
    file = paste0("/home/rstudio/data//", segment_name, ".txt"), append = FALSE)

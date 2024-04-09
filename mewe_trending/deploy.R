
files_to_copy = c("data/popularity_scores.feather", 
                  "data/posts_res_select.feather",
                  "data/posts_res_summary.feather", 
                  "data/users_res.feather", 
                  "data/groups_res.feather", 
                  "server.R",
                  "ui.R",
                  "data/column_names.rda")

for(file in files_to_copy) {
  ff = paste0("/home/rstudio/analytics/mewe_trending/", file)
  stopifnot(file.exists(ff))
  file.copy(from = ff,
            to = paste0("/home/rstudio/shiny-server/mewe_trending/", file), overwrite = TRUE)
}

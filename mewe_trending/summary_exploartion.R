# summary

# posts, nr of posts, users, groups
# groups / users / pages (barplot)
# postType (barplot)
# keywords (chmura)
who

# users

# pages

# groups - tutaj na pewno trzeba dodać rzeczy (measures)

# changes
# change from last week

posts_res <- feather::read_feather("data/posts_res_summary.feather")
posts_res = data.table(posts_res_summary)
posts_res[, group_banned_or_user_nsfw := (group_banned & !is.na(group_banned)) | 
            (user_nsfw & !is.na(user_nsfw))]
# nsfw
dat_nsfw1 = posts_res[, .(n_posts = .N,
                          n_emojis = sum(user_emojied, na.rm = T),  
                          n_emojis_per_post = round(sum(user_emojied, na.rm = T) / .N, 2),
                          median_n_emojis = median(as.numeric(user_emojied), na.rm = T),
                          n_users = uniqueN(userPk),
                          n_emojis_per_user = round(sum(user_emojied, na.rm = T) / uniqueN(userPk), 2),
                          n_posts_per_user = round(.N / uniqueN(userPk), 2)), 
                      by = .(group_banned = (group_banned & !is.na(group_banned)), 
                             user_nsfw = (user_nsfw & !is.na(user_nsfw)))]

dat_nsfw2 = posts_res[, .(n_users = uniqueN(userPk),
                          n_emojis = sum(user_emojied, na.rm = T),
                          median_n_emojis = median(as.numeric(user_emojied), na.rm = T),
                          
                          n_emojis_per_post = sum(user_emojied, na.rm = T) / .N,
                          n_emojis_per_user = sum(user_emojied, na.rm = T) / uniqueN(userPk),
                          n_posts_per_user = .N / uniqueN(userPk),
                          n_posts = .N), 
                      by = .(group_banned_or_user_nsfw)]
# n_users n_emojis n_emojis_per_post n_users_per_post n_posts
ggplot(melt(dat_nsfw2, id.vars = c("group_banned_or_user_nsfw"))[
  variable %in% c("n_posts_per_user", "n_emojis_per_user", "n_emojis_per_post")]) +
  aes(x = group_banned_or_user_nsfw,
      y = value,
      fill = variable, 
      group = variable) +
  geom_bar(position = "dodge", stat="identity")

ggplot(melt(dat_nsfw1, id.vars = c("user_nsfw", "group_banned"))) +
  aes(x = user_nsfw + group_banned, y = value, fill = variable, group = variable) +
  geom_bar(position = "dodge", stat="identity")


# nsfw vs non nsfw

# top emojis
# top keywords
# top group cathegories

# postType / system
posts_res[group_banned_or_user_nsfw == F, .(n_posts = .N,
                                         n_users = uniqueN(userPk),
                                         n_emojis = sum(user_emojied, na.rm = T), 
                                         median_n_emojis = median(as.numeric(user_emojied), na.rm = T),
                                         n_emojis_per_post = sum(user_emojied, na.rm = T) / .N,
                                         n_emojis_per_user = sum(user_emojied, na.rm = T) / uniqueN(userPk),
                                         n_posts_per_user = .N / uniqueN(userPk)), 
                      by = .(system, group = !is.na(groupRefId))][order(-n_users)]

res = posts_res[group_banned_or_user_nsfw == T, .(n_posts = .N,
                                         n_users = uniqueN(userPk),
                                         n_emojis = sum(user_emojied, na.rm = T), 
                                         median_n_emojis = median(as.numeric(user_emojied), na.rm = T),
                                         n_emojis_per_post = sum(user_emojied, na.rm = T) / .N,
                                         n_emojis_per_user = sum(user_emojied, na.rm = T) / uniqueN(userPk),
                                         n_posts_per_user = .N / uniqueN(userPk)), 
          by = .(group = !is.na(groupRefId))][order(-n_users)]

mres = melt(res, id.vars = c("group"))
plot_ly(mres[variable %in% c("n_posts", "n_emojis")],
        x = ~group,
        y = ~value,
        color = ~variable) %>%
  add_bars()
# groups

# nowe wskaźniki
# liczba postów
# liczba nowych członków
# scograph
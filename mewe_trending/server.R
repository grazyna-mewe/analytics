library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(data.table)
library(ggplot2)

options(shiny.sanitize.errors = FALSE)
load("data/column_names.rda")

posts_res = feather::read_feather("data/posts_res_select.feather")
posts_res = data.table(posts_res)
groups_res = feather::read_feather("data/groups_res.feather")
groups_res = data.table(groups_res)
users_res = feather::read_feather("data/users_res.feather")
users_res = data.table(users_res)
popularity_scores = feather::read_feather("data/popularity_scores.feather")
popularity_scores = data.table(popularity_scores)
posts_res_summary = feather::read_feather("data/posts_res_summary.feather")
posts_res_summary = data.table(posts_res_summary)

d1 = as.POSIXct("2024-04-01 11:53:53 UTC")
# max(posts_res_summary$createdAt)
cat("data loaded")


# Define server logic required to draw a histogram
function(input, output, session) {
  
  get_popularity_scores = reactive({
    # if(input$premium == TRUE)
    #   popularity_scores = popularity_scores[premium == TRUE]
    if(!"All" %in% input$location)
      popularity_scores = popularity_scores[location %in% input$location]
    
    return(popularity_scores[, 
                             .(chosen_audience_emojied = sum(n)), 
                             by = .(objectId)])
  })
  filter_posts = function(posts_res) {
    # posts filters
    if(input$nsfw_user == F) {
      posts_res = posts_res[user_nsfw == F]
    }
    if(input$group_banned == F) {
      posts_res = posts_res[group_banned == F | is.na(group_banned)]
    }
    if(input$group_posts == F) {
      posts_res = posts_res[system != "group"]
    }
    if(input$non_group_posts == F) {
      posts_res = posts_res[system == "group"]
    }
    if(input$non_group_posts != 7) {
      posts_res = posts_res[difftime(d1, createdAt, units = "days") < input$last_days]
    }
    return(posts_res)
  }
  get_posts_table_raw = function(posts_res) {
    if(!"All" %in% input$location) {
      posts_res = posts_res[get_popularity_scores(), on = "objectId"][
        order(-chosen_audience_emojied)]
      
    }
    posts_res = filter_posts(posts_res)
    return(posts_res)
  }
  
  get_posts_table = reactive({
    return(get_posts_table_raw(posts_res) %>%
             dplyr::select(any_of(c(input$posts_cols, "chosen_audience_emojied")))  #%>%
             # setnames(post_names, names(post_names), skip_absent = T) %>%
             # setnames("chosen_audience_emojied", "Nr of chosen audience users emojied", skip_absent = T)
           )
  })
  
  get_users_table = reactive({
    res = get_posts_table_raw(posts_res_summary)
    if("chosen_audience_emojied" %in% names(res)) {
      res = res[, .(chosen_audience_emojied = sum(chosen_audience_emojied, na.rm = T),
                    user_emojied = sum(user_emojied, na.rm = T),
                    user_emojied_per_post = round(sum(user_emojied, na.rm = T) / uniqueN(objectId), 1),
                    nr_posts = uniqueN(objectId),
                    nr_group_posts = uniqueN(objectId[system == "group"])),
                by = .(userPk, author_location)]
      col_order = "chosen_audience_emojied"
    } else {
      res = res[, .(user_emojied = sum(user_emojied, na.rm = T),
                    user_emojied_per_post = round(sum(user_emojied, na.rm = T) / uniqueN(objectId), 1),
                    nr_posts = uniqueN(objectId),
                    nr_group_posts = uniqueN(objectId[system == "group"])),
                by = .(userPk, author_location)]
      col_order = "user_emojied"
    }
    
    users_res = merge(users_res, res, by = "userPk")  %>%
      setorderv(col_order, -1) %>%
      dplyr::select(any_of(c(input$users_cols, "chosen_audience_emojied"))) %>%
      setnames(user_names, names(user_names), skip_absent = T) %>%
      setnames("chosen_audience_emojied", "Nr of chosen audience users emojied", skip_absent = T)
    return(users_res)
  })
  get_groups_table = reactive({
    res = get_posts_table_raw(posts_res_summary)
    if("chosen_audience_emojied" %in% names(res)) {
      res = res[!is.na(groupRefId), .(chosen_audience_emojied = sum(chosen_audience_emojied, na.rm = T),
                                      user_emojied = sum(user_emojied, na.rm = T),
                                      nr_posts = uniqueN(objectId), 
                                      user_emojied_per_post = round(sum(user_emojied, na.rm = T) / uniqueN(objectId), 1)),
                by = .(groupRefId)]
      col_order = "chosen_audience_emojied"
      
    } else {
      res = res[!is.na(groupRefId), .(user_emojied = sum(user_emojied, na.rm = T),
                                      nr_posts = uniqueN(objectId), 
                                      user_emojied_per_post = round(sum(user_emojied, na.rm = T) / uniqueN(objectId), 1)),
                by = .(groupRefId)]
      col_order = "user_emojied"
    }
    
    res = merge(groups_res, res, by = "groupRefId") %>%
      dplyr::select(any_of(c(input$groups_cols, "chosen_audience_emojied"))) %>%
      setorderv(col_order, -1)   %>%
      setnames(group_names, names(group_names), skip_absent = T) %>%
      setnames("chosen_audience_emojied", "Nr of chosen audience users emojied", skip_absent = T)
    
    return(res)
  })
  
  get_summary_table = reactive({
    res = get_posts_table_raw(posts_res_summary)
    if("chosen_audience_emojied" %in% names(res)) {
      res = res[!is.na(chosen_audience_emojied)][chosen_audience_emojied != 0]
      res[, user_emojied := chosen_audience_emojied]
    }
    
    res = res %>%
      group_by(across(input$summarize_by)) %>%
      summarise(n_posts = n(),
                n_emojis = sum(user_emojied, na.rm = T),
                n_emojis_per_post = round(sum(user_emojied, na.rm = T) / n(), 2),
                median_n_emojis = median(as.numeric(user_emojied), na.rm = T),
                n_authors = uniqueN(userPk),
                n_emojis_per_author = round(sum(user_emojied, na.rm = T) / uniqueN(userPk), 2),
                n_posts_per_author = round(n() / uniqueN(userPk), 2)) %>%
      setorderv("n_emojis", -1)
    
    return(res)
  })
  
  output$summary_plot = renderPlot({
    dat = get_summary_table() %>% 
      as.data.table %>%
      melt(id.vars = input$summarize_by) %>%
      setnames(input$summarize_by, "x")
    
    ggplot(dat[variable %in% input$plot_variables]) +
      aes(x = x,
          y = value,
          fill = variable,
          group = variable) +
      geom_bar(position = "dodge", stat="identity") +
      xlab(input$summarize_by)
  }, height = 400, width = 600)
  output$summary_table = DT::renderDataTable((get_summary_table()),
                                             options = list(
                                               dom = 't'))
  output$user_nrow = renderText(nrow(get_users_table()))
  output$groups_nrow = renderText(nrow(get_groups_table()))
  output$users_table = DT::renderDataTable(
    get_users_table(),
    extensions = c("Scroller", "Buttons"),
    filter = "bottom",
    options = list(
      dom = 'lrtipB', #"<lf<\"datatables-scroll\"t>ipr>"
      buttons = list(
        list(extend = "csv", text = "Download Current Page", filename = "page",
             exportOptions = list(
               modifier = list(page = "current")
             )
        ),
        list(extend = "csv", text = "Download Full Results", filename = "data",
             exportOptions = list(
               modifier = list(page = "all")
             )
        )),
      columnDefs = list(list(targets = 1,
                             render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 15 ?","'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;","}")
      ))
      # scrollY = TRUE,
      # scroller = TRUE,
      # scrollX = TRUE
    )
  )
  output$groups_table = renderDataTable(
    get_groups_table(),
    extensions = c("Scroller", "Buttons"),
    filter = "top",
    options = list(
      dom = 'lfrtipB', #"<lf<\"datatables-scroll\"t>ipr>"
      buttons = list(
        list(extend = "csv", text = "Download Current Page", filename = "page",
             exportOptions = list(
               modifier = list(page = "current")
             )
        ),
        list(extend = "csv", text = "Download Full Results", filename = "data",
             exportOptions = list(
               modifier = list(page = "all")
             )
        )),
      columnDefs = list(list(targets = 1,
                             render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 15 ?","'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;","}")
      ))
      # scrollY = TRUE,
      # scroller = TRUE,
      # scrollX = TRUE
    )
  )
  output$posts_table = renderDataTable(
    get_posts_table(),
    filter = "top",
    extensions = c("Scroller", "Buttons"),
    options = list(
      dom = 'lfrtipB', #"<lf<\"datatables-scroll\"t>ipr>"
      buttons = list(
        list(extend = "csv", text = "Download Current Page", filename = "page",
             exportOptions = list(
               modifier = list(page = "current")
             )
        ),
        list(extend = "csv", text = "Download Full Results", filename = "data",
             exportOptions = list(
               modifier = list(page = "all")
             )
        )),
      columnDefs = list(list(targets = 1,
                             render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 15 ?","'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;","}")
      ))
      # ,
      # scrollY = TRUE,
      # scroller = TRUE,
      # scrollX = TRUE
    )
  )
  
}

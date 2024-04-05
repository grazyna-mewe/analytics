#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(data.table)

# dane
# popularity_scores
# posts_res

# # TODO change to fread
# users_res <- readRDS("~/mewe_trending/data/users_res.rds")
# cat("data loaded")
# 
# groups_res <- readRDS("~/mewe_trending/data/groups_res.rds")
# cat("data loaded")
# 
posts_res <- readRDS("~/mewe_trending/data/posts_res.rds")
popularity_scores <- feather::read_feather("~/mewe_trending/data/popularity_scores.feather")
# popularity_scores = data.table(popularity_scores)
cat("data loaded")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  get_popularity_scores <- reactive({
    if(input$premium == TRUE)
      popularity_scores = popularity_scores[premium == TRUE]
    if(!"All" %in% input$location)
      popularity_scores = popularity_scores[location %in% input$location]
    
    return(popularity_scores[, 
                             .(n = sum(n)), 
                             by = .(objectId)])
  })

  get_posts_table_raw = reactive({
    if(!"All" %in% input$location || input$premium == TRUE) {
      posts_res = posts_res[get_popularity_scores()][order(-n)]
      cat("posts_table_raw")
      
    }
    
    # posts filters
    if(input$nsfw_user == F) {
      posts_res = posts_res[user_nsfw == F]
    }
    if(input$group_banned == F) {
      posts_res = posts_res[group_banned == F]
    }
    if(input$group_posts == F) {
      posts_res = posts_res[system != "group"]
    }
    if(input$non_group_posts == F) {
      posts_res = posts_res[system == "group"]
    }
    
    posts_res$text = NULL
    return(posts_res)
  })
  
  get_posts_table = reactive({
    return(get_posts_table_raw() %>%
             dplyr::select(any_of(input$posts_cols)))
  })
  
  get_users_table = reactive({
    res = get_posts_table_raw()
    res = res[, .(user_emojied = sum(user_emojied, na.rm = T),
                  nr_posts = uniqueN(objectId),
                  nr_group_posts = uniqueN(objectId[system == "group"])),
              by = .(userPk, user_public, user_nsfw, 
                     firstName, lastName, user_keywords, user_receiving_emojis,
                     publicLinkId, mainContinent, locale, timezone)][order(-user_emojied)] %>%
      dplyr::select(any_of(input$users_cols))
    return(res)
  })
  get_groups_table = reactive({
    res = get_posts_table_raw()
    res = res[!is.na(groupRefId), .(user_emojied = sum(user_emojied, na.rm = T),
                              nr_posts = uniqueN(objectId), 
                              group_keywords[1]),
        by = .(groupRefId, name, groupModelType, groupThematicType,
               group_banned, group_receiving_emojis, description)][order(-user_emojied)] %>%
      dplyr::select(any_of(input$groups_cols))
    return(res)
  })
  

  output$users_table <- renderDataTable(
    DT::datatable(data = get_users_table(),
                  extensions = c("Scroller", "Buttons"),
                  options = list(
                    dom = 'frtipB', #"<lf<\"datatables-scroll\"t>ipr>"
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
                                           render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}")
                    )),
                    scrollY = TRUE, 
                    scroller = TRUE,
                    scrollX = TRUE)
    ))
  output$groups_table <- renderDataTable(get_groups_table(),
                                         extensions = c("Scroller", "Buttons"),
                                         options = list(
                                           dom = 'frtipB', #"<lf<\"datatables-scroll\"t>ipr>"
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
                                                                  render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}")
                                           )),
                                           scrollY = TRUE, 
                                           scroller = TRUE,
                                           scrollX = TRUE))
  output$posts_table <- renderDataTable(get_posts_table(),
                                        extensions = c("Scroller", "Buttons"),
                                        options = list(
                                          dom = 'frtipB', #"<lf<\"datatables-scroll\"t>ipr>"
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
                                                                 render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;","}")
                                                                 )),
                                          scrollY = TRUE, 
                                          scroller = TRUE,
                                          scrollX = TRUE)
  )
  
}

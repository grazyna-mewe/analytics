library(bslib)

load("data/column_names.rda")

ui <- page_sidebar(

  # App title ----
  title = "Trending on mewe",

  # Sidebar panel for inputs ----
  sidebar = sidebar(

    h3("Post filters"),
    # selectInput("system", "System",
    #             multiple = T,
    #             choices = c("group", "contacts", "event", "page"),
    #             selected = c("group", "contacts", "event", "page")),
    sliderInput("last_days", "How many days ago", min = 1, max = 7, value = 7, step = 1),
    checkboxInput("group_posts",
                  "Group posts",
                  value = c(T)),
    checkboxInput("non_group_posts",
                  "Non group posts",
                  value = c(T)),
    checkboxInput("nsfw_user",
                  "Posted by NSFW user",
                  value = c(T)),
    checkboxInput("group_banned",
                  "Posted in banned group",
                  value = c(T)),
    # selectInput("author_location", label = "Author country", #label = h3("Select box"),
    #             choices = c("All", "United States", "Malaysia", "India", "Hong Kong", "United Kingdom",
    #                         "Australia", "Canada", "Germany", "South Korea", "Japan", "Italy",
    #                         "France", "Mexico", "Indonesia", "Brazil", "Saudi Arabia", "Sweden",
    #                         "Spain", "The Netherlands", "Belgium"),
    #             multiple = T,
    #             selected = "All"),

    h3("Audience filters"),

    # Input: Select the random distribution type ----
    # checkboxGroupInput("client_type", "Client type",
    #              c("ios", "web", "android")),
    # # br() element to introduce extra vertical spacing ----
    # br(),
    # Input: Slider for the number of observations to generate ----
    selectInput("location", label = "Country", #label = h3("Select box"),
                choices = c("All", "United States", "Malaysia", "India", "Hong Kong", "United Kingdom",
                               "Australia", "Canada", "Germany", "South Korea", "Japan", "Italy",
                               "France", "Mexico", "Indonesia", "Brazil", "Saudi Arabia", "Sweden",
                               "Spain", "The Netherlands", "Belgium"),
                multiple = T,
                selected = "All")
    # checkboxInput("premium",
    #               "Only premium members",
    #               value = c(F))
  ),


  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_tab(
    title = "",
    
    nav_panel("Users",
              selectInput("users_cols", "Show columns",
                          choices = user_names,
                          width = "55%",
                          
                          selected = c("publicLinkId", "user_public", "user_nsfw", "author_location", "followers",
                                       "user_keywords", "user_receiving_emojis",
                                       "total_user_emojied", "total_nr_posts"),
                          multiple = TRUE),
              div(DT::dataTableOutput("users_table"))),

    nav_panel("Groups",
              selectInput("groups_cols", "Show columns",
                          width = "55%",
                          choices = group_names,
                          selected = c("name", "author_location", "groupThematicType", "membersCount",
                                       "group_banned",  "group_keywords", "nr_posts",
                                       "user_emojied", "user_emojied_per_post"),
                          multiple = TRUE),
              div(DT::dataTableOutput("groups_table"))),

    nav_panel("Posts",
              selectInput("posts_cols", "Show columns",
                          choices = post_names,
                          width = "55%",
                          
                          selected = c("publicLinkId",
                                       "postType",
                                       "system",
                                       "author_location",
                                       "keywords",
                                       "emojis",
                                       "user_public",
                                       "user_emojied"),
                          multiple = TRUE),
              div(DT::dataTableOutput("posts_table"))),

    # Panel with summary ----
    nav_panel("Summary",
              selectInput("summarize_by", "Summarize by",
                          c("user_nsfw", "group_banned", "author_location", "system", "groupThematicType", "groupModelType",
                            "mainContinent", "locale", "timezone", "user_public"),
                          selected = c("system"),
                          multiple = TRUE),
              div(dataTableOutput("summary_table")),
              selectInput("variable", "Show variable",
                          c("n_users", "n_emojis", "median_n_emojis", "n_emojis_per_post",
                            "n_emojis_per_user", "n_posts_per_user", "n_posts"),
                          selected = c("n_posts", "n_authors"),
                          multiple = TRUE),
              plotOutput("summary_plot")
              )
  ) 
)
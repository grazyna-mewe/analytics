library(bslib)

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
    
    h3("Audience filters"),
    
    # Input: Select the random distribution type ----
    # checkboxGroupInput("client_type", "Client type",
    #              c("ios", "web", "android")),
    # # br() element to introduce extra vertical spacing ----
    # br(),
    # Input: Slider for the number of observations to generate ----
    selectInput("location", label = "Region", #label = h3("Select box"), 
                choices = c("All", "United States", "Malaysia", "India", "Hong Kong", "United Kingdom", 
                               "Australia", "Canada", "Germany", "South Korea", "Japan", "Italy", 
                               "France", "Mexico", "Indonesia", "Brazil", "Saudi Arabia", "Sweden", 
                               "Spain", "The Netherlands", "Belgium"), 
                multiple = T,
                selected = "All"),
    checkboxInput("premium",
                  "Only premium members",
                  value = c(F))
  ),

  
  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_tab(
    title = "",
    # # Panel with plot ----
    # nav_panel("Plot", plotOutput("plot")),
 
    # Panel with table ----
    nav_panel("Posts",
              selectInput("posts_cols", "Show columns",
                          c("objectId", "userPk", "publicLinkId", "firstName", "lastName", 
                            "mainContinent", "locale", "timezone", "groupRefId", "groupModelType", 
                            "groupThematicType", "name", "description", "postType", "system", 
                            "emojis", "user_public", "user_nsfw", "group_banned", "user_emojied", 
                            "text", "group receivig emojis" = "group_receiving_emojis", "group_keywords", "user_receiving_emojis", 
                            "user_keywords"),
                          selected = c("publicLinkId",
                                       "postType", 
                                       "system",
                                       "keywords",
                                       "emojis",
                                       "user_public", 
                                       "user_emojied"), 
                          multiple = TRUE),
              div(dataTableOutput("posts_table"))),
    
    nav_panel("Users", 
              selectInput("users_cols", "Show columns",
                          c("userPk", "user_public", "user_nsfw", "firstName", "lastName", 
                            "publicLinkId", "mainContinent", "locale", "timezone", "user_emojied", 
                            "user_keywords", "user_receiving_emojis",
                            "nr_posts", "nr_group_posts"),
                          selected = c("publicLinkId", "user_keywords", "user_receiving_emojis",
                                       "user_public", "user_nsfw", "nr_posts", "nr_group_posts",
                                       "user_emojied"), 
                          multiple = TRUE),
              div(dataTableOutput("users_table"))),
    
    nav_panel("Group", 
              selectInput("groups_cols", "Show columns",
                          c("groupRefId", "name", "groupModelType", "groupThematicType",
                            "group_banned"),
                          selected = c("name", "groupModelType", "groupThematicType",
                                       "group_banned", "nr_posts",
                                       "user_emojied"), 
                          multiple = TRUE),
              div(dataTableOutput("groups_table"))),
    
    # Panel with summary ----
    nav_panel("Summary", 
              selectInput("groups_cols", "Show columns",
                          c("user_nsfw", "group_banned", "system", "groupThematicType", "groupModelType",
                            "mainContinent", "locale", "timezone", "user_public"),
                          selected = c("system"), 
                          multiple = TRUE),
              div(dataTableOutput("summary_table")))
  )
  # ,
  # 
  # navset_card_tab(
  #   title = "Users",
  #   # # Panel with plot ----
  #   # nav_panel("Plot", plotOutput("plot")),
  #   # Panel with table ----
  #   nav_panel("Users table", dataTableOutput("users_table")),
  #   # Panel with summary ----
  #   nav_panel("Users summary", verbatimTextOutput("summary")),
  # )
  # 
  # navset_card_tab(
  #   title = "Groups",
  #   # # Panel with plot ----
  #   # nav_panel("Plot", plotOutput("plot")),
  #   # Panel with table ----
  #   nav_panel("Table", dataTableOutput("groups_table")),
  #   # Panel with summary ----
  #   nav_panel("Summary", verbatimTextOutput("summary")),
  # )
)
dashboardPage(
  #header 
  dashboardHeader(title = "Analysis of Anime"),
  
  dashboardSidebar(
    sidebarUserPanel(name = "GROUP - 1"),
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Welcome Page', tabName = 'welcome', icon = icon('tv')),
                menuItem('Anime Overview', tabName = 'anime', icon = icon('tv')),
                
                menuItem('Anime', tabName = 'anime-category', icon = icon('tv'),
                         menuItem('Demographic', tabName = 'demographic', icon = icon('tv')),
                         menuItem('Source', tabName = 'source', icon = icon('book')),
                         menuItem('Genre', tabName = 'genre', icon = icon('bars')),
                         menuItem('Season', tabName = 'season', icon = icon('calendar')),
                         menuItem('Type', tabName = 'type', icon = icon('stream')),
                         menuItem('Broadcast', tabName = 'broadcast', icon = icon('building')),
                         menuItem('Duration', tabName = 'duration', icon = icon('folder')),
                         menuItem('Studio', tabName = 'studio', icon = icon('music'))),
                

                menuItem('Interactions', tabName='interactions', icon = icon('chart-area')),
                menuItem('Recommednations', tabName='reco', icon = icon('line-chart')),
                menuItem('Data', tabName='data', icon = icon('database'))
                
                
    )),
  
  
  dashboardBody(    
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = 'welcome',
              h1("Say HELLO to the Anime Universe", align = 'center'),
              img(src='animehomepage.jpg', height="80%", width="80%", 
                  style="display: block; margin-left: auto; margin-right: auto;"),
              img(src='anime.png', height="80%", width="80%",
                  style="display: block; margin-left: auto; margin-right: auto;")
      ),
      
      tabItem(tabName = 'anime',
              h3("General summary of Anime Dataset"),
              tabsetPanel(
                tabPanel('Visulaizations',
                         fluidRow(infoBoxOutput("total"),
                                  infoBoxOutput("avg_score"),
                                  infoBoxOutput("avg_member")
                         ),
                         fluidRow(
                           h3("Summary Histograms of Anime Dataset", align = "center"),
                           column(width = 6,
                                  plotOutput("typeHist")),
                           column(width = 6,
                                  plotOutput("genreHist"))
                         ),
                         fluidRow(
                           column(width = 6,
                                  plotOutput("sourceHist")),
                           column(width = 6,
                                  plotOutput("ratingHist"))
                         )
                         ),
                tabPanel('HeatMaps',
                         fluidRow(
                           column(width = 9,
                                  plotOutput("corr")
                           ),
                           column(width = 3,
                                  selectizeInput("selectcorr","Select Variables to Compute Correlations",
                                                 choices = numeric_var,
                                                 selected = numeric_var,
                                                 multiple = TRUE)
                           )
                         )
                         )
              )
              
      ),
      tabItem(tabName = 'source',
              tabsetPanel(
                fluidRow(
                  column(width = 10,
                         plotOutput("demographic1")
                  )
                ))),
      tabItem(tabName = 'source',
              tabsetPanel(
                fluidRow(
                  column(width = 10,
                         plotOutput("source")
                  )
                ))),
      # tabItem(tabName = 'genre',
      #         tabsetPanel(
      #           
      #           fluidRow(
      #             column(width = 10,
      #                    plotOutput("genre1")
      #             ),
      #             selectInput("count", "Select Country for Comparison",
      #                         choices = c("Mystery", "Horror" ,"Drama", "Action", "Comedy"),
      #                         selected = c("Action", "Comedy"))
      #           ),
      #             
      #             fluidRow(
      #               column(width = 10,
      #                      plotOutput("genre2")
      #               ),
      #               column(width = 2,
      #                      radioButtons("genre2select","Choose an attribute from the following:",
      #                                   choices = list("Score", "Year", "Demographic"),
      #                                   selected = "Year")
      #               ))
      #           ))),
      
     
      tabItem(tabName = 'season',
              tabsetPanel(
                
                         fluidRow(
                           column(width = 10,
                                  plotOutput("season")
                           )
                           ))),
      
      tabItem(tabName = 'type',
              tabsetPanel(
                
                fluidRow(
                  column(width = 10,
                         plotOutput("type")
                  ),
                  column(width = 2,
                                          radioButtons("typeselect","Choose an attribute from the following:",
                                                       choices = list("Score", "Year"),
                                                       selected = "Year")
                                   )
                ))),
      # tabItem(tabName = 'type',
      #         fluidRow(
      #           column(width = 10,
      #                  h3("Summary Plots Related to Durations"),
      #                  plotOutput("type")
      #           )
      #           ,
      #           column(width = 2,
      #                  radioButtons("radioduration","Choose an attribute from the following:",
      #                               choices = list("Score", "Year"),
      #                               selected = "Year")
      #           )
      #         ),
      
      tabItem(tabName = 'broadcast',
              tabsetPanel(
                
                fluidRow(
                  column(width = 10,
                         plotOutput("broadcast1")
                  ),
                  
                  fluidRow(
                    column(width = 10,
                           plotOutput("broadcast2")
                    ))
                ))),
      
      tabItem(tabName = 'duration',
              tabsetPanel(
                
                fluidRow(
                  column(width = 10,
                         plotOutput("duration")
                  )
                ))),
      tabItem(tabName = 'studio',
              tabsetPanel(
                
                fluidRow(
                  column(width = 10,
                         plotOutput("studio1")
                  ),
                  
                  fluidRow(
                    column(width = 10,
                           plotOutput("studio2")
                    ))
                ))),
      tabItem(tabName = "interactions",
              fluidRow(
                column(width = 9,
                       plotOutput("corr")
                ),
                column(width = 3,
                       selectizeInput("selectcorr","Select Variables to Compute Correlations",
                                      choices = numeric_var,
                                      selected = numeric_var,
                                      multiple = TRUE)
                )
              ),
              fluidRow(
                column(width = 9,
                       plotOutput("scatter")
                ),
                column(width = 3,
                       selectizeInput("selecttwo","Select Two Variables for Scatter Plots",
                                      choices = numeric_var,
                                      multiple = TRUE, options = list(maxItems = 2))
                )
              )),          
      
      tabItem(tabName = "data", 
              fluidRow(box(DT::dataTableOutput("table"), width = 200))
      )
      
    
      
      
      
    ))
)



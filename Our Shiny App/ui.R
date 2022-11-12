dashboardPage(
  #header 
  dashboardHeader(title = "Analysis of Anime"),
  
  dashboardSidebar(
    sidebarUserPanel(name = "GROUP - 1"),
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Welcome Page', tabName = 'welcome', icon = icon('tv')),
                menuItem('Anime Overview', tabName = 'anime', icon = icon('tv')),
                
                menuItem('Anime', tabName = 'anime-category', icon = icon('tv'),
                         menuItem('Type', tabName = 'type', icon = icon('tv')),
                         menuItem('Source', tabName = 'source', icon = icon('book')),
                         menuItem('Rating', tabName = 'rating', icon = icon('bars')),
                         menuItem('Year', tabName = 'year', icon = icon('calendar')),
                         menuItem('Duration', tabName = 'duration', icon = icon('stream')),
                         menuItem('Studio', tabName = 'studio', icon = icon('building')),
                         menuItem('Genre', tabName = 'genre', icon = icon('folder')),
                         menuItem('Anime Songs', tabName = 'songs', icon = icon('music'))),
                

                menuItem('Interactions', tabName='interactions', icon = icon('chart-area')),
                menuItem('Rankings', tabName='ranking', icon = icon('line-chart')),
                menuItem('Data', tabName='data', icon = icon('database'))
                
                
    )),
  
  
  dashboardBody(    
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = 'welcome',
              h1("Say HELLO to the Anime Universe", align = 'center'),
              img(src='anime.png', height="80%", width="80%", 
                  style="display: block; margin-left: auto; margin-right: auto;"),
              img(src='akiba.jpg', height="80%", width="80%",
                  style="display: block; margin-left: auto; margin-right: auto;")
      ),
      
      tabItem(tabName = 'anime',
              h3("General summary of Anime Dataset"),
              fluidRow(infoBoxOutput("total"),
                       infoBoxOutput("avg_score"),
                       infoBoxOutput("avg_member")
              ),
              fluidRow(
                h3("Summary Histograms of Anime Dataset", align = "center"),
                column(width = 12,
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
      
     
      tabItem(tabName = 'duration',
              fluidRow(
                column(width = 10,
                       h3("Summary Plots Related to Durations"),
                       plotOutput("duration")
                ),
                column(width = 2,
                       sliderInput("durationlider", "Choose Range of Durations",
                                   min = min(anime1$duration, na.rm = TRUE),
                                   max = max(anime1$duration, na.rm = TRUE), 
                                   value = c(min(anime1$duration, na.rm = TRUE), max(anime1$duration, na.rm = TRUE))
                       ),
                       selectizeInput("selectduration","Select Item to Display",
                                      choices = displaychoice,
                                      selected = "score")
                )
              ),
              fluidRow(
                h3("Summary Info Boxes of Selected Durations"),
                infoBoxOutput("duration1"),
                infoBoxOutput("duration2")
              ),
              fluidRow(
                infoBoxOutput("duration3"),
                infoBoxOutput("duration4")
              )
      ),
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



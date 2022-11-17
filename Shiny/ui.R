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
              h3("General summary of Anime Dataset", align = "center"),
              tabsetPanel(
                tabPanel('visualizations',
                         fluidRow(infoBoxOutput("total"),
                                  infoBoxOutput("avg_score"),
                                  infoBoxOutput("avg_member")
                         ),
                         fluidRow(
                           h3("Summary Histograms of Anime Dataset", align = "center"),
                           column(width = 6,
                                  plotOutput("sourceHist")),
                           column(width = 6,
                                  plotOutput("ratingHist"))
                         ),
                         fluidRow(
                           
                           
                           column(align="center", width = 6, 
                                  plotOutput("genreHist"))
                         )
                         
                         ),
                tabPanel('Heatmap',
                         fluidRow(
                           column(width = 9,
                                  plotOutput("corr")
                           ),
                           column(width = 3,
                                  selectizeInput("selectcorr","Select Variables to Compute Correlations",
                                                 choices = numeric_var,
                                                 selected = numeric_var[1:4],
                                                 multiple = TRUE)
                           )
                         )
                         )
              )
              
      ),
      tabItem(tabName = 'demographic',
                fluidRow(
                  column(width = 10,
                         plotOutput("demographic1")
                  ),
                  column(width = 2,
                                            radioButtons("demographicselect","Choose an attribute from the following:",
                                            choices = list("Score", "Year"),
                                            selected = "Score")
                                                      )
                )
              ,
                fluidRow(
                  column(width = 10,
                         plotOutput("demographic2")
                  ),
                  column(width = 2,
                         radioButtons("demoselect","Choose an attribute from the following:",
                                      choices = list("Score", "Year"),
                                      selected = "Year"),
                         checkboxGroupInput("scorecatselect", 
                                            h3("Choose Score Category"), 
                                            choices = list("6+" = "6+", 
                                                           "7+" = "7+", 
                                                           "8+" = "8+",
                                                           "9+" = "9+"),
                                            selected = "6+"),
                         checkboxGroupInput("yearcatselect", 
                                            h3("Choose Year Category"), 
                                            choices = list("1963 - 1973" = "1963 - 1973", 
                                                           "1974 - 1983" = "1974 - 1983", 
                                                           "1984 - 1993" = "1984 - 1993",
                                                           "1994 - 2003" = "1994 - 2003",
                                                           "2004 - 2013" = "2004 - 2013",
                                                           "2014 - 2022" = "2014 - 2022"),
                                            selected = "2004 - 2013")
                         
                         )
                  )
                  )
                
                
                ,
      tabItem(tabName = 'source',
              tabsetPanel(
                fluidRow(
                  column(width = 10,
                         plotOutput("source")
                  )
                ))),
      tabItem(tabName = 'genre',
              

                fluidRow(
                  column(width = 10,
                         plotOutput("genre1")
                  ),
                  column(width = 2,
                         checkboxGroupInput("vennselect", 
                                            h3("Choose Genre Category"), 
                                            choices = list("Mystery" = "Mystery", 
                                                           "Horror" = "Horror", 
                                                           "Drama" = "Drama",
                                                           "Action" = "Action",
                                                           "Comedy" = "Comedy"),
                                            selected = c("Comedy", "Action"))
                         )
                  
                ),

                  fluidRow(
                    column(width = 10,
                           plotOutput("genre2")
                    ),
                    column(width = 2,
                           radioButtons("genre2select","Choose an attribute from the following:",
                                        choices = list("Score", "Year"),
                                        selected = "Year"),
                           checkboxGroupInput("scorecatselect2", 
                                              h3("Choose Score Category"), 
                                              choices = list("6+" = "6+", 
                                                             "7+" = "7+", 
                                                             "8+" = "8+",
                                                             "9+" = "9+"),
                                              selected = "6+"),
                           checkboxGroupInput("yearcatselect2", 
                                              h3("Choose Year Category"), 
                                              choices = list("1963 - 1973" = "1963 - 1973", 
                                                             "1974 - 1983" = "1974 - 1983", 
                                                             "1984 - 1993" = "1984 - 1993",
                                                             "1994 - 2003" = "1994 - 2003",
                                                             "2004 - 2013" = "2004 - 2013",
                                                             "2014 - 2022" = "2014 - 2022"),
                                              selected = "2004 - 2013")
                           
                    ))
                ),


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
                )
                
              )),

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
                  column(width = 8,
                         plotOutput("duration"), align = "center"
                  )
                ))),
      tabItem(tabName = 'studio',

                fluidRow(
                  column(width = 12,
                         plotOutput("studio1")
                  ),

                  fluidRow(
                    column(width = 12,
                           plotOutput("studio2")
                    ))
                )),
      tabItem(tabName = 'reco',
              
                
                fluidRow(
                  column(width = 12,
                         plotOutput("reco"), align = "center"
                  )
                )),
      tabItem(tabName = "data", 
              fluidRow(box(DT::dataTableOutput("table"), width = 200))
      )
      
    
      
      
      
    ))
)



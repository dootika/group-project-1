function(input, output, session){
  # infoboxes
  output$total <- renderInfoBox(
    infoBox("Total Animes", nrow(anime1), icon("chart-bar"))
  )
  output$avg_score <- renderInfoBox(
    infoBox("Average Score", sprintf("%.2f", mean(anime1$score, na.rm = TRUE)), icon("chart-bar"))
  )
  output$avg_member <- renderInfoBox(
    infoBox("Average Number Watching", sprintf("%.2f", mean(mem, na.rm = TRUE)), 
            icon("chart-bar"))
  )
  
  
  
  
  # Summarize all the anime here
  output$typeHist <- renderPlot({
    heat_m((numeric_var[numeric_var != "Rank"])[1:8],"pearson")
  })
  output$genreHist <- renderPlot({
    # function for margins
    background_image <- function(raster.img){
      annotation_raster(raster.img,
                        xmin = -Inf, xmax = Inf,
                        ymin = -Inf, ymax = Inf)
    }
    img <- jpeg::readJPEG("yearp.jpeg")
    
    plot_d <- three_var_na(dat$YEAR, dat$Rating, dat$SCORE)
    rating_year <- ggplot(plot_d, aes(x = v1, fill = v2)) + background_image(img)+
      geom_bar(position="dodge") + 
      labs(x = "YEAR", y = "COUNT") + 
      theme_stata() + scale_color_stata() + 
      ggtitle("RATING - YEAR")
    rating_year <- rating_year + guides(fill=guide_legend(title="RATING"))
    rating_year
    
  })
  output$sourceHist <- renderPlot({
    # function for margins
    background_image <- function(raster.img){
      annotation_raster(raster.img,
                        xmin = -Inf, xmax = Inf,
                        ymin = -Inf, ymax = Inf)
    }
    img <- jpeg::readJPEG("yearp.jpeg")
    
    plot_d <- three_var_na(dat$YEAR, dat$score, dat$SCORE)
    score_year <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + 
      background_image(img) +
      geom_boxplot() + labs(x = "Year", y = "SCORE") +
      theme_stata() + scale_color_stata() + 
      ggtitle("SCORE - YEAR")
    score_year <- score_year +  guides(fill=guide_legend(title="SCORE"))
    score_year
  })
  
  output$ratingHist <- renderPlot({
    img <- jpeg::readJPEG("yearp.jpeg")
    
    plot_d1 <- two_var_na(dat$YEAR, (dat$popularity))
    popularity_year <- ggplot(plot_d1, aes(x = v1, y = v2)) + 
      background_image(img) +
      geom_boxplot() + labs(x = "YEAR", y = "POPULARITY RANKS") +
      theme_stata() + scale_color_stata() + 
      ggtitle("POPULARITY - YEAR")
    popularity_year <- popularity_year +  guides(fill=guide_legend(title="SCORE"))
    popularity_year
  })
  
  
  output$demographic1 <- renderPlot({
    tree <- function(vec,inside)
    {
      if(inside == "SCORE")
      {
        plt <- treemap(dat, #Your data frame object
                       index=c(vec,inside),  #A list of your categorical variables
                       vSize = "score",  #This is your quantitative variable
                       type = "index", #Type sets the organization and color scheme of your treemap
                       palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
                       title=paste("Treemap of",vec,"with respect to SCORE"), #Customize your title
                       fontsize.title = 15 #Change the font size of the title
        )
      }
      else if(inside == "YEAR")
      {
        plt <- treemap(dat, #Your data frame object
                       index=c(vec,inside),  #A list of your categorical variables
                       vSize = "Year",  #This is your quantitative variable
                       type = "index", #Type sets the organization and color scheme of your treemap
                       palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
                       title=paste("Treemap of",vec,"with respect to YEAR"), #Customize your title
                       fontsize.title = 15 #Change the font size of the title
        )
      }
      
      return(plt)
    }
    if(input$demographicselect == "Year"){
      tempdemo <- c("YEAR")
    }
    if(input$demographicselect == "Score"){
      tempdemo <- c("SCORE")
    }
    
    tree("demographic",tempdemo)
  })
  output$demographic2 <- renderPlot({
    pie_chart_dem <- function(scor,col)
    {
      data1 <- subset(dat,dat[,col] %in% scor)
      perc <- round(100*table(data1$demographic)/sum(table(data1$demographic)), 1)
      plt <-pie(table(data1$demographic), main = paste("Target Audience for",scor,col))
      return(plt)
    }
    
    if(input$demoselect == "Year"){
      tempdemo2 <- c("YEAR")
      score_sub <- input$yearcatselect
    }
    if(input$demoselect == "Score"){
      tempdemo2 <- c("SCORE")
      score_sub <- input$scorecatselect
      
    }
    # colm <- "score"
    # score_sub <- c("2004 - 2013")
    pie_chart_dem(score_sub,tempdemo2)
  })
  
  
  output$source <- renderPlot({
    hist_p <- function(x,n)
    {
      a <- dat[,x]
      df <- data.frame(table(a))
      df <- df[order(df$Freq, decreasing = TRUE), ]
      top20 <- head(df, n)
      top20$a <- reorder(top20$a, top20$Freq)
      plott <- ggplot(top20, aes(x = a, y = Freq, fill = a, label = Freq)) +
        geom_bar(stat="identity", show.legend = FALSE)  +
        coord_flip() +
        labs(title = paste("Top",n,"frequently occuring", x), x = paste(x), y = "Count") +
        geom_label(aes(fill = a),colour = "white", fontface = "bold", show.legend = FALSE)
      
      return(plott)
    }
    
    hist_p("Final_Source",8)
  })
  output$genre1 <- renderPlot({
    # please input list
    vennd <- function(vec)
    {
      veci <- vec[1:4]
      veci <- veci[!(is.na(veci))]
      x <- select(dat,veci)
      x <- x %>% mutate_all(as.logical)
      x <- tibble(x)
      plt <- ggvenn(x)
      return(plt)
    }
    vec <- input$vennselect
    vennd(vec)
  })
  
  output$genre2 <- renderPlot({
    library(ggplot2)
    library(forcats)
    pie_chart_genre <- function(scor,col)
    {
      data1 <- subset(dat,dat[,col] %in% scor)
      values <- c(sum(data1$Mystery),sum(data1$Romance), sum(data1$Action), sum(data1$Horror), sum(data1$Comedy), sum(data1$Others))
      labels <- c("MYSTERY", "ROMANCE", "ACTION", "HORROR", "COMEDY", "OTHERS")
      summ <- sum(values)
      
      df <- data.frame(value = values, group = labels)
      df2 <- df %>% 
        mutate(csum = rev(cumsum(rev(value))), 
               pos = value/2 + lead(csum, 1),
               pos = if_else(is.na(pos), value/2, pos))
      plt <- ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(data = df2,
                         aes(y = pos, label = paste0(round(value/summ*100,2),"% - " ,group)),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Group")) +
        theme_void()
      return(plt)
    }
    if(input$genre2select == "Year"){
      colm <- "YEAR"
      score_sub <- input$yearcatselect2
    }
    if(input$genre2select == "Score"){
      colm <- c("SCORE")
      score_sub <- input$scorecatselect2
    }
    if(input$genre2select == "Demographic"){
      colm <- c("demographic")
    }
    
    
    pie_chart_genre(score_sub,colm)
  })
  
  
  output$season <- renderPlot({
    plot_d <- three_var_na(dat$season, dat$score, dat$SCORE)
    p4 <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) +
      geom_boxplot() + labs(x = "SEASON", y = "SCORE")
    p4 <- p4 + guides(fill=guide_legend(title="SCORE"))
    p4
  })
  
  output$type <- renderPlot({
    tree <- function(vec,inside)
    {
      if(inside == "SCORE")
      {
        plt <- treemap(dat, #Your data frame object
                       index=c(vec,inside),  #A list of your categorical variables
                       vSize = "score",  #This is your quantitative variable
                       type = "index", #Type sets the organization and color scheme of your treemap
                       palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
                       title=paste("Treemap of",vec,"with respect to SCORE"), #Customize your title
                       fontsize.title = 15 #Change the font size of the title
        )
      }
      else if(inside == "YEAR")
      {
        plt <- treemap(dat, #Your data frame object
                       index=c(vec,inside),  #A list of your categorical variables
                       vSize = "Year",  #This is your quantitative variable
                       type = "index", #Type sets the organization and color scheme of your treemap
                       palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
                       title=paste("Treemap of",vec,"with respect to YEAR"), #Customize your title
                       fontsize.title = 15 #Change the font size of the title
        )
      }
      
      return(plt)
    }
    if(input$typeselect == "Year"){
      temptype <- c("YEAR")
    }
    if(input$typeselect == "Score"){
      temptype <- c("SCORE")
    }
    
    tree("Type",temptype)
  })
  
  output$broadcast1 <- renderPlot({
    plot_d <- three_var_na(dat$broadcast, dat$score, dat$SCORE)
    broadcast_score <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + 
      geom_boxplot() + labs(x = "Day of Broadcast", y = "SCORE")
    broadcast_score <- broadcast_score + guides(fill=guide_legend(title="SCORE"))
    broadcast_score
  })
  
  output$broadcast2 <- renderPlot({
    word_c <- function(x)
    {
      a <- dat[,x]
      df <- data.frame(table(a))
      set.seed(42)
      df <- df %>%
        mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
      plott <- ggplot(df,aes(label = a, size = Freq,
                             color = factor(sample.int(10, nrow(df), replace = TRUE)))) +
        geom_text_wordcloud_area() +
        scale_size_area(max_size = 10) +
        theme_minimal()
      return(plott)
    }
    # word_c("Studios")
    word_c("broadcast")
  })
  
  
  output$duration <- renderPlot({
    plot_d <- three_var_na(dat$Episode, dat$duration, dat$SCORE)
    duration_eps <- ggplot(plot_d, aes(x = v1, y = v2, color = v3)) + 
      geom_point() + labs(x = "EPISODES", y = "DURATION (in min)")+ geom_smooth(method = "lm",se = FALSE) +
      xlim(0,250)+ ylim(0,200) + geom_vline(xintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"Episode"],linetype = "dashed", color="red") + 
      geom_hline(yintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"duration"],linetype = "dashed", color="red") 
    duration_eps <- duration_eps + guides(fill=guide_legend(title="SCORE"))
    duration_eps
  })
  
  
  
  output$studio1 <- renderPlot({
    hist_p <- function(x,n)
    {
      a <- dat[,x]
      df <- data.frame(table(a))
      df <- df[order(df$Freq, decreasing = TRUE), ]
      top20 <- head(df, n)
      top20$a <- reorder(top20$a, top20$Freq)
      plott <- ggplot(top20, aes(x = a, y = Freq, fill = a, label = Freq)) +
        geom_bar(stat="identity", show.legend = FALSE)  +
        coord_flip() +
        labs(title = paste("Top",n,"frequently occuring", x), x = paste(x), y = "Count") +
        geom_label(aes(fill = a),colour = "white", fontface = "bold", show.legend = FALSE)
      
      return(plott)
    }

    hist_p("Studios",8)
  })
  
  output$studio2 <- renderPlot({
    # defining function to plot WORDCLOUD
    word_c <- function(x)
    {
      a <- dat[,x]
      df <- data.frame(table(a))
      set.seed(42)
      df <- df %>%
        mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
      plott <- ggplot(df,aes(label = a, size = Freq,
                             color = factor(sample.int(10, nrow(df), replace = TRUE)))) +
        geom_text_wordcloud_area() +
        scale_size_area(max_size = 10) +
        theme_minimal()
      return(plott)
    }
    word_c("Studios")

  })
  
  
  #type plot/table
  
  
  #Source plot/table
  
  #Duration plot/table
  
  
  
  
  # Interactions
  output$corr <- renderPlot(
    ggcorr(anime1[input$selectcorr], label = TRUE, palette = "RdGy",
           label_size = 5, label_round = 2, label_color = "black") +
      ggtitle("Correlation Plots") + 
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  scatter_plot_stat <- reactive(
    plot_scatter <- two_var_na(dat$input$selecttwo[1], dat$input$selecttwo[2])
    # anime1[is_outlier(anime1[,input$selecttwo[1]]) == F &
    #          is_outlier(anime1[,input$selecttwo[2]]) == F,]
  )
  
  
  output$scatter <- renderPlot(
    
    ggplot(scatter_plot_stat(), (aes(x = scatter_plot_stat()[,input$selecttwo[1]],
                                     y = scatter_plot_stat()[,input$selecttwo[2]])), na.rm = T) + geom_point() +
      geom_smooth(method='lm') + xlab(input$selecttwo[1]) + ylab(input$selecttwo[2]) + 
      ggtitle("Scatter Plots of Two Variables") + theme_bw()+ 
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
      theme(plot.title = element_text(size = 20, face = "bold"))+
      theme(axis.title = element_text(size = 12, face = "bold"))
  )
  
  # Rankings
  output$reco <- renderPlot({
    rec <- function(x)
    {
      img <- load.image(x)
      pl <- plot(img, axes =  FALSE)
      return(pl)
    }
    
    data <- head(dat,10)
    par(mfrow = c(2,5))
    for(i in 1:10)
    {
      rec(data$Image_links[i])
      title(paste(data$Name[i]),"\n",paste("SCORE:",data$score[i]))
      
    }
  })
  
  #data table
  output$table <- DT::renderDataTable({
    datatable(anime1, options = list(scrollX = TRUE,autoWidth = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
}
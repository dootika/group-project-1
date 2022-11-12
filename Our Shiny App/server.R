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
    numeric_var <- c()
    for(i in 1:dim(dat)[2])
    {
      if(typeof(dat[,i]) == "double" | typeof(dat[,i]) == "integer")
      {
        numeric_var <- append(numeric_var,colnames(dat)[i])
      }
    }
    numeric_var
    data <- dat[,numeric_var]
    data1 <- na.omit(data)
    corr_mat <- round(cor(data1),2)
    melted_corr_mat <- melt(corr_mat)
    heat_map_numeric <- ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                                           fill=value)) +
      geom_tile(color = "black",
                lwd = 1.2,
                linetype = 1)+
      scale_fill_gradient2(low = "#075AFF",
                           mid = "#FFFFCC",
                           high = "#FF0000") +
      coord_fixed()+ xlab(" ") + ylab(" ")+
      geom_text(aes(Var2, Var1, label = value),
                color = "black", size = 4)
    heat_map_numeric
  })
  
  
  
  
  #type plot/table
  
  
  #Source plot/table
  
  #Duration plot/table
  anime1$mem
  duration_table <- reactive({
    anime1 %>% filter(duration > input$durationlider[1] & duration < input$durationlider[2]) %>% 
      group_by(duration) %>% summarise(count = n(), average_score = mean(score),
                                       average_raters = mean(Rating),
                                       average_watching = mean(Members),
                                       average_favorites = mean(Favorites))
  })
  
  output$duration <- renderPlot(
    
      duration_table()%>%
        ggplot() + geom_point(aes_string(x = "duration", y = paste(strsplit(input$selectduration, " ")[[1]],
                                                                   collapse = "_"))) +
        ggtitle('Plot of Characteristics of Anime VS. Durations') + theme_bw()+ 
        theme(axis.text.x = element_text(face = "bold", color = "black", size = 16)) +
        theme(axis.text.y = element_text(face = "bold", color = "black", size = 16)) +
        theme(plot.title = element_text(size = 20, face = "bold"))+
        theme(axis.title = element_text(size = 12, face = "bold"))
    
  )
  
  
  
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
  
  
  #data table
  output$table <- DT::renderDataTable({
    datatable(anime1, options = list(scrollX = TRUE,autoWidth = TRUE), rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
}

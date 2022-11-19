load("Anime_ranking.Rdata")
library(ggplot2)
### Barplot of source of anime with type
g <- ggplot(data, aes(Source))
g + geom_bar(aes(fill=Type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorial Variable of Anime Data ", 
       subtitle="Source across Type ") 


####Graph of members vs Source 
load("data1.Rdata")
 library(ggplot2)
 dat$Members <- as.numeric(as.character(dat$Members))
 r <- ggplot(dat, aes(x = Final_Source, y = Members))
 r + geom_boxplot(aes(col = Type )) + labs(title = "Boxpot",
                                  subtitle = "Members VS Source Across Type",
                                          x = "Source", y = "Members") 

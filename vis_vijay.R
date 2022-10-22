load("Anime_ranking.Rdata")
library(ggplot2)
### Barplot of source of anime with type
g <- ggplot(data, aes(Source))
g + geom_bar(aes(fill=Type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorial Variable of Anime Data ", 
       subtitle="Source across Type ") 

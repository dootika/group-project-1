library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyr)
library(googleVis)
library(DT)
library(dashboardthemes)
library(GGally)
# library(ggplot)
library(imager)
library(rvest)
library(ggthemes)
library(ggpubr)
library(ggwordcloud)
# install.packages("ggplot")
library(reshape2)
library(ggvenn)
library(treemap)
library(ggrepel)

load("data1.RData")

anime1 <- dat

choice <- c("score", "raters", "members", "favorites")
unique_source <- unique(anime1$Final_Source)
unique_rating <- unique(anime1$Rating)
displaychoice <- c("count","average score", "average raters", "average watching", "average favorites")
displaychoice1 <- c("average per year","average score", "average raters", 
                    "average watching", "average favorites")
#FINDING AVERAGE MEMBERS 
mem <- anime1$Members
mem <- as.numeric(mem)
mem_na <- which(is.na(mem) == TRUE)
mem[mem_na] = 0
mean(mem)

min = min(anime1$duration, na.rm = TRUE)
max = max(anime1$duration, na.rm = TRUE) 
value = c(min(anime1$duration), max(anime1$duration) - 1)
# ind_na_duration <- which(is.na(anime1$duration))
# anime1$duration[ind_na_duration] = -1
# min(anime1$duration)
max(anime1$duration, na.rm = TRUE)


#install.packages("ggwordcloud") # Install 




colnames(dat)
for(i in 1:ncol(dat))
{
  print(paste(colnames(dat)[[i]],length(unique(dat[[i]])),sum(is.na(dat[[i]]))))
}
factors <- c("Type","Status","YEAR","demographic","SCORE","Final_Source",
             "season","broadcast","Rating")
num <- c()


# _______________________________________________________________________
# -----------------------------------------------------------------------
# 1. REMOVING NA VALUES AUTOMATED
# _______________________________________________________________________
# -----------------------------------------------------------------------

# Removing NA values 
# 2 variable
two_var_na <- function(v1, v2)
{
  data <- data.frame(v1,v2)
  data <- data[((is.na(v1)) | (is.na(v2))) == 0,]
  return(data)
}
# 3 variable
three_var_na <- function(v1, v2, v3)
{
  data <- data.frame(v1,v2,v3)
  data <- data[((is.na(v1)) | (is.na(v2)) | (is.na(v3))) == 0,]
  return(data)
}
# 4 variable
four_var_na <- function(v1, v2, v3, v4)
{
  data <- data.frame(v1,v2,v3,v4)
  data <- data[((is.na(v1)) | (is.na(v2))| (is.na(v3))| (is.na(v4))) == 0,]
  return(data)
}

# _______________________________________________________________________
# -----------------------------------------------------------------------
# 2. NUMERIC VARIABLE
# _______________________________________________________________________
# -----------------------------------------------------------------------

numeric_var <- c()
for(i in 1:dim(dat)[2])
{
  if(typeof(dat[,i]) == "double" | typeof(dat[,i]) == "integer")
  {
    numeric_var <- append(numeric_var,colnames(dat)[i])
  }
}
numeric_var

heat_m <- function(num,methodd)
{
  data <- dat[,num]
  data1 <- na.omit(data)
  corr_mat <- round(cor(data1, method = methodd),2)
  melted_corr_mat <- melt(corr_mat)
  
  heat_map_numeric <- ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                                         fill=value)) +
    geom_tile(color = "black",
              lwd = 1.2,
              linetype = 1)+
    scale_fill_gradient2() +
    coord_fixed()+ xlab(" ") + ylab(" ")+
    geom_text(aes(Var2, Var1, label = value),
              color = "black", size = 4) 
  return(heat_map_numeric)
  
}













# _______________________________________________________________________
# -----------------------------------------------------------------------
# 3. COMMON PLOT FUNCTION
# _______________________________________________________________________
# -----------------------------------------------------------------------

# _______________________________________________________________________
# 3.1 SCATTER PLOT
# _______________________________________________________________________
re <- function(x,y,i)
{
  plot_d <- three_var_na(x, y, as.factor(dat[,factors[i]]))
  pl <- ggplot(plot_d, aes(x = v1, y = v2, color = v3)) + 
    geom_point() + labs(x = "V1", y = "V2") + xlim(c(min(x,na.rm = TRUE),max(x,na.rm = TRUE))) +
    ylim(c(min(y,na.rm = TRUE),max(y,na.rm = TRUE)))
  return(pl)
}


ply <- function(x,y)
{
  figure <- ggarrange(re(x,y,1), re(x,y,2),
                      re(x,y,3), re(x,y,4),
                      re(x,y,5), re(x,y,6),
                      re(x,y,7), re(x,y,8),
                      re(x,y,9),
                      labels = factors,
                      ncol = 3, nrow = 3)
  return(figure)
}
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
colm <- "YEAR"
score_sub <- "2004 - 2013"
pie_chart_genre(score_sub,colm)


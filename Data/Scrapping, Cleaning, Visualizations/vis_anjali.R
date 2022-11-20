#install.packages("ggthemes") # Install 
#install.packages("ggpubr") # Install 
#install.packages("ggwordcloud") # Install 
#install.packages("ggrepel")# Install 
#install.packages("reshape2") # Install 
#install.packages("ggvenn") # Install 
#install.packages("treemap") # Install 


library(ggplot2)
library(imager)
library(rvest)
library(ggthemes)
library(ggpubr)
library(ggwordcloud)
library(ggrepel)
library(tidyverse)
library(reshape2)
library(ggvenn)
library(treemap)


load("data1.RData")
View(dat)

colnames(dat)
for(i in 1:ncol(dat))
{
  print(paste(colnames(dat)[[i]],length(unique(dat[[i]])),sum(is.na(dat[[i]]))))
}
factors <- c("Type","Status","YEAR","demographic","SCORE","Final_Source",
             "season","broadcast","Rating")
num <- c()

colnames(dat)
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
    ylim(c(min(y,na.rm = TRUE),max(y,na.rm = TRUE))) + geom_smooth(method = "lm", se = FALSE)
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

# _______________________________________________________________________
# -----------------------------------------------------------------------
# ROUGH PLOTTING --1
# _______________________________________________________________________
# -----------------------------------------------------------------------

ply(dat$Year,dat$popularity)
ply(dat$Year,dat$score)
ply(dat$YEAR,dat$score)
ply(dat$Members, dat$Favorites)
ply(dat$Episode, dat$duration)

# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENERAL TAB : PLOT 1  <-  RATING - YEAR (BARPLOT)
# _______________________________________________________________________
# -----------------------------------------------------------------------

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



# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENERAL TAB : PLOT 2  <-  HEATMAP(NUMERIC VAR)
# _______________________________________________________________________
# -----------------------------------------------------------------------


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

heat_m((numeric_var[numeric_var != "Rank"])[1:8],"pearson")
heat_m((numeric_var[numeric_var != "popularity"])[1:8],"pearson")
heat_m(c("Rank","popularity"),"spearman")
img
# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENERAL TAB : PLOT 3  <-  SCORE - YEAR (BOXPLOT)
# _______________________________________________________________________
# -----------------------------------------------------------------------

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

img <- jpeg::readJPEG("yearp.jpeg")

plot_d1 <- two_var_na(dat$YEAR, (dat$popularity))
popularity_year <- ggplot(plot_d1, aes(x = v1, y = v2)) + 
  background_image(img) +
  geom_boxplot() + labs(x = "YEAR", y = "POPULARITY RANKS") +
  theme_stata() + scale_color_stata() + 
  ggtitle("POPULARITY - YEAR")
popularity_year <- popularity_year +  guides(fill=guide_legend(title="SCORE"))
popularity_year

# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENERAL TAB : PLOT 4  <-  WORDCLOUD
# _______________________________________________________________________
# -----------------------------------------------------------------------

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
word_c("broadcast")


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
hist_p("Year",2500)

a <- dat[,"Studios"]
df <- data.frame(table(a))
View()
df)
top20 <- head(df, n)
top20$a <- reorder(top20$a, top20$Freq)
plott <- ggplot(top20, aes(x = a, y = Freq, fill = a, label = Freq)) +
  geom_bar(stat="identity", show.legend = FALSE)  +
  coord_flip() +
  labs(title = paste("Top",n,"frequently occuring", x), x = paste(x), y = "Count") +
  geom_label(aes(fill = a),colour = "white", fontface = "bold", show.legend = FALSE)


# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENRE TAB : PLOT 1  <- PIECHART
# _______________________________________________________________________
# -----------------------------------------------------------------------

pie_chart_genre <- function(scor,col)
{
  heading <- col
  data1 <- subset(dat,dat[,col] %in% scor)
  values <- c(sum(data1$Mystery),sum(data1$Romance), sum(data1$Action), sum(data1$Horror), sum(data1$Comedy), sum(data1$Others))
  labels <- c("MYSTERY", "ROMANCE", "ACTION", "HORROR", "COMEDY", "OTHERS")
  summ <- sum(values)
  
  df <- data.frame(value = values, group = labels)
  df2 <- df %>% 
    mutate(csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  plt <- ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) + ggtitle(paste("DISTRIBUTION OF GENRE ACCORDING TO",col)) +
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

colm <- "SCORE"
score_sub <- c("9+")

colm <- "demographic"
score_sub <- c("Girls(12-18yr)","Women(18-40yr)")
score_sub <- c("Boys(12-18yr)","Men(18-40yr)")

p1 <- pie_chart_genre(score_sub,colm)
p2 <- pie_chart_genre(score_sub,colm)
p2
unique(dat$demographic)



# colm_dem = "YEAR","SCORE"
pie_chart_dem <- function(scor,col)
{
  data1 <- subset(dat,dat[,col] %in% scor)
  perc <- round(100*table(data1$demographic)/sum(table(data1$demographic)), 1)
  plt <-pie(table(data1$demographic), main = paste("Target Audience for",paste(scor),col))
  return(plt)
}
colm <- "YEAR"
score_sub <- c("2004 - 2013")

colm <- "SCORE"
score_sub <- c("6+")
pie_chart_dem(score_sub,colm)

cor(na.omit(dat[,29:34]))
unique(dat$Type)



# _______________________________________________________________________
# -----------------------------------------------------------------------
# SOURCE TAB : PLOT 1  <-  TREEMAP
# _______________________________________________________________________
# -----------------------------------------------------------------------


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
tree("demographic","SCORE")

# _______________________________________________________________________
# -----------------------------------------------------------------------
# GENRE TAB : PLOT 1  <-  VENN DIAGRAM
# _______________________________________________________________________
# -----------------------------------------------------------------------

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
vec <- c("Mystery", "Romance","Comedy","Action","Horror") 
vennd(vec)

# _______________________________________________________________________
# -----------------------------------------------------------------------
# RECCOMENDATION
# _______________________________________________________________________
# -----------------------------------------------------------------------

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


# _______________________________________________________________________
# -----------------------------------------------------------------------
# DURATION-EPISODE
# _______________________________________________________________________
# -----------------------------------------------------------------------

plot_d <- three_var_na(dat$Episode, dat$duration, dat$SCORE)
duration_eps <- ggplot(plot_d, aes(x = v1, y = v2, color = v3)) + 
  geom_point() + labs(x = "EPISODES", y = "DURATION (in min)")+ geom_smooth(method = "lm",se = FALSE) +
  xlim(0,250)+ ylim(0,200) + geom_vline(xintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"Episode"],linetype = "dashed", color="red") + 
  geom_hline(yintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"duration"],linetype = "dashed", color="red") 
duration_eps <- duration_eps + guides(color =guide_legend(title="SCORE"))
duration_eps

# _______________________________________________________________________
# -----------------------------------------------------------------------
# BROADCAST TAB: PLOT 1: BROADCAST- SCORE
# _______________________________________________________________________
# -----------------------------------------------------------------------

plot_d <- three_var_na(dat$broadcast, dat$score, dat$SCORE)
broadcast_score <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + 
  geom_boxplot() + labs(x = "Day of Broadcast", y = "SCORE")
broadcast_score <- broadcast_score + guides(fill=guide_legend(title="SCORE"))
broadcast_score

# _______________________________________________________________________
# -----------------------------------------------------------------------
# SEASON - SCORE RELATION
# _______________________________________________________________________
# -----------------------------------------------------------------------

img2 <- jpeg::readJPEG("season.jpeg")
plot_d <- three_var_na(dat$season, dat$score, dat$SCORE)
p4 <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + background_image(img2) +
  geom_boxplot() + labs(x = "SEASON", y = "SCORE")  
p4 <- p4 + guides(fill=guide_legend(title="SCORE"))
p4



# _______________________________________________________________________
# -----------------------------------------------------------------------
# ROUGH PLOTTING -- 2
# _______________________________________________________________________
# -----------------------------------------------------------------------

# general
# plot_d <- three_var_na(dat$Year, dat$Favorites, dat$SCORE)
# p2 <- ggplot(plot_d, aes(x = v1, y = v2, color = v3)) + 
#   geom_point() + labs(x = "Year of Release", y = "Number of people who tagged the anime 'Favourite'")+
#   xlim(1950,2024) + geom_vline(xintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"Year"],linetype = "dashed", color="red") + 
#   geom_hline(yintercept = dat[which(dat$score == max(dat$score,na.rm = TRUE)),"Favorites"],linetype = "dashed", color="red")
# p2 <- p2 + guides(fill=guide_legend(title="SCORE"))
# p2


# p5 <- ggplot(plot_d, aes(x = v1, fill = v3)) + 
#   geom_bar() + labs(x = "season", y = "favourite")
# p5

# 
# 
# plot_d <- three_var_na(dat$Rank, dat$popularity, dat$SCORE)
# p6 <- ggplot(plot_d, aes(x = v1, y = v2, col = v3)) + 
#   geom_point() + labs(x = "Rank", y = "Popularity")
# p6 <- p6 + guides(fill=guide_legend(title="SCORE"))
# p6
# 
# plot_d <- three_var_na(dat$Year, dat$Name_length, dat$SCORE)
# p7 <- ggplot(plot_d, aes(x = v1, y = v2, col = v3)) + 
#   geom_point() + labs(x = "Year", y = "name_length")
# p7 <- p7 + guides(fill=guide_legend(title="SCORE"))
# p7
# 
# p8 <- ggplot(plot_d, aes(x = v1, y = v2)) + 
#   geom_boxplot() + labs(x = "Year", y = "name_length")
# p8
# 
# 
# plot_d <- three_var_na(dat$YEAR, dat$Name_length, dat$SCORE)
# p9 <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + 
#   geom_boxplot() + labs(x = "Year", y = "name_length")
# p9
# 
# plot_d <- three_var_na(dat$YEAR, dat$score, dat$SCORE)
# p91 <- ggplot(plot_d, aes(x = v1, y = v2, fill = v3)) + 
#   geom_boxplot() + labs(x = "Year", y = "SCORE")
# p91
# 
# plot_d <- three_var_na(dat$YEAR,10 -log(dat$popularity), dat$SCORE)
# p10 <- ggplot(plot_d, aes(x = v2, y = v1)) + 
#   geom_violin() + labs(x = "POPULAITY", y = "YEAR")
# p10
# 





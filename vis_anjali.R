# install.packages("ggthemes") # Install 

library(ggplot2)
library(imager)
library(rvest)
library(ggthemes)
library(ggpubr)
library(ggwordcloud)
load("data1.RData")
View(dat)

colnames(dat)
for(i in 1:28)
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

# _______________________________________________________________________
# 3.2 BARPLOT
# _______________________________________________________________________


# _______________________________________________________________________
# -----------------------------------------------------------------------
# ROUGH PLOTTING
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

data <- dat[,numeric_var]
data1 <- na.omit(data)
corr_mat <- round(cor(data1),2)
melted_corr_mat <- melt(corr_mat)
View(melted_corr_mat)

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
word_c("Premiered")




dat <- load("data1.Rdata")
dat
View(dat)
View(table(dat$score))
View(table(dat$Genres))
plot(dat$score,dat$Genres)
# remove na in r - test for missing values (is.na example)
test <- c(1,2,3,NA) is.na(test)
is.na(dat$score)
View(table(dat$duration))
plot(dat$score,dat$duration)
??scatterplot
View(table(dat$Premiered))
plot(dat$Premiered,dat$score,)
library(ggplot2)
install.packages("ggplot2")
ggplot2.scatterplot(data=df, xName='dat$score',yName='dat$duration', 
                    groupName='popularity', size=3,
                    backgroundColor="white", setColorByGroupName=FALSE)  
??ggplot2
ggplot(dat$popularity, aes(dat$score, dat$duration)) + 
  stat_chull(geom = "point", size = 4, colour = "red") +
  geom_point()
?boxplot
# boxplot on a data frame:
df. <- as.data.frame(dat)
par(las = 1) # all axis labels horizontal
boxplot(df., main = "boxplot(*, horizontal = TRUE)", horizontal = TRUE)


######
plot(dat$Year,dat$score)
ggplot(dat, aes(dat$Year,dat$score)) +
  geom_boxplot()
View(table(dat$duration))
d <- density(dat$score)
plot(d)
hist(dat$duration,dat$popularity)
View(table(dat$Name_length))
View(table(dat$Rating))
?barplot()
barplot(dat$Year, space = 2.6)
View(table(dat$Year,dat$Image_links))
hist(dat$Year)
lines(dat$Year)
install.packages("treemap")
library(treemap)

treemap(dat, #Your data frame object
        index=c("Type","Source","SCORE"),  #A list of your categorical variables
        vSize = "score",  #This is your quantitative variable
        type = "index", #Type sets the organization and color scheme of your treemap
        palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="anime analysis", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)


treemap(dat,index = c("Type"),vSize ="score",palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="anime analysis")

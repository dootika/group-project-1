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


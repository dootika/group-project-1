#install.packages("stringr")

library(rvest)
library(dplyr)
library(stringr)
load("Anime_ranking.Rdata")
View(data)
dat <- data

# Unique entries in each column
for(i in 1:28)
{
  print(paste(colnames(dat)[[i]],length(unique(dat[[i]])),sum(is.na(dat[[i]]))))
}


# Extracting numeric variables

View(dat)
dat$score <- as.numeric(substr(dat$Score,1,4))
dat$score <- as.numeric(dat$score)
dat <- dat[,!grepl("Score",names(dat))]

dat$popularity <- as.numeric(substr(data$Popularity,2,8))
dat <- dat[,!grepl("Popularity",names(dat))]

dat$Rank <- seq(1,dim(dat)[1])
dat <- dat[,!grepl("Ranked",names(dat))]
dat$Episode <- as.numeric(dat$Episodes)
dat <- dat[,!grepl("Episodes",names(dat))]


# function for cleaning duration column


dur <- function(dam)
{
  if((grepl("min. per ep.",dam, fixed = TRUE) == TRUE) | (grepl("min.",dam, fixed = TRUE) == TRUE))
  {
    ret <- as.numeric(substr(dam,1,2))
  }
  if(grepl("hr.",dam, fixed = TRUE) == TRUE)
  {
    a <- as.numeric(substr(dam,1,2))
    a <- a*60
    b <- as.numeric(substr(dam,7,9))
    ret <- a+b
  }
  return(ret)
}

dur(dat$Duration[8])

duration <- numeric(5000)
for(i in 1:5000)
{
  duration[i] <- dur(dat$Duration[i])
}
dat$duration <- duration
dat <- dat[,!grepl("Duration",names(dat))]

# NAN value check
sum(is.na(dat$Broadcast))
table(dat$Type)
sum(table(dat$Premiered))
View(table(dat$Studios))




# Broadcast cleanup
day <- function(vec)
{
  if(is.na(vec) == TRUE)
  {
    ret <- NA
  }
  if(sum(grepl("Not scheduled once per week",vec, fixed = TRUE)) >= 1)
  {
    ret <- NA
  }
  dy <- c("Sundays","Mondays","Tuesdays","Wednesdays","Thursdays","Fridays","Saturdays")
  for(i in 1:7)
  {
    if(sum(grepl(dy[i],vec, fixed = TRUE)) >= 1)
    {
      ret <- dy[i]
    }
  }
  return(ret)
}

datee <- numeric(dim(dat)[1])
for(i in 1:dim(dat)[1])
{
  datee[i] <- day(dat$Broadcast[i])
}
dat$broadcast <- datee
dat <- dat[,!grepl("Broadcast",names(dat))]



# Premiered cleanup
# 1. Year column formation
year <- function(vec)
{
  var <- as.numeric(substr(vec,str_length(vec)-4,str_length(vec)))
  return(var)
}

yearr <- numeric(dim(dat)[1])
for(i in 1:dim(dat)[1])
{
  yearr[i] <- year(dat$Premiered[i])
}
dat$Year <- yearr

# 2. Spring/Fall column formation

sp_fa <- function(vec)
{
  if(is.na(vec) == TRUE)
  {
    ret <- NA
  }
  dy <- c("Spring", "Fall", "Summer", "Winter")
  for(i in 1:4)
  {
    if(sum(grepl(dy[i],vec, fixed = TRUE)) >= 1)
    {
      ret <- dy[i]
    }
  }
  return(ret)
}


spr_fall <- numeric(dim(dat)[1])
for(i in 1:dim(dat)[1])
{
  spr_fall[i] <- sp_fa(dat$Premiered[i])
}
dat$season <- spr_fall

# Length of title

name_len <- numeric(dim(dat)[1])
for(i in 1:dim(dat)[1])
{
  name_len[i] <- str_length(dat$Name[i])
}
dat$Name_length <- name_len


# SCORE column building


ind9 <- (dat$score<10)  & (dat$score>9)
ind8 <- (dat$score<9)  & (dat$score>8)
ind7 <- (dat$score<8)  & (dat$score>7)
ind6 <- (dat$score<7)  & (dat$score>6)

vec <- c()
for(i in 1:dim(dat)[1])
{
  vec[ind9] <- "9+"
  vec[ind8] <- "8+"
  vec[ind7] <- "7+"
  vec[ind6] <- "6+" 
}
dat$SCORE <- vec

# Demographic cleaning
a <- dat$Demographic
dat$demographic <- substr(a,1,str_length(dat$Demographic)/2)
dat <- dat[,!grepl("Demographic",names(dat))]

# Favorites numeric column conversion
dat$Favorites <- as.numeric(dat$Favorites)

# Year a catagory
dat$Year <- as.numeric(dat$Year)
min_y <- min(dat$Year, na.rm = TRUE)
max_y <- max(dat$Year, na.rm = TRUE)
se <- seq(min_y, max_y, 10)

ind1 <- (dat$Year<se[2])  & (dat$Year >= se[1])
ind2 <- (dat$Year<se[3])  & (dat$Year >= se[2])
ind3 <- (dat$Year<se[4])  & (dat$Year >= se[3])
ind4 <- (dat$Year<se[5])  & (dat$Year >= se[4])
ind5 <- (dat$Year<se[6])  & (dat$Year >= se[5])
ind6 <- (dat$Year >= se[6])

vec1 <- c()
for(i in 1:dim(dat)[1])
{
  vec1[ind1] <- "1963 - 1973"
  vec1[ind2] <- "1974 - 1983"
  vec1[ind3] <- "1984 - 1993"
  vec1[ind4] <- "1994 - 2003" 
  vec1[ind5] <- "2004 - 2013"
  vec1[ind6] <- "2014 - 2022"
  
  
}
vec1
dat$YEAR <- vec1

# Members to numeric
dat$Members <- as.numeric(dat$Members)




# 2. Demographic column formation

demo <- function(vec)
{
  if(is.na(vec) == TRUE)
  {
    ret <- NA
  }
  dy <- c("Shounen", "Shoujo", "Seinen", "Josei", "kids")
  for(i in 1:length(dy))
  {
    if(sum(grepl(dy[i],vec, fixed = TRUE)) >= 1)
    {
      ret <- dy[i]
    }
  }
  return(ret)
}

demm <- numeric(dim(dat)[1])
for(i in 1:dim(dat)[1])
{
  demm[i] <- demo(dat$demographic[i])
}
demm
vec <- c()
for(i in 1:5000)
{
  if(is.na(dat$demographic[i]) == FALSE)
  {
    if(dat$demographic[i] == "Shounen")
    {
      vec[i] <- "Boys(12-18yr)"
    }
    
    else if(dat$demographic[i] == "Shoujo")
    {
      vec[i] <- "Girls(12-18yr)"
    }
    else if(dat$demographic[i] == "Seinen")
    {
      vec[i] <- "Men(18-40yr)"
    }
    else if(dat$demographic[i] == "Josei")
    {
      vec[i] <- "Women(18-40yr)"
    }
    else if(dat$demographic[i] == "Kids")
    {
      vec[i] <- "Kids"
    }
    else
    {
      vec[i] <- NA
    } 
  }
  
}
vec <- c(vec,NA)
dat$demographic <- vec


# GENRE column divided into 
load("data1.RData")
genre_pop <- c("Love", "Sports", "Suspense", "Life" ,"Win" ,
               "Comedy", "Adventure","Gourmet" ,"Garde" , "Mystery", 
               "Fantasy", "Supernatural", "Romance", "Drama", 
               "Horror", "Sci-Fi", "Ecchi", "Action")
romance <- c("Love", "Romance", "Ecchi")
mystery <- c("Suspense", "Mystery","Fanstasy", "Garde", "Si-Fi")
action <- c("Action","Adventure", "Life", "Win", "Sports")
horror <- c("Horror", "Supernatural")
comedy <- c("Comedy","Life", "Drama")
others <- c("Garde")



identifi <- function(vec)
{
  v1 <- c()
  for(i in 1:dim(dat)[1])
  {
    if(sum(vec %in% dat$Final_Genres[[i]]) > 0 )
    {
      v1[i] <- 1
    }
    else
    {
      v1[i] <- 0
    }
  }
  return(v1)
}

dat$Mystery <- identifi(mystery)
dat$Romance <- identifi(romance)
dat$Action <- identifi(action)
dat$Horror <- identifi(horror)
dat$Comedy <- identifi(comedy)
dat$Others <- identifi(others)




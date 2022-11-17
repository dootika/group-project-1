library(rvest)
library(dplyr)
library(stringr)
load("Anime_ranking.Rdata")
View(data)
dat <- data

# Unique entries in each column
for(i in 1:21)
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

dat$Rank <- seq(1,5000)
dat <- dat[,!grepl("Ranked",names(dat))]
dat$Episode <- as.numeric(dat$Episodes)
dat <- dat[,!grepl("Episodes",names(dat))]


# function for cleaning duration column
dur <- function(dam)
{
  if(grepl("min. per ep.",dam, fixed = TRUE) == TRUE)
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

duration <- numeric(5000)
for(i in 1:5000)
{
  duration[i] <- dur(dat$Duration[i])
}
dat$duration <- duration
dat <- dat[,!grepl("Duration",names(dat))]

# NAN value check
sum(is.na(dat$broadcast))

table(dat$Type)
sum(table(dat$Premiered))
View(table(dat$Studios))




# Broadcast cleanup
day <- function(vec)
{
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
fin_dat <- function(vec)
{
  if(day(vec) == 110)
  {
    retu <- NA
  }
  else
  {
    retu <- day(vec)
  }
  return(retu)
}


datee <- numeric(5000)
for(i in 1:5000)
{
  datee[i] <- fin_dat(dat$Broadcast[i])
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

yearr <- numeric(5000)
for(i in 1:5000)
{
  yearr[i] <- year(dat$Premiered[i])
}
fin_year(dat$Premiered[1])
dat$Year <- yearr

# 2. Spring/Fall column formation

sp_fa <- function(vec)
{
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

fin_sp_fa <- function(vec)
{
  if(sp_fa(vec) == 110)
  {
    retu <- NA
  }
  else
  {
    retu <- sp_fa(vec)
  }
  return(retu)
}


spr_fall <- numeric(5000)
for(i in 1:5000)
{
  spr_fall[i] <- fin_sp_fa(dat$Premiered[i])
}
dat$season <- spr_fall

# Length of title

name_len <- numeric(5000)
for(i in 1:5000)
{
  name_len[i] <- str_length(dat$Name[i])
}
dat$Name_length <- name_len


# SCORE column building
load("data1.RData")

ind9 <- (dat$score<10)  & (dat$score>9)
ind8 <- (dat$score<9)  & (dat$score>8)
ind7 <- (dat$score<8)  & (dat$score>7)
ind6 <- (dat$score<7)  & (dat$score>6)

vec <- c()
for(i in 1:5000)
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
for(i in 1:5000)
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







library(rvest)
library(tidyverse)


number_of_movies <- 5000

# Codes for max no. of Anime
codes <- function(max)
{
  page <- seq(0,max,50)
  codess <- c()
  for(i in 1:length(page))
  {
    html <- read_html(paste("https://myanimelist.net/topanime.php?limit=",page[i],sep = ""))
    code <- html %>% html_elements(".hoverinfo") %>% html_attr("id")
    code <- as.numeric(gsub("info","",code))
    codess <- append(codess,code)
  }
  return(codess)
}
code <- codes(number_of_movies)

# DataFrame and Columns in DataFrame
columns <- c("Type","Episodes","Status","Aired",
             "Premiered", "Broadcast", "Producers",
             "Licensors", "Studios", "Source", "Genres",
             "Demographic", "Duration","Rating", "Score",
             "Ranked", "Popularity", "Members", "Favorites","Image_links")
data <- data.frame(matrix(nrow = 0, ncol = length(columns)+1))
colnames(data) <- c("Name",columns)
View(data)

# Values in Rows according to page function
values <- function(info)
{
  value <- c()
  for(i in 1:length(columns))
  {
    index <- grepl(columns[i],info, fixed = TRUE)
    if(sum(index) > 0)
    {
      value[i] <- gsub("\n","",gsub("  ","",gsub(",","",gsub(paste(columns[i],":",sep = ""),"", info[index])))) 
    }
    else
    {
      value[i] <- NA
    }
    
  }
  return(value)
}



# Allocation
num <- number_of_movies
for(i in 1:num)
{
  html <- read_html(paste("https://myanimelist.net/anime/",code[i],sep =""))
  a <- html %>% html_elements(".spaceit_pad") %>% html_text()
  data[i,2:(length(columns)+1)] <- values(a)
  data[i,1] <- html %>% html_elements(".title-name.h1_bold_none") %>% html_text()
  b <- html %>% html_elements(".lazyload") %>% html_attr("data-src")
  data[i,21] <- b[1]
  print(i)
  
}


save(data,file = "Anime_ranking.Rdata")




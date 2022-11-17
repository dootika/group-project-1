library(rvest)

html <- read_html("https://www.fortuneindia.com/fortune-500/company-listing/?year=2021&page=1&query=&per_page=500")
dat<- html %>% html_table()


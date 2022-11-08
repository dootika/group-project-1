library(ggplot2)
library(imager)
library(rvest)

load("data1.RData")
View(dat)



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


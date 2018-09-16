library(RCurl)  # not sure I still need this?
library(readtext)
library("zoo")
library(dplyr)
library(RCurl) ; options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


rm(list = ls())




setwd("C:/Users/Brigitte/Documents/Hurricanes")

# Time range of text data
years <- c(2004:2017)

for(curr_year in years) {
  
  # first get list of URLS to download
  list_of_urls <- paste("https://www.nhc.noaa.gov/archive/text/TWOAT/",as.character(curr_year),"/",sep="")
  
  html <- paste(readLines(list_of_urls), collapse="\n")
  library(stringr)
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]

  print(head(links)) # to check things out

  Sys.sleep(30) # because I was getting some errors
    
    # download each text file
  for (i in (6:length(links))) {
    url <- paste("https://www.nhc.noaa.gov/archive/text/TWOAT/",curr_year,"/",links[i],sep="")
    download.file(url, destfile=paste(links[i],"txt",sep="."))  # saving to working directory
  }
}




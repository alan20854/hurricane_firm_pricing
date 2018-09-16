# Written by undergrad research assistant Alan Yan

library(stringr)
library(rvest)
library(stringi)
library(RCurl)
library(RTidyHTML)
library(XML)
library(RDCOMClient) 

setwd("C:/Users/Alan/Dropbox/Yan/1_data/hurricanes/ForecastAdvisories/raw")
years <- c(1998:2018)
working_years <- c(1998:2018)
greek_names <- c("ALPHA.html", "BETA.html", "GAMMA.html", "DELTA.html", "EPSILON.html", 
                 "ZETA.html", "ETA.html", "THETA.html", "IOTA.html", "KAPPA.html", 
                 "LAMBDA.html", "MU.html", "NU.html", "XI.html", "OMICRON.html",
                 "PI.html", "RHO.html", "SIGMA.html", "TAU.html", "UPSILON.html", 
                 "PHI.html", "CHI.html", "PSI.html", "OMEGA.html")
number_names <- c("ONE.html", "TWO.html", "THREE.html", "FOUR.html", "FIVE.html", 
                  "SIX.html", "SEVEN.html", "EIGHT.html", "NINE.html", "TEN.html", 
                  "ELEVEN.html", "TWELVE.html", "THIRTEEN.html", "FOURTEEN.html", "FIFTEEN.html", 
                  "SIXTEEN.html", "SEVENTEEN.html", "EIGHTTEEN.html", "NINETEEN.html", "TWENTY.html", 
                  "TWENTY-ONE.html", "TWENTY-TWO.html", "TWENTY-THREE.html", "TWENTY-FOUR.html", "TWENTY-FIVE.html", 
                  "TWENTY-SIX.html", "TWENTY-SEVEN.html", "TWENTY-EIGHT.html", "TWENTY-NINE.html", "THIRTY.html")

# Included External Function to convert HTML to text

# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}

############################################################
#Start of code to download raw forecast advisory text files#
############################################################
for(year in working_years) {
  print(year)
  if (year == 1998) {
    year_url <- "https://www.nhc.noaa.gov/archive/1998/1998archive.shtml"
  } else {
    year_url <- paste("https://www.nhc.noaa.gov/archive/", year, "/", sep = "")
  }
  
  html <- paste(readLines(year_url), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  if(year == 1998) {
    storm_links <- links[grepl("^1.*html", links)]
  } else {
    storm_links <- links[grepl("^[A-Z].*html", links)]
  }
  
  storm_counter <- 0
  a_letter_name_appeared <- FALSE
  for(storm_link in storm_links) {
    first_char <- substr(storm_link, 1, 1)
    if((a_letter_name_appeared && storm_link != "ALPHA.html" && first_char == "A") || 
       (grepl("-E", storm_link) && (!storm_link %in% number_names))) {
      break
    }
    if(!a_letter_name_appeared) {
      if(first_char == "A") {
        a_letter_name_appeared <- TRUE
        storm_counter <- storm_counter + 1
      } else {
        storm_counter <- storm_counter + 1
      }
    } else {
      storm_counter <- storm_counter + 1
    }
  }
  atlantic_storm_links <- storm_links[c(1:storm_counter)]
  
  for(atlantic_storm_link in atlantic_storm_links) {
    storm_html_link <- paste("https://www.nhc.noaa.gov/archive/",year,"/",atlantic_storm_link,sep = "")
    storm_page <- read_html(storm_html_link)
    all_links <- storm_page %>% 
      html_nodes("a") %>% 
      html_attr('href')
    if (year <= 2005) {
      forecast_links <- all_links[grepl("/mar/", all_links)] #works for 1998 - 2005
    } else {
      forecast_links <- all_links[grepl("fstadv", all_links)]
    }
    
    print(forecast_links)
    for (forecast_adv_link in forecast_links) {
      if(year == 1998) {
        forecast_url <- paste("https://www.nhc.noaa.gov/archive/1998/", forecast_adv_link, sep = "")
      } else {
        forecast_url <- paste("https://www.nhc.noaa.gov", forecast_adv_link, sep = "")
      }
      
      txt <- htmlToText(forecast_url)
      txt <- gsub(".*ZCZC", "ZCZC", txt)
      txt <- gsub("NNNN.*", "NNNN", txt)
      fileName <- sub(".*/", "", forecast_url)
      if (year != 1998) {
        fileName <- sub('[.][^.]+$', '', fileName)
      }
      fileName <- gsub('\\.', '-', fileName)
      print(fileName)
      fileLoc <- paste("C:/Users/Alan/Dropbox/Yan/1_data/hurricanes/ForecastAdvisories/raw", 
                       year, "/", fileName, ".txt", sep = "")
      file.create(fileLoc)
      fileConn<-file(fileLoc)
      writeLines(txt, fileConn)
      close(fileConn)
    }
  }
}

# Written by undergrad research assistant Alan Yan

library(stringr)
library(rvest)
library(stringi)
library(RCurl)
library(RTidyHTML)
library(XML)
library(RDCOMClient) 

main_dir <- "C:/Users/Alan/Documents/Hurricanes"
setwd(main_dir)
years <- c(1998:2018)
greek_names <- c("ALPHA.shtml?", "BETA.shtml?", "GAMMA.shtml?", "DELTA.shtml?", "EPSILON.shtml?", 
                 "ZETA.shtml?", "ETA.shtml?", "THETA.shtml?", "IOTA.shtml?", "KAPPA.shtml?", 
                 "LAMBDA.shtml?", "MU.shtml?", "NU.shtml?", "XI.shtml?", "OMICRON.shtml?",
                 "PI.shtml?", "RHO.shtml?", "SIGMA.shtml?", "TAU.shtml?", "UPSILON.shtml?", 
                 "PHI.shtml?", "CHI.shtml?", "PSI.shtml?", "OMEGA.shtml?")
number_names <- c("ONE.html", "TWO.html", "THREE.html", "FOUR.html", "FIVE.html", 
                  "SIX.html", "SEVEN.html", "EIGHT.html", "NINE.html", "TEN.html", 
                  "ELEVEN.html", "TWELVE.html", "THIRTEEN.html", "FOURTEEN.html", "FIFTEEN.html", 
                  "SIXTEEN.html", "SEVENTEEN.html", "EIGHTTEEN.html", "NINETEEN.html", "TWENTY.html", 
                  "TWENTY-ONE.html", "TWENTY-TWO.html", "TWENTY-THREE.html", "TWENTY-FOUR.html", "TWENTY-FIVE.html", 
                  "TWENTY-SIX.html", "TWENTY-SEVEN.html", "TWENTY-EIGHT.html", "TWENTY-NINE.html", "THIRTY.html",
                  "ONE.shtml?", "TWO.shtml?", "THREE.shtml?", "FOUR.shtml?", "FIVE.shtml?", 
                  "SIX.shtml?", "SEVEN.shtml?", "EIGHT.shtml?", "NINE.shtml?", "TEN.shtml?", 
                  "ELEVEN.shtml?", "TWELVE.shtml?", "THIRTEEN.shtml?", "FOURTEEN.shtml?", "FIFTEEN.shtml?", 
                  "SIXTEEN.shtml?", "SEVENTEEN.shtml?", "EIGHTTEEN.shtml?", "NINETEEN.shtml?", "TWENTY.shtml?", 
                  "TWENTY-ONE.shtml?", "TWENTY-TWO.shtml?", "TWENTY-THREE.shtml?", "TWENTY-FOUR.shtml?", "TWENTY-FIVE.shtml?", 
                  "TWENTY-SIX.shtml?", "TWENTY-SEVEN.shtml?", "TWENTY-EIGHT.shtml?", "TWENTY-NINE.shtml?", "THIRTY.shtml?")
storm_folder_names <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010",
                        "011", "012", "013", "014", "015", "016", "017", "018", "019", "020",
                        "021", "022", "023", "024", "025", "026", "027", "028", "029", "030",
                        "031", "032", "033", "034", "035", "036", "037", "038", "039", "040",
                        "041", "042", "043", "044", "045", "046", "047", "048", "049", "050",
                        "051", "052", "053", "054", "055", "056", "057", "058", "059", "060",
                        "061", "062", "063", "064", "065", "066", "067", "068", "069", "070",
                        "071", "072", "073", "074", "075", "076", "077", "078", "079", "080",
                        "081", "082", "083", "084", "085", "086", "087", "088", "089", "090",
                        "091", "092", "093", "094", "095", "096", "097", "098", "099", "100")
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
for(year in years) {
  print(year)
  
  sub_dir <- paste("/", year, sep = "")
  year_dir <- file.path(main_dir, sub_dir)
  if (!dir.exists(year_dir)){
    dir.create(year_dir)
  }
  
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
    if (year == 1998 && storm_link == "1998AGATHAadv.html") {
      break;
    }
    if((a_letter_name_appeared && storm_link != "ALPHA.shtml?" && first_char == "A") || 
       (grepl("-E", storm_link) && (!storm_link %in% number_names)) || 
       (grepl("MEP", storm_link))) {
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
  
  storm_in_year_counter <- 1
  for(atlantic_storm_link in atlantic_storm_links) {
    storm_sub_dir <- paste("/", storm_in_year_counter, sep = "")
    storm_dir <- file.path(year_dir, storm_sub_dir)
    if (!dir.exists(storm_dir)) {
      dir.create(storm_dir)
    }
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
      fileLoc <- paste(storm_dir, "/", fileName, ".txt", sep = "")
      file.create(fileLoc)
      fileConn<-file(fileLoc)
      writeLines(txt, fileConn)
      close(fileConn)
    }
    storm_in_year_counter <- storm_in_year_counter + 1
  }
}

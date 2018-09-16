#' ---	
#' title: "R Notebook"	
#' ---	
#' #################################################################	
#' AUTHOR INFO: Written by David Rubio, 2018 Summer Intern. 	
#' 	
#' BACKGROUND: NOAA releases weather forecasts through its National Hurricane Center (NHC) that contain tables of wind-speed probabilities by location whenever it detects a storm forming. They are formatted as text files and can be downloaded online from NOAA's website (https://www.nhc.noaa.gov/archive/text/). The files used here are the PWSEP and PWSAT text files that range from 2007-2017. We wish to use wind speed probabilities as a measure of ex-ante storm intensity/risk exposure and would like to have a data frame that contains all of the individual tables from each file combined. 	
#' 	
#' PURPOSE OF THIS FILE: This file takes all of the NOAA forecast text files and recreates only the wind speed probability tables as a data frame. It does this by truncating unnecessary text from each file, reconstructing the table, and combining all the tables from each file together into one large table. The bottom chunk of this markdown file also contains sanity checks to ensure that the combined wind speed probability table is correctly constructed and that data from text files was properly read. 	
#' 	
#' IMPORTANT NOTES: (1) The truncations of excess text occur at points in the file that are common to all files and are detected using regular expressions and stringr functions. However, one text file (PWSEP2.201609180231.txt.txt) appears to be incomplete (NOAA's fault, not ours) and is omitted from the data frame. (2) Some of the text files appear to be inexplicably repeated within themselves (i.e. the entirety of the forecast text in the file appears twice). This is a problem with the raw files themselves (as verified by re-downloading) and not the code. (3) The combined wind speed table filters out international locations and locations that are coordinates/buoys/etc. with no associated US state. Including these locations in the future would require exporting a new "PWSAT_City_State.csv" file that includes these locations, adding missing states by hand, and merging the new completed file; thus, this is NOT recommended and this file is intended only for US locations. See the code for more notes on this.	
#' #################################################################	
#' 	
#' Package Load	
#' 	
library(dplyr)	
library(stringr)	
library(lubridate)	
library(tidyr)	
library(tibble)	
#' 	
#' 	
#' Clear Environment	
#' 	
rm(list = ls())	
#' 	
#' 	
#' 	
#' Data Initialization	
#' 	
mypath = "~/mcr/home/m1bxr01/Hurricanes/data/raw/PWSAT_PWSEP/" #David	
mycleanpath = "~/mcr/home/m1bxr01/Hurricanes/data/clean/" #David	
censuspath = "~/Hurricanes/"	
#mypath = "~/drives/mcr/home/m1bxr01/Hurricanes/data/raw/PWSAT_PWSEP/" #Brigitte	
#mycleanpath = "~/drives/mcr/home/m1bxr01/Hurricanes/data/clean/" #Brigitte	
#censuspath = "~/drives/mcr/home/m1dnr00/Hurricanes/" #Brigitte	
	
#Retrieves vector of filenames for all NOAA text files	
filevector = list.files(path = mypath, pattern = "*")	
	
#Removes "PWSEP2.201609180231.txt.txt" from filevector for reasons described in the introductino.	
filevector = filevector[which(filevector!="PWSEP2.201609180231.txt.txt")] 	
#' 	
#' 	
#' 	
#' Recreates wind speed probability table from PWSAT/PWSEP text files as data frames. Truncates unnecesary text and preserves just the tables.	
#' 	
setwd(mypath)	
combined_dataframe = data.frame(matrix(ncol = 11, nrow = 0)) #Blank data frame that will hold combined data	
names(combined_dataframe) = c("filename", "location", "KT", "12", "24", "36", "48", "72", "96", "120", "time")	
	
	
#Reads each text file in as a data frame and gives it the same dimensions/names as combined_dataframe.	
for(i in 1:length(filevector)) {	
  temp_frame = read.delim(file = filevector[i], header = F, colClasses = "character")	
  temp_frame[ ,2:11] = "" #Add blank columns	
  names(temp_frame) = names(combined_dataframe) #Make names equivalent to blank data frame to allow for row binding	
  time = str_extract(string = temp_frame[ ,1], pattern = "[[:digit:]]{4,} UTC") #Extracts publication time of the file	
  temp_frame[ ,11] = time[!is.na(time)] #Adds time to each row	
	
  	
#Truncate text files to include only data relevant to wind speed probabilty	
#NOTE: In most cases, start_truncation has only one match and is a single value. However, 2 values may appear due to text files that have inexplicably repeated contents which causes start_truncation to sometimes be a vector. The first value is always used, even though both values would be equivalent. The same is true for end_truncation.	
  start_truncation = grep(x = temp_frame[ ,1], pattern = "LOCATION *KT") #Find line where wind table begins	
  temp_frame = temp_frame[-(1:start_truncation[1]), ] #Truncate all text before this line	
  end_truncation = grep(temp_frame[ ,1], pattern = "[-]+ WIND SPEED PROBABILITIES FOR FORECAST|[$]+|[.]+THERE") #Find line where table ends	
  temp_frame = temp_frame[-(end_truncation[1]:length(temp_frame[ ,1])), ] #Truncate all text after this line	
  	
  	
#Removes blank lines from the data frame. These occur because there are blank lines between each row in the text.	
  non_blank_lines = grep(x = temp_frame[ ,1], pattern = "[[:alnum:]]") #Finds blank lines	
  temp_frame = temp_frame[non_blank_lines, ] #Removes blank lines	
  	
  	
#Drop files that have no data after truncation (i.e. the forecast in that file had no windspeed table)	
  if(nrow(temp_frame) >0) {	
	
    	
#Finds location name in each row and makes a column containing this location name	
  for(j in 1:length(temp_frame[ ,1])) {   	
    testword = temp_frame[j, 1] #testword is the cell in the first column. This cell contains all of the line's text in one string 	
    testword = gsub(x = testword, pattern = "[[:space:]]+", replacement = " ") #Removes excess white space from each line	
    testword = gsub(x = testword, pattern = "\\( *[[:alnum:]]*\\)", replacement = "") #Removes parentheses and their contents from table	
    testword = str_replace_all(string = testword, pattern = "\\bX\\b", replacement = "0") #Replaces X's with 0's	
    testword = unlist(strsplit(x = testword, split = " ")) #Splits large string into character vector that will fill columns	
    	
    	
#Returns indices within the character vector that contain the location name. The additional "or" logical is added to capture some exceptions to the typical location format since there are a small number of locations of the form "Buoy #####". 	
    location_indices= grep(x = testword, pattern = "[A-Z]|[[:digit:]]{4, }") #Find location name (only alphabetical text in the character vector)	
    location_name = paste(testword[location_indices], collapse = " ") #Vector that holds location names	
    temp_frame[j, 2] = location_name #Adds location name to a column	
    	
#Adds wind speed probabilities to columns	
    testword = testword[-(1:length(location_indices))] #All parts of testword that don't contain the location contain probabilities	
    temp_frame[j, 3:(length(testword) + 2)] = testword #Adds probabilities to columns	
	
	
  }	
    temp_frame[ ,1] = filevector[i]   #Add column that contains filename	
    temp_frame = temp_frame %>% mutate_at(funs(as.integer), .vars = 4:10) #Converts wind speed probabilties to integers to reduce data frame size	
    combined_dataframe = rbind(combined_dataframe, temp_frame) #Appends data frame from a single file to combined_dataframe	
	
  }	
}	
	
#Removes locations that contain numbers in their names (e.g. 120N 150W, Buoy 50001, etc.). Can be commented out as needed.	
combined_dataframe = filter(.data = combined_dataframe, !str_detect(string = location, pattern = "[[:digit:]]+"))	
	
	
#Adds date to each row of the data frame	
combined_dataframe = mutate(combined_dataframe, date = ymd(substr(x = filename, start = 8, stop = 15)))	
	
	
#Adds state to each row of the data frame. NOTE: The file "PWSAT_City_State.csv" was created in a chunk later on in this file. It contains the locations listed in the windspeed data with their associated states. When these states were not explicitly included in the windspeed files, they were determined through Google searches and manually added. 	
combined_dataframe = combined_dataframe %>% 	
  inner_join(y = read.csv(paste(mycleanpath, "PWSAT_City_State.csv", sep = ""), colClasses = "character", stringsAsFactors = F), by = "location") 	
  	
	
#Adds storm names to each row of the data frame. The .csv read here contains the named hurricanes mentioned in every NOAA text file , including the stage it was in. Note that each file only refers to one storm. The .csv file is one of the outputs of the code located in "m1dnr00/Hurricanes/download_hurricane_advisory_time_frames_locations_v2David.r". This code is the 2nd version of code created by Brigitte Roth Tran.	
combined_dataframe = read.csv(paste0(mycleanpath, "full_long_states_storms_dates_filenames_PWSAT_PWSEP.csv"), colClasses = "character")[ ,-1] %>% 	
  filter(str_detect(string = stormname, pattern = "SUBTROPICAL|TROPICAL|DEPRESSION|HURRICANE")) %>% 	
  select(-c(date, state)) %>%	
  unique() %>% 	
  right_join(combined_dataframe, by = "filename") %>%	
  filter(!is.na(stormname)) 	
#Files with NA stormname entries referenced: (1) storms in the pacific (we aren't interested in these) or (2) numbered storms without names	
	
#Removes state abbreviation from location name	
combined_dataframe = combined_dataframe %>% mutate(location = str_replace_all(string = location, pattern = paste(" +", state, "\\b", sep = ""),  replacement = ""))	
	
	
#The objective of the following code is to add as much county/state identifying info (fips, state abbreviation, state name, county name) as possible to the windspeed data using joins with preexisting data sets, and then manually changing the names of mismatched locations in the windspeed data so that they join properly.  	
	
#The file "census_places_cleaned.csv" is Census data (provided by Shifrah Aron-Dine) that lists (almost) every city in the US with its county and FIPS code. 	
census_data = read.csv(paste(censuspath, "census_places_cleaned.csv", sep = ""), stringsAsFactors = F) %>%	
  select(name, places_county) %>%	
  inner_join(maps::county.fips, by = c("places_county" = "fips")) %>%  #Inner join removes all NAs (i.e. Puerto Rico data)	
  mutate(State.Name = toupper(str_split_fixed(string = polyname, pattern = ",", n = Inf)[ ,1]), #Creates a column of state names	
         County.Name = toupper(str_split_fixed(string = polyname, pattern = ",", n = Inf)[ ,2]), #Creates a column of county names	
         City.Name = toupper(str_split_fixed(string = name, pattern = ",", n = Inf)[ ,1]),#Creates a column of city names	
#Removes location descriptions because they prevent merging. NOTE: This creates exceptions that must be corrected for further joins to work.	
         City.Name = str_replace_all(string = City.Name, pattern = " CITY| CDP| VILLAGE| TOWN| BOROUGH", replacement = "")) %>% 	
  select(City.Name, State.Name, County.Name, places_county) %>%	
#Adds a column of state abbreviations for joining. Data frame used for joining is constructed from vectors provided through R datasets	
  inner_join(y = data.frame(State.Name = toupper(datasets::state.name), State.Abb = datasets::state.abb, stringsAsFactors = F), by = "State.Name") %>%	
  rbind(c("WASHINGTON", "DISTRICT OF COLUMBIA", "DISTRICT OF COLUMBIA", "11001", "DC")) %>% #Adds a row for Washington DC 	
#Remove states that don't appear in wind speed tables	
  filter(State.Abb %in% unique(combined_dataframe$state)) %>%	
#This adds rows for independent cities in Virginia that are listed as locations in the wind speed table but excluded from the census data. Their information is manually added using this FIPS data from the NRCS: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697	
  rbind(data.frame(City.Name = c("RICHMOND", "DANVILLE", "CHARLOTTESVILLE"),	
                   State.Abb = c("VA", "VA", "VA"),	
                   State.Name = c("VIRGINIA","VIRGINIA", "VIRGINIA"),	
                   County.Name = c("RICHMOND", "DANVILLE", "CHARLOTTESVILLE"),	
                   places_county = c("51159", "51590", "51540"))) #FIPS codes for each city	
                   	
  	
merged_dataframe = combined_dataframe %>%	
#The following case_when statement individually changes all the non-matching locations in the wind speed table so that they reflect the corresponding cities in the census data. Whenever the city referenced in the wind speed table is unavailable in the census, I choose the closest nearby city that appears in the census data.	
  mutate(state = ifelse(location == "STENNIS", yes = "MS", no = state), 	
    location = case_when(	
    location == "MOREHEAD CITY" ~ "MOREHEAD",	
    location == "AUGUSTA" & state == "GA" ~ "AUGUSTA-RICHMOND COUNTY CONSOLIDATED GOVERNMENT (BALANCE)", 	
    location == "FT PIERCE" ~ "FORT PIERCE",	
    location == "W PALM BEACH" ~ "WEST PALM BEACH",	
    location == "FT MYERS" ~ "FORT MYERS",	
    location == "ST MARKS" ~ "ST. MARKS", 	
    location == "PANAMA CITY" ~ "PANAMA",	
    location == "PORT O CONNOR" ~ "PORT O'CONNOR", 	
    location == "CAPE HATTERAS" ~ "HATTERAS",	
    location == "HYANNIS" ~ "BARNSTABLE",	
    location == "MONTAUK POINT" ~ "MONTAUK",	
    location == "NEW YORK CITY" ~ "NEW YORK",	
    location == "OCEAN CITY" ~ "OCEAN PINES",	
    location == "ATLANTIC CITY" ~ "ATLANTIC",	
    location == "NORFOLK NAS" ~ "NORFOLK",	
    location == "WALLOPS CDA" ~ "WATTSVILLE",	
    location == "BEAUFORT MCAS" ~ "BEAUFORT",	
    location == "KINGS BAY" ~ "KINGS BAY BASE",	
    location == "MAYPORT NS" ~ "JACKSONVILLE",	
    location == "ROCKY MT" ~ "ROCKY MOUNT",	
    location == "CHERRY PT" ~ "HAVELOCK",	
    location == "NEW RIVER" ~ "JACKSONVILLE",	
    location == "SURF CITY" ~ "SURF",	
    location == "BALD HEAD ISL" ~ "BALD HEAD ISLAND",	
    location == "THE VILLAGES" ~ "LADY LAKE",	
    location == "WHITING FLD" ~ "MILTON",	
    location == "OCEANA NAS" ~ "VIRGINIA BEACH",	
    location == "ELIZABETH CTY" ~ "ELIZABETH",	
    location == "PAX RIVER NAS" ~ "LEXINGTON PARK",	
    location == "CHARLOTTESVIL" ~ "CHARLOTTESVILLE", 	
    location == "FT LAUDERDALE" ~ "FORT LAUDERDALE",	
    location == "HOMESTEAD ARB" ~ "HOMESTEAD BASE",	
    location == "CAPE HENLOPEN" ~ "LEWES",	
    location == "STENNIS" ~ "PEARLINGTON",	
    location == "NYC JFK AIRPRT" ~ "NEW YORK",	
    location == "NYC CNTRL PARK" ~ "NEW YORK",	
    location == "NWS EARLE" ~ "TINTON FALLS",	
    location == "MORGAN CITY" ~ "MORGAN",	
    location == "FORT POLK" ~ "FORT POLK NORTH",	
    location == "HIGH ISLAND" ~ "ANAHUAC",	
    TRUE ~ location)) %>%	
  left_join(census_data, by = c("location" = "City.Name", "state" = "State.Abb")) %>% #Adds State,City, and County names with FIPS.	
  rename(City.Name = location,	
         State.Abb = state,	
         fips = places_county) 	
	
	
#Reshapes wind speed tables in a wide format so that the variable "KT" (which describes the wind speed that the probabilities are referring to) is represented in the column names. 	
merged_dataframe = merged_dataframe %>% 	
  reshape(idvar = c("filename", "City.Name"), timevar = "KT", v.names = c("12", "24", "36", "48", "72", "96", "120"), direction = "wide") %>% 	
  replace(is.na(.), 0) #Replaces NA values with 0	
	
	
#Change names of reshaped variables. "Horizon" indicates the number of hours after the forecast. "Speed" is the wind speed being estimated/predicted.	
names(merged_dataframe) = str_replace_all(string = names(merged_dataframe), pattern = "([[:digit:]]+).([[:digit:]]+)", replacement = "H\\1.Speed\\2")	
  	
	
#Split storm name and category into separate columns. 	
merged_dataframe = merged_dataframe %>% 	
  mutate(Storm.Name = str_extract(string = stormname, pattern = "[[:alpha:]]+$"),	
         Storm.Category = str_split_fixed(string = stormname, pattern = paste(" ", Storm.Name, sep = ""), n = Inf)[ ,1]) %>%	
#simply rearrange the columns of the data frame to organize it for export.	
  select(filename, stormname, Storm.Name, Storm.Category, City.Name, State.Name, State.Abb, County.Name, fips, time, date, 	
         setdiff(names(merged_dataframe), c("filename", "stormname", "Storm.Name", "Storm.Category", "City.Name", "State.Name", "State.Abb", "County.Name", "fips", "time", "date")))	
	
        	
#' 	
#' 	
#' 	
#' 	
#' Writes combined_dataframe from the chunk above into a .csv file so that the code doesn't need to re-run each time to generate the combined_dataframe object (i.e. it can be read from the .csv file). 	
#' 	
write.csv(x = merged_dataframe, row.names = F, file = paste(mycleanpath, "Wind_Speed_Probability_Tables.csv", sep = ""))	
#' 	
#' 	
#' 	
#' 	
#' NOTE: NO NEED TO RUN THE CHUNK BELOW! The output file "PWSAT_City_State.csv" is an intermediate output and requires additional, manual changes. The completed version is in "m1dnr00/Hurricanes/PWSAT_City_State.csv" and is read in the chunks above. DO NOT REPLACE THE FINAL VERSION!	
#' 	
#' Creates a .csv file with two columns: one with unique location names and another with the corresponding state (if available within the location name string). Any missing states in the .csv file are expected to be added manually (i.e. typing them in) after writing.	
#' 	
# unique_locations = unique(combined_dataframe$location) #Generates unique locations from combined_dataframe	
# 	
# #Finds state abbreviations within the location (if available) and extracts them. NA if not found.	
# state_extractor = str_extract(string = unique_locations, pattern = "\\b[[:alpha:]]{2}$")	
# state_extractor = ifelse(is.na(state_extractor), yes = "", no = state_extractor)	
# 	
# # Logical vector with length(unique_locations) that contains FALSE if abbreviations for Mexico, Nova Scotia, Puerto Rico, island, and New Brunswick are detected. Will be used to filter out these locations.	
# condition = !str_detect(string = state_extractor, pattern = "MX|NS|PR|IS|NB")	
# 	
# export_frame = data.frame(location = unique_locations, state = state_extractor) %>% 	
#   filter(condition)	
# 	
# write.csv(x = export_frame, file = paste(mycleanpath, "PWSAT_City_State.csv", sep = ""), row.names = F)	
#' 	
#' 	
#' 	
#' 	
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX	
#' TESTING THE DATA FOR ERRORS	
#' 	
#' Tests for issues related to line 44 of the code (omitting files with no wind speed tables). There may be tables that are erroneously considered blank after the truncation due to inconsistencies with the text file formatting. Here we perform a number of logic checks to ensure the files created above are correct. 	
#' 	
#Test 1: Check to see which filenames contain zero rows (i.e. no wind speed probability table) after truncation (contained in blankfilevector). Compare it with "full_long_states_storms_dates_filenames_PWSAT_PWSEP.csv" which is a .csv file that contains all mentions of hurricanes (at their various stages) in the PWSAT_PSWEP text files for the years 2007-2017. We would expect there to be no intersection of filenames between the two; this is based on the assumption that if a hurricane is referenced (in any stage) it should always have a wind speed table.	
	
mentions = read.csv(paste0(mycleanpath, "full_long_states_storms_dates_filenames_PWSAT_PWSEP.csv"))	
mentions = unique(mentions$filename)	
setwd(mypath)	
	
blankfilevector = c() #Vector to contain names of files that generate blank data frames once truncated. 	
for(i in 1:length(filevector)) {	
  temp_frame = read.delim(file = filevector[i], header = F, colClasses = "character")	
  temp_frame[ ,2:11] = ""	
  start_truncation = grep(x = temp_frame[ ,1], pattern = "LOCATION *KT") 	
  temp_frame = temp_frame[-(1:start_truncation[1]), ]	
  end_truncation = grep(temp_frame[ ,1], pattern = "[-]+ WIND SPEED PROBABILITIES FOR FORECAST|[$]+|[.]+THERE")	
  temp_frame = temp_frame[-(end_truncation[1]:length(temp_frame[ ,1])), ]	
  non_blank_lines = grep(x = temp_frame[ ,1], pattern = "[[:alnum:]]")	
  temp_frame = temp_frame[non_blank_lines, ] 	
  if(nrow(temp_frame) ==0) {	
    blankfilevector[i] = filevector[i]	
  }	
}	
	
blankfilevector = blankfilevector[!is.na(blankfilevector)]	
intersect(x = mentions, y = blankfilevector)	
#We find that there is an intersection between blankfilevector and the filenames in the .csv file. This means that hurricane names appeared in certain text files but had no wind speed probability table included. This seems unlikely; however, checking the files that appear in the intersection reveals that this is indeed the case. Therefore, the assumption that a hurricane should always have a wind speed table is wrong.	
#' 	
#' 	
#' 	
#Test 2: (Test 1 must be run for this to work). Check to see if there are files with no wind tables "sandwiched" between other files with wind tables. The premise of this check is that it would be very unlikely for a wind speed table to appear in one file, followed by a blank table in the next file, followed by another non-blank table. We check to see if these files exist.	
	
blankfileindices = which(filevector %in% blankfilevector) #Gives indices of files within filevector that have blank tables.	
sandwichedfilevector = c(NA) #Vector to hold sandwiched files indices	
	
#We start with i ==2 since the expression [i-1] causes issues when i ==1; also, we see from visual inspection that file 1 is not sandwiched.	
for(i in 2:length(blankfileindices)) { 	
  if(blankfileindices[i+1] != blankfileindices[i] + 1 & blankfileindices[i-1] != blankfileindices[i] -1) {#Evaluation for interior elements	
    sandwichedfilevector[i] = filevector[i]	
  }	
  if(i == length(blankfileindices) & blankfileindices[i-1] != (blankfileindices[i] - 1)) {#Evaluation for last element	
    sandwichedfilevector[i] = filevector[i]	
  }	
}	
	
sandwichedfilevector = sandwichedfilevector[!is.na(sandwichedfilevector)]	
sandwichedfilevector	
#We find that there are 102 sandwiched files. However, further inspection shows that they actually do not contain wind speed tables. Thus, sandwiched files do contain blank wind speed tables. 	
#' 	
#' 	
#' 	
#Test 3: Visually inspect a random subset of files that the code identified as not having wind speed tables; verify that they actually do not have wind speed tables.	
	
blankfilevector[sample(x = 1:length(blankfilevector), size = 20, replace = F)]	
	
#All files from the sample were verified as not having wind speed tables. 	
#' 	
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX	
#' 	

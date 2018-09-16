#' ---	
#' title: "R Notebook"	
#' editor_options: 	
#'   chunk_output_type: console	
#' ---	
#' #################################################################	
#' AUTHOR INFO: Written by David Rubio, 2018 Summer Intern. 	
#' 	
#' BACKGROUND: NOAA releases weather forecasts that contain wind-speed probabilities by location whenever it detects a storm forming. They are formatted as text files and can be downloaded online from NOAA's website (https://www.nhc.noaa.gov/archive/text/). The files used here are the PWSEP and PWSAT text files that range from 2007-2017. We wish to determine whether some counties do not have NOAA data (perhaps because they are negligibly small) and are sandwiched between counties with data . If this is the case, we must decide how to determine hurricane wind risk in these counties since they are close to other counties with measured risk (and it would be incorrect to assume they have zero windspeed risk simply because they do not have a reporting station). 	
#' 	
#' PURPOSE OF THIS FILE: Creates maps of the counties with NOAA forecast data in order to determine whether there are data-less counties between them. We find that this is the case and that it is necessary to interpolate windspeed risk for nearby counties without data. Therefore, this file also contains a function that assigns a windspeed risk dummy to data-less counties within a specified radius of counties with data and that meet specified criteria.	
#' 	
#' IMPORTANT NOTES: This file requires that you install the package "measurements" to make distance unit conversions more simple. 	
#' #################################################################	
#' 	
#' 	
#' NOTES TO SELF:	
#' -Find out why map of counties with least-stringent criteria still looks different from map of all NOAA data.	
#'   -Perhaps because of the weekend/market close omissions?	
#' 	
#' 	
#' Install "measurements" package (only needs to be done once)	
#' 	
#install.packages("measurements")	
#' 	
#' 	
#' 	
#' Package Load	
#' 	
library(dplyr)	
library(maps)	
library(ggplot2)	
library(stringr)	
library(sp)	
library(rgeos)	
library(rgdal)	
library(measurements)	
library(lubridate)	
library(chron)	
library(stringr)	
#' 	
#' 	
#' Clear Environment	
#' 	
rm(list = ls())	
#' 	
#' 	
#' Allows Mathias to run the code	
#' 	
#knitr::opts_knit$set(root.dir = "/")	
#' 	
#' 	
#' 	
#' Data Initialization	
#' 	
directory_prefix <- "~/" #David	
mycleanpath <-  "mcr/home/m1bxr01/Hurricanes/data/clean/" #David/Brigitte	
Davidfolderpath <-  "Hurricanes/clean/" #For David only	
county_shp_path <- "US_Shapefiles/" #David	
	
#directory_prefix <- "~/drives/" #Brigitte	
#county_shp_path <- "mcr/home/m1dnr00/US_Shapefiles/" #Brigitte	
	
#' 	
#' 	
#' Creates preliminary data frames necessary to create maps of the windspeed counties. 	
#' 	
#Creates a data frame of the unique states in the NOAA wind speed tables. Will be used to filter county data below. 	
NOAA_states <-read.csv(paste0(directory_prefix, "mcr/home/m1dnr00/Hurricanes/Wind_Speed_Probability_Tables.csv"), stringsAsFactors = F) %>%	
  select(State.Name) %>%	
  mutate(State.Name = tolower(State.Name)) %>%	
  unique() %>%	
  pull(State.Name)	
	
#Creates a data frame of all US counties with fips codes, coordinates, and corrections for counties that are split (e.g. "washington,pierce:north")	
counties <- county.fips %>% 	
  mutate(polyname = case_when(	
    str_detect(string = polyname, pattern = "washington,pierce") ~ "washington,pierce",	
    str_detect(string = polyname, pattern = "louisiana,st martin") ~ "louisiana,st martin",	
    str_detect(string = polyname, pattern = "texas,galveston") ~ "texas,galveston",	
    str_detect(string = polyname, pattern = "florida,okaloosa") ~ "florida,okaloosa",	
    str_detect(string = polyname, pattern = "virginia,accomack") ~ "virginia,accomack",	
    str_detect(string = polyname, pattern = "north carolina,currituck") ~ "north carolina,currituck",	
    str_detect(string = polyname, pattern = "washington,san juan") ~ "washington,san juan",	
    T ~ polyname),	
    region_subregion = lapply(X = 1:length(polyname), function(x) FUN = str_split_fixed(string = polyname[x], pattern = ",", n = Inf)),	
    region = sapply(X = 1:length(region_subregion), function(x) region_subregion[[x]][1]),	
    subregion = sapply(X = 1:length(region_subregion), function(x) region_subregion[[x]][2])) %>% 	
  inner_join(y = map_data("county"), by = c("region", "subregion")) %>% 	
  select(-region_subregion)	
	
	
#Creates a data frame that lists each county within a NOAA-referenced state and determines whether each county has NOAA data or not. Also merges with the data frame above to associate coordinates with these counties. 	
NOAA_counties = read.csv(paste0(directory_prefix, "mcr/home/m1dnr00/Hurricanes/Wind_Speed_Probability_Tables.csv"), stringsAsFactors = F) %>%	
  select(County.Name, State.Name) %>%	
  mutate(County.Name = tolower(County.Name),	
         State.Name = tolower(State.Name),	
         has_data = 1) %>%	
  unique(.) %>%	
  right_join(y = counties, by = c("State.Name" = "region", "County.Name" = "subregion")) %>%	
  replace(is.na(.), 0) %>%	
  filter(State.Name %in% NOAA_states) %>%	
  mutate(has_data = as.factor(has_data))	
	
levels(NOAA_counties$has_data) = c("No", "Yes") #Changes category names for "has_data" variable to Yes or No	
	
rm(counties)	
#' 	
#' 	
#' 	
#' Creates a map of the the counties that do/don't have NOAA whether data. Shows only states that contain a county with NOAA data. 	
#' 	
	
county_map = ggplot(data = NOAA_counties) +	
  coord_fixed(ratio = 1.3) + 	
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data), color = "black", size = 0.2) +	
  theme_bw() +	
  theme(axis.text = element_blank(),	
        axis.line = element_blank(),	
        axis.ticks = element_blank(),	
        panel.border = element_blank(),	
        panel.grid = element_blank(),	
        axis.title = element_blank()) +	
  scale_fill_manual(values = c("#33c7ff", "#ff9f33")) +	
  labs(title = "Counties with NOAA Wind Speed Forecast Data",	
       subtitle = "All Storms from 2007 - 2017. Non-Atlantic States and States without Data Excluded.",	
       fill = "Has NOAA data?") 	
      	
#county_map 	
#There are indeed gaps between counties that must be accounted for. 	
	
#Save map to the desired directory in David's folder	
#ggsave(plot = county_map, filename = paste0(directory_prefix, Davidfolderpath, "NOAA_county_forecast_map.pdf"), height = 3.5, width = 6, units = "in")	
rm(county_map)	
#' 	
#' 	
#' 	
#' THE CODE BELOW MODIFIES AND STORES A COMPLETED OBJECT (DESCRIBED BELOW) AS AN ".RData" FILE WHICH CAN SIMPLY BE LOADED INTO THE ENVIRONMENT IN THE FUTURE; IT SHOULD NOT NEED TO BE RUN AGAIN. The output is saved in the directory specified at the very bottom of the chunk.	
#' 	
#' Creates a distance matrix (i.e. the distances between each county) called "US_counties" that contains the distance between each county within states that have any county with windspeed data. 	
#' 	
# setwd(paste0(directory_prefix, "mcr/home/m1dnr00/", county_shp_path))	
# 	
# #Reads in county shapefile data from US Census Bureau. Downloaded from https://www.census.gov/cgi-bin/geo/shapefiles/index.php. Use drop-down list options: year = "2017" and layer type = "Counties (and Equivalent)"	
# US_counties <- readOGR(dsn = "All_US_Counties", layer = "UScounties")	
# 	
# #Finds geographic centers for each county and returns the coordinates as a matrix. The lapply creates a list of the coordinates for each county, where each list element is one county's coordinates	
# centroids <- coordinates(US_counties)	
# centroids <- lapply(X = 1:nrow(centroids), function(x) return(centroids[x, 1:2]))	
# 	
# #Add coordinates list to US_counties@data dataframe as a column	
# US_counties@data <- US_counties@data %>%	
#   mutate(county_center = centroids,	
#          STATE_NAME = tolower(STATE_NAME),	
#          NAME = tolower(NAME)) %>%	
#   select(-STATE_FIPS, -CNTY_FIPS)	
# rm(centroids)	
# 	
# #Keep only states that have counties with wind speed data.	
# US_counties <- US_counties[US_counties@data$STATE_NAME %in% NOAA_states, ]	
# 	
# #Saves data frame from "US_counties" object so that the rest of the polygons can be removed (to reduce memory usage).	
# US_counties <- US_counties@data	
# 	
# US_counties <- US_counties %>%	
#   mutate(FIPS = as.integer(as.character(FIPS))) %>% #Makes "FIPS" column into an integer rather than a factor	
#   arrange(FIPS)	
# 	
# #Generate a list of comminbations of county coordinates so that each county's coordinates is uniquely paired with every other county's coordinates. There are 1204^2 elements in the list (about 1.5 million)	
# combination_list <- purrr::cross2(US_counties$county_center, US_counties$county_center)	
# 	
# #Goes through each coordinate combination in the list above and calculates the distance between the two coordinate. Also converts these distances to miles (from meters). Returns a vector of distances.	
# combination_list <- sapply(X = 1:length(combination_list), function(a) conv_unit(x = geosphere::distm(x = combination_list[[a]][[1]], y = combination_list[[a]][[2]]), from = "m", to = "mi"))	
# 	
# #Takes the vector of distances from above and makes it into a matrix. The matrix has been created carefully so that each row and column represents a distance from one county to another.	
# distance_matrix <- matrix(data = combination_list, nrow = 1204, byrow = F)	
# rm(combination_list)	
# 	
# #Appends distances to "US_counties" dataframe which contains all of the counties' geographic data (FIPS, county name, state, etc.)	
# US_counties <- cbind(US_counties, distance_matrix) %>% select(-county_center)	
# rm(distance_matrix)	
# 	
# #Make column names of distance matrix the FIPS code of the county being compared	
# names(US_counties)[4:length(names(US_counties))] <- US_counties$FIPS	
# 	
# #Saves "US_counties" shapefile object so that it can be loaded without running the code again. The shapefile contains the boundaries of the counties, county centers, and the distance matrix.	
# save(US_counties, file = paste0(directory_prefix, Davidfolderpath, "Windspeed_County_Distance_Matrix.RData"))	
#' 	
#' 	
#' 	
#' 	
#' Imports Wind Speed Probability Table and modifies it so that it can be used to interpolate wind speed risk.	
#' 	
#Load in windspeed table	
windspeed <- read.csv(file = paste0(directory_prefix, "mcr/home/m1dnr00/Hurricanes/Wind_Speed_Probability_Tables.csv"), stringsAsFactors = F)	
	
#Aggregate wind speed data for each county so that there is one unique observation per county (i.e. take the mean of wind speed probabilities if there are multiple locations in the same county)	
windspeed <- windspeed %>% 	
  group_by(filename, stormname, Storm.Name, Storm.Category, State.Name, State.Abb, County.Name, fips, time, date) %>% 	
  summarise_if(.predicate = is.numeric, funs(mean)) %>% 	
  ungroup()	
	
# #Filter wind speed data so that only the most recent forecast before market close time is used . Market close time is 4pm EDT/EST depending on whether daylight savings is in effect. The wind speed data is in UTC which does not adjust for daylight savings; Thus, the cutoff is either 2000 or 2100 UTC respectively. Weekend forecasts are dropped since no market data is reported during this time frame.	
# windspeed <- windspeed %>%	
#   mutate(is_dst = dst(date), #Adds a dummy variable =1 if a given forecast was released in EDT	
#          is_weekend = is.weekend(date), #Adds a dummy variable =1 if a given forecast was released on a weekend. Row will be removed if so.	
#          time = as.integer(str_extract_all(string = time, pattern = "[0-9]+"))) %>% #Extracts number from  time column and removes "UTC"	
#   filter(is_weekend == 0) %>% #Removes weekend forecasts	
#   group_by(stormname, Storm.Name, Storm.Category, State.Name, State.Abb, County.Name, fips, date, is_dst, is_weekend) %>%	
#   mutate(within_close = (time < 2000 & is_dst == 1) | (time < 2100 & is_dst == 0)) %>% #Checks to see if forecasts come before trade close	
#   ungroup() %>%	
#   filter(within_close == 1) %>% #Removes forecasts that are not within close of trade day	
#   group_by(Storm.Name, State.Name, State.Abb, County.Name, fips, date, is_dst, is_weekend) %>%	
#   arrange(time) %>%	
#   filter(row_number() == n()) %>%	
#   ungroup()	
	
#The code below performs a similar function as above, but does not drop weekend forecasts and treats them as if they have a 4pm market close. This was specifically requested by Brigitte Roth Tran, Sumudu Watugala, and Mathias Kruttli because it will be useful for lagged regressions on the price of stocks at market open on Monday.	
windspeed <- windspeed %>%	
  mutate(is_dst = dst(date),	
         time = as.integer(str_extract_all(string = time, pattern = "[0-9]+"))) %>%	
  group_by(stormname, Storm.Name, Storm.Category, State.Name, State.Abb, County.Name, fips, date, is_dst) %>%	
  mutate(within_close = (time < 2000 & is_dst == 1) | (time < 2100 & is_dst == 0)) %>%	
  ungroup() %>%	
  filter(within_close == 1)  %>%	
  group_by(Storm.Name, State.Name, State.Abb, County.Name, fips, date, is_dst) %>%	
  arrange(time) %>%	
  filter(row_number() == n()) %>%	
  ungroup()	
#' 	
#' 	
#' 	
#' 	
#' Creates a function that assigns to data-less counties a wind speed risk dummy that is assigned if certain user-defined criteria are met. Dummies are only assigned to data-less counties within a user-defined radius. Outputs a data frame object that lists all counties (those with NOAA data and those that were interpolated) that meet the criteria. NOTE: This function will be nested in another one below and need not be called separately. 	
#' 	
#Load in US county shapefiles with distance matrix	
load(file = paste0(directory_prefix, Davidfolderpath, "Windspeed_County_Distance_Matrix.RData"))	
	
#Description of function arguments: Radius = distance from county center as an integer	
#forecast_horizon_vector = Ex: c("H12.Speed34", "H24.Speed34") or any such range	
#wind_probability cutoff = cumulative probability across forecast horizonabove which dummy variable	
#csv = Dummy indicating whether or not to export a .csv file of the data frame that is returned.  	
interpolate_risk <- function(radius, 	
                             forecast_horizon_vector , 	
                             wind_probability_cutoff, 	
                             distance_matrix = US_counties, 	
                             csv) {	
  	
#Blank data frame to hold counties with dummies	
  interpolated_counties <- as.data.frame(matrix(data = rep(0, 1), nrow = 1))	
  	
  	
#Replaces distances that are greater than the radius argument with NAs	
  distance_matrix <- distance_matrix %>% 	
    mutate_if(.predicate = is.double, function(x) ifelse(x < radius, yes = x, no = NA))	
  	
  filevector <- unique(windspeed$filename) #Obtain vector of all unique files that appear in wind speed data	
  	
  	
#Determine whether sum of wind probabilities from the forecast horizon specified in "forecast_horizon_vector" is greater than or equal to the "wind_probability_cutoff" specified. If so, assign a dummy variable of 1 to the row in the "windspeed" data frame. Removes observations with dummy = 0	
  windspeed$wind_dummy = ifelse(rowSums(windspeed[ , forecast_horizon_vector]) >=  wind_probability_cutoff, yes = 1, no = 0)	
  windspeed <- windspeed %>% filter(wind_dummy == 1)	
  	
#Iterate over each file to interpolate wind speed risk for data-less counties using counties that do have data during that file's specified date/time	
  for(i in 1:length(filevector)) {	
    	
    windspeed_at_forecast <- windspeed %>% 	
      filter(filename == filevector[i]) %>% 	
      mutate(interpolated = 0) #"interpolated" is a dummy indicating if wind speed data is from NOAA or if it was interpolated by the code	
    	
#Filter distance matrix so that: (1) only counties with data at a given forecast time appear in the column names, and (2) only counties WITHOUT data appear in the row names. This format makes combining the counties much easier. 	
    data_counties_at_forecast <- windspeed_at_forecast$fips	
    dataless_counties_at_forecast <- distance_matrix$FIPS[!distance_matrix$FIPS %in% data_counties_at_forecast]	
    	
    distance_matrix_at_forecast <- distance_matrix %>% 	
      filter(FIPS %in% dataless_counties_at_forecast) %>% 	
      select_if(grepl(x = names(.), pattern = "[[:alpha:]]+") | names(.) %in% data_counties_at_forecast)	
    	
    	
#Removes rows in distance matrix where there is no single county within the specified radius (i.e. all distance elements are NA in that row because none of the counties with data are within 50 meters of that data-less county)	
    distance_matrix_at_forecast <- 	
      distance_matrix_at_forecast[rowSums(!is.na(distance_matrix_at_forecast[ , grepl(x = names(distance_matrix_at_forecast), pattern = "[0-9]+"), drop = F])) > 0, ]	
    	
#Performs further formatting only if the edited data frame is not blank. Formatting prepares the data frame to be binded and appended into a large data frame for each NOAA file. 	
    if(nrow(distance_matrix_at_forecast) != 0) {	
      distance_matrix_at_forecast <- distance_matrix_at_forecast %>% 	
        select(NAME, STATE_NAME, FIPS) %>% 	
        cbind(., windspeed_at_forecast[1,c("filename", "stormname", "Storm.Name", "Storm.Category", "time", "date")]) %>% 	
        mutate(interpolated = 1) %>% 	
        plyr::rbind.fill(., windspeed_at_forecast[ ,c("filename", "stormname", "Storm.Name", "Storm.Category", "State.Name", "State.Abb", "County.Name", "fips", "time", "date", "interpolated")])	
    }	
    	
    interpolated_counties <- plyr::rbind.fill(interpolated_counties, distance_matrix_at_forecast)	
  }	
	
#Format complete data frame (which contains results from all NOAA files) for final output	
  interpolated_counties <- interpolated_counties %>%	
    mutate(FIPS = ifelse(is.na(fips), yes = FIPS, no = fips),	
           STATE_NAME = ifelse(is.na(State.Name), yes = STATE_NAME, no = State.Name),	
           NAME = ifelse(is.na(County.Name), yes = NAME, no = County.Name)) %>%	
    select(NAME, STATE_NAME, FIPS, filename, stormname, Storm.Name, Storm.Category, time, date, interpolated) %>% 	
    mutate(NAME = toupper(NAME),	
           STATE_NAME = toupper(STATE_NAME)) %>% 	
    slice(-1) #Removes the top row of the data frame before returning because it is always blank.	
           	
  	
#Writes out a .csv file of the output data frame if the user specifies it in the arguments of the function call. Destination will always be to the "Hurricanes/clean" folder in David's drive	
  if(csv == 1) {	
    write.csv(x = interpolated_counties, 	
              file = paste0(directory_prefix, "mcr/home/m1dnr00/Hurricanes/clean/", "rad", radius, "_prob", wind_probability_cutoff, "_speed",	
                            str_extract(string = forecast_horizon_vector[1], pattern = "34|50|64"), ".csv"))	
#Example filenames for output: "rad70_prob1_speed64.csv" or "rad100_prob25_speed34.csv"	
  }	
  	
  return(interpolated_counties) 	
}	
    	
#' 	
#' 	
#' 	
#' Creates a function that takes the data frame object returned by the "interpolate_risk" function, generates a map of the counties with data (meaning either NOAA or interpolated data) for a user-defined storm (or storms) across all forecast dates, and returns the map object. NOTE: This is the final function that should be called when generating interpolation .csv files. 	
#' 	
	
generate_interpolation_map <- function(hurricane_name,	
                                       pdf = 0,	
                                       csv = 0,	
                                       radius = 70, 	
                                       forecast_horizon_vector = c("H12.Speed64", "H24.Speed64", "H36.Speed64", "H48.Speed64",	
                                                                   "H72.Speed64","H96.Speed64","H120.Speed64"), 	
                                       wind_probability_cutoff = 1, 	
	
                                                                              distance_matrix = US_counties){	
  	
#Call function from above with user-defined arguments. Filter it to contain data from a specific storm (or storms)	
  interpolation_file <- interpolate_risk(radius, forecast_horizon_vector, wind_probability_cutoff, distance_matrix, csv) %>% 	
    filter(Storm.Name %in% hurricane_name) %>% 	
    mutate(name = tolower(NAME),	
           state_name = tolower(STATE_NAME))	
                          	
#Create map object if pdf argument is equal to 1  	
  if(pdf == 1) {	
    	
    map_data_one <- left_join(NOAA_counties, interpolation_file, by = c("fips" ="FIPS")) 	
    	
    map <- ggplot(data = map_data_one) +	
      coord_fixed(ratio = 1.3) + 	
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = as.factor(interpolated)), size = 0.2, color = "black") +	
      theme_bw() +	
      theme(axis.text = element_blank(),	
            axis.line = element_blank(),	
            axis.ticks = element_blank(),	
            panel.border = element_blank(),	
            panel.grid = element_blank(),	
            axis.title = element_blank()) +	
      theme(legend.position="bottom") +	
      scale_fill_discrete(na.value = "white") +	
      labs(fill = "Data was interpolated from nearby counties?",	
           title = paste0("County Forecasts for Hurricane ", hurricane_name),	
           subtitle = paste("Interpolation Parameters: Radius =", radius, "Prob =", wind_probability_cutoff, "Speed =", str_extract(string = forecast_horizon_vector[1], pattern = "34|50|64")))	
    	
    	
    ggsave(plot = map, filename = paste0(directory_prefix, "mcr/home/m1dnr00/Hurricanes/clean/", hurricane_name[1], "_rad", radius, "_prob",	
                                         wind_probability_cutoff, "_speed",str_extract(string = forecast_horizon_vector[1], 	
                                                                                       pattern = "34|50|64"), ".pdf"))	
#Example filenames for output: "matthew_rad70_prob1_speed64.pdf" or "rad100_prob25_speed34.pdf"	
    	
    return(map)	
  }	
}	
#' 	
#' 	
#' 	
#' Run function calls in this chunk as needed. 	
#' 	
all_hurricanes <- unique(windspeed$Storm.Name) #Creates a vector of all hurricane names which can be easily used as an argument in a function	
	
# #Example of a valid function call:	
# generate_interpolation_map(hurricane_name = c("SANDY", "HARVEY")  #Hurricanes to search for. Can pass multiple names or one. Must be in CAPS	
#                            pdf = 1, #Dummy that makes a .pdf map (in my directory) of the counties that meet the criteria. Either 1 or 0	
#                            csv = 1, #Dummy that makes a .csv file (in my directory) of county forecast data that meet the criteria. 1 or 0	
#                            radius = 75, #Radius (in miles). Determines the range over which county data should be interpolated.	
#                            wind_probability_cutoff = 5, #The cumulative probability over the horizons which serves as a cutoff for the dummy	
#                            forecast_horizon_vector = c("H12.Speed64", "H24.Speed64", "H36.Speed64", "H48.Speed64", #Time/speed horizon	
#                                                                    "H72.Speed64","H96.Speed64","H120.Speed64"))	
	
# #Another example:	
  # generate_interpolation_map(hurricane_name = all_hurricanes, #Generates data for all hurricanes	
  #                            pdf = 0, 	
  #                            csv = 1, 	
  #                            radius = 100, 	
  #                            wind_probability_cutoff = 1, 	
  #                            forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50")) 	
	
	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 75, 	
                           wind_probability_cutoff = 60, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 75, 	
                           wind_probability_cutoff = 70, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 75, 	
                           wind_probability_cutoff = 80, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
	
	
#----------------------------------------------------------------------------------------------------------------------	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 100, 	
                           wind_probability_cutoff = 60, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 100, 	
                           wind_probability_cutoff = 70, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
generate_interpolation_map(hurricane_name = all_hurricanes,  	
                           pdf = 0, 	
                           csv = 1, 	
                           radius = 100, 	
                           wind_probability_cutoff = 80, 	
                           forecast_horizon_vector = c("H12.Speed50", "H24.Speed50", "H36.Speed50", "H48.Speed50", 	
                                                                   "H72.Speed50","H96.Speed50","H120.Speed50"))	
	
	
#' 	
#' 	
#' 	
#' 	
#' 	
#' 	
#' 	

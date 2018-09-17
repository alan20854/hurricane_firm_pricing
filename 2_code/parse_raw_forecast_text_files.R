# Written by undergrad research assistant Alan Yan
library(stringr)
library(plyr)
library(reshape2)
main_dir <- "C:/Users/Alan/Documents/Hurricanes/"
years <- c(1998:2018)

number_names <- c("ONE", "TWO", "THREE", "FOUR", "FIVE", 
                  "SIX", "SEVEN", "EIGHT", "NINE", "TEN", 
                  "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN", "FIFTEEN", 
                  "SIXTEEN", "SEVENTEEN", "EIGHTTEEN", "NINETEEN", "TWENTY", 
                  "TWENTY-ONE", "TWENTY-TWO", "TWENTY-THREE", "TWENTY-FOUR", "TWENTY-FIVE", 
                  "TWENTY-SIX", "TWENTY-SEVEN", "TWENTY-EIGHT", "TWENTY-NINE", "THIRTY",
                  "THIRTY-ONE", "THIRTY-TWO", "THIRTY-THREE", "THIRTY-FOUR", "THIRTY-FIVE", 
                  "THIRTY-SIX", "THIRTY-SEVEN", "THIRTY-EIGHT", "THIRTY-NINE", "FOURTY",
                  "FOURTY-ONE", "FOURTY-TWO", "FOURTY-THREE", "FOURTY-FOUR", "FOURTY-FIVE", 
                  "FOURTY-SIX", "FOURTY-SEVEN", "FOURTY-EIGHT", "FOURTY-NINE", "FIFTY")
storm_levels <- c("TROPICAL DEPRESSION", "SUBTROPICAL STORM", "TROPICAL STORM", "HURRICANE", "TROPICAL DISTURBANCE", 
                  "SUBTROPICAL DEPRESSION", "POST-TROPICAL CYCLONE", "POTENTIAL TROPICAL CYCLONE", "REMNANTS OF")
month_nums <- c(JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06", 
                JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12")


initNewRow <- function() {
  new_row <- list(storm_name=NA, storm_level=NA, file_date=NA, file_time=NA, date=NA, time=NA, 
              actual_or_forecast=NA, lat=NA, long=NA, max_wind=NA, gusts=NA, 
              eye_speed=NA, eye_location=NA, storm_end=NA)
  return(new_row)
}

parseStormLevelAndName <- function(line) {
  words <- strsplit(line, " ")
  if (words[[1]][1] %in% storm_levels) {
    storm_level <- words[[1]][1]
    storm_name <- words[[1]][2]
  } else if (paste(words[[1]][1], words[[1]][2], sep = " ") %in% storm_levels) {
    storm_level <- paste(words[[1]][1], words[[1]][2], sep = " ")
    storm_name <- words[[1]][3]
  } else if (paste(words[[1]][1], words[[1]][2], words[[1]][3], sep = " ") %in% storm_levels) {
    storm_level <- paste(words[[1]][1], words[[1]][2], words[[1]][3], sep = " ")
    storm_name <- words[[1]][4]
  } else {
    storm_level <- NA
    storm_name <- NA
    print(line)
  }
  if(is.na(storm_level) || is.na(storm_name)) {
    print("BAD storm_level OR storm_name")
    print(line)
  }
  return(c(storm_level, storm_name))
}

parseFileDateAndTime <- function(line, row_entry) {
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  words <- strsplit(line, " ")
  file_time <- x[1]
  month_index <- 3
  if (words[2] == "UTC") {
    month_index <- month_index + 1
  }
  file_date <- paste(month_nums[words[[1]][month_index]], "/", x[2], "/", x[3], sep = "")
  if(is.na(file_time) || is.na(file_date)) {
    print("BAD file_time OR file_date")
    print(line)
  }
  return(c(file_date, file_time))
}

parseCurrLatLong <- function(line) {
  words <- strsplit(line, " ")
  lat <- words[[1]][4]
  long <- words[[1]][6]
  if(is.na(lat) || is.na(long)) {
    print("BAD LAT OR LONG")
    print(line)
  }
  return(c(lat, long))
}

parseCurrMaxWindAndGusts <- function(line) {
  words <- strsplit(line, " ")
  max_wind <- words[[1]][5]
  gusts <- words[[1]][11]
  if(is.na(max_wind) || is.na(gusts)) { #triple digit speeds
    max_wind <- words[[1]][4]
    gusts <- words[[1]][9]
    if(is.na(max_wind) || is.na(gusts)) {
      print("bad curr max_wind/gusts")
      print(line)
    }
  }
  return(c(max_wind, gusts))
}

isEyeSpeedLocationInvalid <- function(eye_speed, eye_location) {
  if(!grepl("^[0-9]+$", eye_speed) || !grepl("^[0-9]+$", eye_location) || is.na(eye_speed) || is.na(eye_location)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

parseEyeSpeedAndLocation <- function(line) {
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  eye_speed <- x[2]
  eye_location <- x[1]
  if(isEyeSpeedLocationInvalid(eye_speed, eye_location)) {
    print("BAD eye_speed OR eye_location")
    print(line)
    print(paste("Eye_speed: ", eye_speed, " Location: ", eye_location, sep = ""))
  }
  return(c(eye_speed, eye_location))
}

isPrevLatLongTimeInvalid <- function(lat, long, time) {
  if(!grepl("\\(?[0-9]+\\)?", time) || !grepl("[0-9]+\\.[0-9]+", lat) || !grepl("[0-9]+\\.[0-9]+", long)) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

parsePrevLatLongAndTime <- function(line) {
  words <- strsplit(line, " ")
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  lat_long <- str_extract_all(line, "[0-9]+\\.[0-9]+")[[1]]
  lat <- lat_long[1]
  long <- lat_long[2]
  time <- x[2]
  if(is.na(lat) || is.na(long) || is.na(time) || isPrevLatLongTimeInvalid(lat,long,time)) { #when longitude >= 100.0
    lat <- words[[1]][7]
    long <- substr(words[[1]][8], 1, 6)
    if(is.na(lat) || is.na(long) || is.na(time)) { 
      print("BAD lat OR long or time")
      print(line)
    }
  }
  return(c(lat, long, time))
}

parseActualLines <- function(lines) {
  curr_row <- initNewRow()
  three_hours_prev_row <- initNewRow()
  
  #print(lines)
  curr_row$actual_or_forecast <- "A"
  curr_row$storm_end <- 0
  
  for (i in 1:length(lines)) {
    if (grepl("FORECAST/ADVISORY NUMBER", lines[i])) {
      if (grepl("^SPECIAL", lines[i]) || grepl("^FORECAST", lines[i])) { #cases where the line begins with (Special) Forecast... and does not have name+level on line
        
        level_and_name <- parseStormLevelAndName(lines[i-1])
        
        print(level_and_name)
        curr_row$storm_level <- level_and_name[1]
        curr_row$storm_name <- level_and_name[2]
      } else {
        level_and_name <- parseStormLevelAndName(lines[i])
        curr_row$storm_level <- level_and_name[1]
        curr_row$storm_name <- level_and_name[2]
      }
      if (grepl("^[0-9]", lines[i + 2])) {
        date_and_time <- parseFileDateAndTime(lines[i + 2])
        curr_row$file_date <- date_and_time[1]
        curr_row$file_time <- date_and_time[2]
        curr_row$date <- curr_row$file_date
        curr_row$time <- curr_row$file_time
      } else {
        date_and_time <- parseFileDateAndTime(lines[i + 3])
        curr_row$file_date <- date_and_time[1]
        curr_row$file_time <- date_and_time[2]
        curr_row$date <- curr_row$file_date
        curr_row$time <- curr_row$file_time
      }
    }
    
    # Concise information on lat/long
    if (grepl("REPEAT...CENTER LOCATED NEAR", lines[i])) {
      lat_and_long <- parseCurrLatLong(lines[i])
      curr_row$lat <- lat_and_long[1]
      curr_row$long <- lat_and_long[2]
    }
    
    # "WAS" keyword for three hours past
    if (grepl("CENTER WAS LOCATED NEAR", lines[i])) {
      prev_lat_long_time <- parsePrevLatLongAndTime(lines[i])
      three_hours_prev_row$lat <- prev_lat_long_time[1]
      three_hours_prev_row$long <- prev_lat_long_time[2]
      three_hours_prev_row$time <- prev_lat_long_time[3]
    }
    
    if (grepl("^MAX SUSTAINED WINDS", lines[i])) {
      wind_and_gusts <- parseCurrMaxWindAndGusts(lines[i])
      curr_row$max_wind <- wind_and_gusts[1]
      curr_row$gusts <- wind_and_gusts[2]
    }
    
    if (grepl("PRESENT MOVEMENT TOWARD", lines[i])) {
      eye_speed_and_loc <- parseEyeSpeedAndLocation(lines[i])
      curr_row$eye_speed <- eye_speed_and_loc[1]
      curr_row$eye_location <- eye_speed_and_loc[2]
    }
  }
  three_hours_prev_row$storm_name <- curr_row$storm_name
  three_hours_prev_row$file_date <- curr_row$file_date
  three_hours_prev_row$file_time <- curr_row$file_time
  three_hours_prev_row$date <- curr_row$date
  three_hours_prev_row$actual_or_forecast <- curr_row$actual_or_forecast
  three_hours_prev_row$storm_end <- 0
  return(list(curr_row, three_hours_prev_row))
}

parseForecastDayTimeLatLong <- function(line) {
  words <- strsplit(line, " ")
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  lat_long <- str_extract_all(line, "[0-9]+\\.[0-9]+")[[1]]
  day <- x[1]
  time <- x[2]
  if (!grepl("[0-9]+\\.[0-9]+", line)) {
    lat <- NA
    long <- NA
  } else {
    lat <- lat_long[1]
    long <- lat_long[2]
    if (is.na(day) || is.na(time) || is.na(lat) || is.na(long)) {
      print("BAD FORECAST day/time/lat/long")
      print(line)
    }
  }
  return(c(day,time,lat,long))
}

parseForecastWindGusts <- function(line) {
  if (!grepl("MAX WIND", line) && !grepl("GUSTS", line)) {
    return(c(NA, NA))
  } else {
    words <- strsplit(line, " ")
    wind <- words[[1]][4]
    gusts <- words[[1]][7]
    if (is.na(wind) || is.na(gusts)) {
      wind <- words[[1]][3]
      gusts <- words[[1]][5]
      if (is.na(wind) || is.na(gusts)) {
        print("BAD FORECAST WIND/GUST")
        print(line)
      }
    }
    return(c(wind, gusts))
  }
  
}

parseForecastLines <- function(lines) {
  forecast_rows <- list()
  for (i in 1:length(lines)) {
    if(grepl("^FORECAST VALID", lines[i]) || grepl("^OUTLOOK VALID", lines[i])) {
      forecast_row <- initNewRow()
      forecast_row$actual_or_forecast = "F"
      day_time_lat_long <- parseForecastDayTimeLatLong(lines[i])
      forecast_row$date <- day_time_lat_long[1]
      forecast_row$time <- day_time_lat_long[2]
      forecast_row$lat <- day_time_lat_long[3]
      forecast_row$long <- day_time_lat_long[4]
      # if (is.na(forecast_row$lat) && is.na(forecast_row$long)) {
      #   if (!grepl("EXTRATROPICAL", lines[i]) && !grepl("DISSIPAT", lines[i]) && !grepl("ABSORBED", lines[i])) {
      #     #print(lines)
      #     print(lines[i])
      #     print(lines[i+1])
      #   }
      # }
      
      if(grepl("DISSIPATED", lines[i]) || grepl("ABSORBED", lines[i]) || grepl("MERGED", lines[i])) {
        forecast_row$storm_end <- 1
      } else {
        forecast_row$storm_end <- 0
      }
      
      wind_gusts <- parseForecastWindGusts(lines[i+1])
      forecast_row$max_wind <- wind_gusts[1]
      forecast_row$gusts <- wind_gusts[2]
      forecast_rows <- append(forecast_rows, list(forecast_row))
    }
  }
  return(forecast_rows)
}

changeListStormName <- function(l1, name) {
  l1$storm_name <- name
  return(l1)
}

changeListFileDateTime <- function(l1, file_date, file_time) {
  l1$file_date <- file_date
  l1$file_time <- file_time
  return(l1)
} 

changeListDateTime <- function(l1) {
  file_date <- l1$file_date
  file_day <- substr(file_date, 4, 5)
  file_month <- substr(file_date, 1, 2)
  file_year <- substr(file_date, 7, 10)
  forecast_day <- l1$date
  if(is.na(file_day)) {
    print(file_date)
    print(l1)
  }
  if (as.numeric(forecast_day) > as.numeric(file_day)) {
    l1$date <- paste(file_month, "/", forecast_day, "/", file_year, sep = "")
  } else {
    if (file_month == "12") {
      l1$date <- paste("01", "/", forecast_day, "/", as.numeric(file_year) + 1, sep = "")
    } else {
      l1$date <- paste("", as.numeric(file_month) + 1, "/", forecast_day, "/", file_year, sep = "")
    }
  }
  return(l1)
}

fixLongDotsCase <- function(l1) {
  if(grepl("\\.\\.\\.", l1$lat)) {
    print(l1)
  }
  if (grepl("\\.\\.\\.", l1$long)) {
    l1$long <- substr(l1$long, 1, regexpr("\\.\\.\\.", l1$long) - 1)
  }
  return(l1)
}

parseTextToRows <- function(fileName, year) {
  print(fileName)
  conn <- file(fileName, "r")
  lines <- readLines(conn)
  storm_rows_list = list()
  
  actual_last_line <- -1
  actual_lines <- character()
  for (i in 1:length(lines)) {
    if (grepl("FORECAST VALID", lines[i])) {
      actual_last_line <- i-1
      break
    }
  }
  
  if (actual_last_line > 0) {
    storm_rows_list <- append(storm_rows_list, parseActualLines(lines[1:actual_last_line]))
    forecast_rows <- parseForecastLines(lines[actual_last_line+1:length(lines)])
    file_date <- storm_rows_list[[1]]$file_date
    file_time <- storm_rows_list[[1]]$file_time
    forecast_rows <- lapply(forecast_rows, changeListFileDateTime, file_date=file_date, file_time=file_time)
    #print(storm_rows_list)
    forecast_rows <- lapply(forecast_rows, changeListDateTime)
    forecast_rows <- lapply(forecast_rows, fixLongDotsCase)
    storm_rows_list <- append(storm_rows_list, forecast_rows)
  } else {
    storm_rows_list <- append(storm_rows_list, parseActualLines(lines))
  }
  close(conn)
  #print(storm_rows_list)
  return(storm_rows_list)
}

updateStormName <- function(l1, name, year) {
  l1$storm_name <- paste("", year, "_", name, sep = "")
  return(l1)
}

parseStormToRows <- function(storm_folder_dir, files, year) {
  storm_list_of_rows <- list()
  for(file in files) {
    #storm_list_of_rows <- append(storm_list_of_rows, parseTextToRows(file, year))
    ##### do i just want to append here?? ##################
    storm_rows <- parseTextToRows(paste(storm_folder_dir, "/", file, sep = ""), year)
    storm_list_of_rows <- append(storm_list_of_rows, storm_rows)
    ####TODO: let's get the last row and change every row's name to the last one's name ####
    
  }
  for(i in range(length(storm_list_of_rows):1)) {
    storm_name <- storm_list_of_rows[[i]]$storm_name
    if (!is.na(storm_name)) {
     #print(storm_name)
     storm_list_of_rows <- lapply(storm_list_of_rows, updateStormName, name=storm_name, year=year)
     #print(storm_list_of_rows)
     break
    }
  }
  #print(storm_list_of_rows[[1]])
  return(storm_list_of_rows)
}

storm_rows <- list()
for(year in years) {
  year_dir <- paste(main_dir, year, sep = "")
  setwd(year_dir)
  storm_folders <- list.dirs(path = ".", full.names = FALSE, recursive = TRUE)
  for(storm_folder in storm_folders) {
    if (storm_folder != "") {
      storm_folder_dir <- paste(year_dir, "/", storm_folder, sep = "")
      files <- list.files(path=storm_folder_dir, pattern="*.txt", full.names=FALSE, recursive=FALSE)
      #print(files)
      #rows <- parseStormToRows(storm_folder_dir, files, year)
      storm_rows <- append(storm_rows, parseStormToRows(storm_folder_dir, files, year))
    }
  }
}

storm_table <- do.call('rbind', lapply(storm_rows, data.frame))
write.table(storm_table, file="../test.csv", sep=",", append=F, row.names=F, col.names=T)

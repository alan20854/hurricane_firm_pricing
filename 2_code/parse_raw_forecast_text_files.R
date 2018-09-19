# Written by undergrad research assistant Alan Yan
library(stringr)
library(plyr)
library(reshape2)
main_dir <- "C:/Users/Alan/Documents/Hurricanes/"
years <- c(1998:2018)

number_names <- c("ONE", "TWO", "THREE", "FOUR", "FIVE", 
                  "SIX", "SEVEN", "EIGHT", "NINE", "TEN", 
                  "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN", "FIFTEEN", 
                  "SIXTEEN", "SEVENTEEN", "EIGHTEEN", "NINETEEN", "TWENTY", 
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
  new_row <- list(storm_name=NA, year_storm_number=NA, storm_named = 0, storm_level=NA, file_date=NA, file_time=NA, date=NA, time=NA, 
              is_forecast=NA, lat=NA, long=NA, max_wind=NA, gusts=NA, 
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
  
  if(is.element(storm_name, number_names) || is.na(storm_name)) {
    storm_named <- 0
  } else {
    storm_named <- 1
    #print(storm_name)
  }
  return(c(storm_level, storm_name, storm_named))
}

parseFileDateAndTime <- function(line, row_entry) {
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  words <- strsplit(line, " ")
  file_time <- x[1]
  month_index <- 3
  while(is.na(month_nums[words[[1]][month_index]]) && month_index <= length(words[[1]])) {
    month_index <- month_index + 1
  }
  if (month_index == length(words)) {
    print(line)
  }
  file_date <- paste(x[3], month_nums[words[[1]][month_index]], x[2], sep = "")
  if(is.na(file_time) || is.na(file_date) || is.na(month_nums[words[[1]][month_index]])) {
    print("BAD file_time OR file_date")
    print(line)
  }
  return(c(file_date, file_time))
}

parseCurrMaxWindAndGusts <- function(line) {
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  max_wind <- x[1]
  gusts <- x[2]
  if(is.na(max_wind) || is.na(gusts)) {
    print("bad curr max_wind/gusts")
    print(line)
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

parseLatLong <- function(line) {
  lat_long <- str_extract_all(line, "[0-9]+\\.[0-9]+")[[1]]
  lat <- lat_long[1]
  long <- lat_long[2]
  if (grepl(paste(lat, "S"), line)) {
    lat <- paste(lat, "S")
  }
  if (grepl(paste(long, "E"), line)) {
    lat <- paste(long, "E")
  }
  if(is.na(lat) || is.na(long)) {
    print("BAD LAT OR LONG")
    print(line)
  }
  return(c(lat, long))
}

isPrevLatLongTimeInvalid <- function(lat, long, time) {
  if(!grepl("\\(?[0-9]+\\)?", time) || !grepl("[0-9]+\\.[0-9]+", lat) || !grepl("[0-9]+\\.[0-9]+", long)) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

parsePrevLatLongAndTime <- function(line) {
  x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
  lat_long <- parseLatLong(line)
  lat <- lat_long[1]
  long <- lat_long[2]
  time <- x[2]
  if(is.na(lat) || is.na(long) || is.na(time) || isPrevLatLongTimeInvalid(lat,long,time)) { #when longitude >= 100.0
      print("BAD lat OR long or time")
      print(line)
  }
  return(c(lat, long, time))
}

parseActualLines <- function(lines) {
  curr_row <- initNewRow()
  three_hours_prev_row <- initNewRow()
  
  curr_row$is_forecast <- 0
  curr_row$storm_end <- 0
  
  for (i in 1:length(lines)) {
    if (grepl("FORECAST/ADVISORY NUMBER", lines[i])) {
      if (grepl("^SPECIAL", lines[i]) || grepl("^FORECAST", lines[i])) { #cases where the line begins with (Special) Forecast... and does not have name+level on line
        level_and_name <- parseStormLevelAndName(lines[i-1])
      } else {
        level_and_name <- parseStormLevelAndName(lines[i])
      }
      curr_row$storm_level <- level_and_name[1]
      curr_row$storm_name <- level_and_name[2]
      curr_row$storm_named <- level_and_name[3]
      if (grepl("^[0-9]", lines[i + 2])) {
        date_and_time <- parseFileDateAndTime(lines[i + 2])
      } else {
        date_and_time <- parseFileDateAndTime(lines[i + 3])
      }
      curr_row$file_date <- date_and_time[1]
      curr_row$file_time <- date_and_time[2]
      curr_row$date <- curr_row$file_date
      curr_row$time <- curr_row$file_time
    }
    
    # Concise information on lat/long
    if (grepl("REPEAT...CENTER LOCATED NEAR", lines[i])) {
      lat_and_long <- parseLatLong(lines[i])
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
  three_hours_prev_row$is_forecast <- curr_row$is_forecast
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
    
    if (is.na(lat)) {
      lat <- str_extract_all(line, "[0-9]*\\.[0-9]+")[[1]][1]
    }
    if (is.na(long)) {
      long <- str_extract_all(line, "[0-9]*\\.[0-9]+")[[1]][2]
    }
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
    x <- str_extract_all(line,"\\(?[0-9]+\\)?")[[1]]
    wind <- x[1]
    gusts <- x[2]
    if (is.na(wind) || is.na(gusts)) {
      print("BAD FORECAST WIND/GUST")
      print(line)
    }
    return(c(wind, gusts))
  }
}

parseForecastLines <- function(lines) {
  forecast_rows <- list()
  for (i in 1:length(lines)) {
    if(grepl("^FORECAST VALID", lines[i]) || grepl("^OUTLOOK VALID", lines[i])) {
      forecast_row <- initNewRow()
      forecast_row$is_forecast <- 1
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

fixMonthStringRepr <- function(month) {
  if(nchar(month) == 1) {
    return(paste("0", month, sep = ""))
  } else {
    return(month)
  }
}

changeListDateTime <- function(l1) {
  file_date <- l1$file_date
  file_day <- substr(file_date, 7, 8)
  file_month <- substr(file_date, 5, 6)
  file_year <- substr(file_date, 1, 4)
  forecast_day <- l1$date
  if(is.na(file_day)) {
    print(file_date)
    print(l1)
  }
  if (as.numeric(forecast_day) > as.numeric(file_day)) {
    l1$date <- paste("", file_year, file_month, forecast_day, sep = "")
    if (nchar(l1$date) != 8) {
      print("1 BAD DATE CHANGE")
      print(l1$date)
      print(file_year)
      print(file_month)
      print(forecast_day)
    }
  } else {
    if (file_month == "12") {
      l1$date <- paste("", as.numeric(file_year) + 1, "01", forecast_day, sep = "")
      if (nchar(l1$date) != 8) {
        print("2 BAD DATE CHANGE")
        print(l1$date)
        print(file_year)
        print(file_month)
        print(forecast_day)
      }
    } else {
      next_month_num <- as.numeric(file_month) + 1
      next_month <- paste("", next_month_num, sep = "")
      if (next_month_num < 10) {
        next_month <- fixMonthStringRepr(next_month)
      }
      l1$date <- paste("", file_year, next_month, forecast_day, sep = "")
      if (nchar(l1$date) != 8) {
        print("3 BAD DATE CHANGE")
        print(l1$date)
        print(file_year)
        print(file_month)
        print(forecast_day)
      }
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
    forecast_rows <- lapply(forecast_rows, changeListDateTime)
    forecast_rows <- lapply(forecast_rows, fixLongDotsCase)
    storm_rows_list <- append(storm_rows_list, forecast_rows)
  } else {
    storm_rows_list <- append(storm_rows_list, parseActualLines(lines))
  }
  close(conn)
  return(storm_rows_list)
}

updateStormName <- function(l1, name, year) {
  l1$storm_name <- paste("", year, "_", name, sep = "")
  return(l1)
}

parseStormToRows <- function(storm_folder_dir, files, year) {
  storm_list_of_rows <- list()
  for(file in files) {
    storm_rows <- parseTextToRows(paste(storm_folder_dir, "/", file, sep = ""), year)
    storm_list_of_rows <- append(storm_list_of_rows, storm_rows)
    
  }
  for(i in length(storm_list_of_rows):1) {
    is_forecast <- storm_list_of_rows[[i]]$is_forecast
    gusts <- storm_list_of_rows[[i]]$gusts
    if (is_forecast == 0 && !is.na(gusts)) {
      storm_name <- storm_list_of_rows[[i]]$storm_name
      storm_list_of_rows <- lapply(storm_list_of_rows, updateStormName, name=storm_name, year=year)
      break
    }
  }
  return(storm_list_of_rows)
}

setStormNumber <- function(x, num_storm) {
  x$storm_number <- num_storm
  return(x)
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
      rows <- parseStormToRows(storm_folder_dir, files, year)
      rows <- lapply(rows, setStormNumber, num_storm=storm_folder)
      storm_rows <- append(storm_rows, rows)
    }
  }
}

storm_table <- do.call('rbind', lapply(storm_rows, data.frame))
write.table(storm_table, file="../storms.csv", sep=",", append=F, row.names=F, col.names=T)

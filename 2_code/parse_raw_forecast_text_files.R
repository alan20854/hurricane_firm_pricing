# Written by undergrad research assistant Alan Yan

main_dir <- "C:/Users/Alan/Documents/Hurricanes/"
years <- c(1998:1998)

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


parseTextToRows <- function(fileName, year) {
  print(fileName)
  conn <- file(fileName, "r")
  lines <- readLines(conn)
  storm_rows_list = list()
  for (i in 1:length(lines)) {
    print(lines[i])
  }
  close(conn)
}

parseStormToRows <- function(storm_folder_dir, files, year) {
  storm_list_of_rows <- list()
  for(file in files) {
    #storm_list_of_rows <- append(storm_list_of_rows, parseTextToRows(file, year))
    parseTextToRows(paste(storm_folder_dir, "/", file, sep = ""), year)
  }
  #print(files)
}

for(year in years) {
  year_dir <- paste(main_dir, year, sep = "")
  setwd(year_dir)
  storm_folders <- list.dirs(path = ".", full.names = FALSE, recursive = TRUE)
  #print(year_dir)
  for(storm_folder in storm_folders) {
    if (storm_folder != "") {
      storm_folder_dir <- paste(year_dir, "/", storm_folder, sep = "")
      #print(storm_folder_dir)
      files <- list.files(path=storm_folder_dir, pattern="*.txt", full.names=FALSE, recursive=FALSE)
      #print(files)
      #rows <- parseStormToRows(storm_folder_dir, files, year)
      parseStormToRows(storm_folder_dir, files, year)
    }
    
    #files <- list.files(path=getwd(), pattern="*.txt", full.names=FALSE, recursive=FALSE)
  }
}
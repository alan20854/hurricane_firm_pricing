# Written by undergrad research assistant Alan Yan

years <- c(1998:2018)

parseTextToRow <- function(fileName) {
  
}

for(year in years) {
  setwd(paste("C:/Users/Alan/Documents/Hurricanes/", year, sep= ""))
  files <- list.files(path=getwd(), pattern="*.txt", full.names=FALSE, recursive=FALSE)
  
}
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(fiftystater)
library(maptools)
library(rgeos)
library(mapproj)
library(maps)
library(RColorBrewer)

setwd("C:/Users/Alan/Dropbox/Yan/2_code")
orig_df <- read.table("GVKEY_FIPS_EstabShare_Strict.csv", sep=",", header=TRUE)

columns_to_keep <- c("Year", "FIPS", "NrEstabFIPS")
df <- orig_df[columns_to_keep]

groupColumns <- c("Year","FIPS")
dataColumns <- c("NrEstabFIPS")
summed_df <- ddply(df, groupColumns, function(x) colSums(x[dataColumns]))

getYearData <- function(year, df) {
  new_df <- df
  year_df <- new_df[df$Year==year,]
  return(year_df)
}

setColors <- function(value) {
#   colorscale = c("#ff0000 ", "#e7e8ec", "#d0d1d9", "#b9bac6", 
#                  "#a2a4b3", "#8b8da0", "#73768d", "#5c607a", 
#                  "#454967", "#2e3254", "#171c42")
#   # print(row$value)
#   if (is.na(value) || value == 0) {
#     color <- colorscale[1]
#   } else if (value < 10) {
#     color <- colorscale[2]
#   } else if (value < 25) {
#     color <- colorscale[3]
#   } else if (value < 50) {
#     color <- colorscale[4]
#   } else if (value < 100) {
#     color <- colorscale[5]
#   } else if (value < 200) {
#     color <- colorscale[6]
#   } else if (value < 500) {
#     color <- colorscale[7]
#   } else if (value < 1000) {
#     color <- colorscale[8]
#   } else if (value < 2500) {
#     color <- colorscale[9]
#   } else if (value < 5000) {
#     color <- colorscale[10]
#   } else {
#     color <- colorscale[11]
#   }
#   return(color)
}

plotYearEstablishments <- function(year_df, year) {
#   library(maps)
#   library(dplyr)
#   data(county.fips)
#   map("county", fill=TRUE , col=counties$color , resolution=0, lty=0) #fill=TRUE, col=counties$color)
#   #map("county", fill=TRUE, col=counties$color)
#   # counties <- data.frame(region=county.fips$fips)
#   # fips_data <- merge(counties, year_df, all.x=TRUE)
#   # fips_data$color <- numeric(length(fips_data$region))
#   # print(all_fips)
#   # for(i in 1:length(fips_data$region)) {
#   #   fips_data[i, ]$color <- setColors(fips_data[i, ]$value)
#   # }
#   map("county", fill=TRUE, col=fips_data$color)
# 
}

plotShadedCounties <- function(year, stateNames, countyNames, dataCol, 
                               dataName="data",
                               fillValue=NA,
                               borderColor=NA,
                               tercile=NA,
                               quintile=NA,
                               decile=NA){
  #~ https://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r
  
  # stateNames <- c("alabama", "alabama", "arizona", "arkansas", "california", "colorado", 
  #                 "connecticut", "florida", "georgia")
  # countyNames <- c("autauga", "blount", "apache", "bradley", "orange", "boulder",
  #                  "new haven", "flagler", "turner")
  # dataCol <- c(0, 5, 15, 50, 150, 1000, 5000, 249, 30)
  library(ggplot2)
  map.county <- map_data('county')
  counties   <- unique(map.county[,5:6])
  data_map <- merge(counties, data.frame(region=stateNames, 
                                         subregion=countyNames, 
                                         data1= dataCol),
                    by=c("region","subregion"), all.x=T, all.y=F
  )
  
  data_map$data1[which(is.na(data_map$data1))] <- fillValue
  # for(i in 1:length(data_map$data1)) {
  #   if(is.na(data_map$data1[i])) {
  #     data_map$data1[i] <- 0
  #   }
  # }
  library(data.table)
  map.county <- data.table(map_data('county'))
  setkey(map.county,region,subregion)
  data_map <- data.table(data_map)
  setkey(data_map,region,subregion)
  map.df <- map.county[data_map]
  
  if (!is.na(tercile)) {
    
  } else if (!is.na(quintile)) {
    
  } else if (!is.na(decile)) {
    
  } else {
    
  }
  map.df$brks <- cut(map.df$data1, 
                     breaks=c(0, 1, 10, 25, 100, 250, 1000, 10000), 
                     labels=c("0", "1 - 10", "11 - 25", "26 - 100", 
                              "101 - 250", "251 - 1000", "1001 - 10000"),
                     include.lowest = TRUE)
  plot <- ggplot(map.df, aes(x=long, y=lat, group=group, fill=brks)) + 
    scale_fill_brewer(palette="YlOrRd") +
    geom_polygon(colour = borderColor, size = 0.001) +
    coord_map()  +
    expand_limits(x = map.county$long, y = map.county$lat) +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    labs(fill=dataName)  +
    theme(legend.position = "right", panel.background = element_blank()) +
    ggtitle(paste("", year, "Establishments", sep = " ")) +
    guides(fill=guide_legend(title=""))
  pdf(paste0("", year, "_Establishments.pdf"))
  print(plot)
  dev.off()

}

isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

convertFipsToCountyState <- function(year_df) {
  library(sqldf)
  data(county.fips)
  countyTable <- county.fips
  states_counties <- c()
  states <- c()
  counties <- c()
  for(i in 1:length(year_df$FIPS)) {
    currFips <- toString(year_df$FIPS[i])[1]
    res <- fn$sqldf("select * from countyTable where fips='$currFips'")
    state_county <- str_split(res$polyname[1], ",")
    state <- state_county[[1]][1]
    county <- state_county[[1]][2]
    states <- c(states, state)
    counties <- c(counties, county)
    if(is.na(state) || is.na(county)) {
      #print(currFips)
    }
  }
  return(list(states, counties))
}

plotEstablishments <- function(summed_df, year) {
  year_df <- getYearData(year, summed_df)
  print(year)
  states_counties <- convertFipsToCountyState(year_df)
  states <- states_counties[[1]]
  counties <- states_counties[[2]]
  plotShadedCounties(year, states, counties, year_df$NrEstabFIPS, fillValue=0)
  # new_col_names <- c("year", "region", "value")
  # colnames(year_df) <- new_col_names
  # gg <- county_choropleth(year_df, title=paste0(year, " Establishments"))
}

plotEstablishments(summed_df, "1990")
plotEstablishments(summed_df, "1995")
plotEstablishments(summed_df, "2000")
plotEstablishments(summed_df, "2005")
plotEstablishments(summed_df, "2010")
plotEstablishments(summed_df, "2014")
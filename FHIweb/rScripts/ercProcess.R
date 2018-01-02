## Load Libraries
library(data.table)
library(Hmisc)
library(dtplyr)
library(dplyr)
library(rgeos)


watershedMap <- readOGR(dsn = "f:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")

## Website base
urlBase <- "http://www.wfas.net/archive/www.fs.fed.us/land/wfas/archive/"
years <- 2000:2017
txtDate <- as.Date("2009-06-04")
fileName <- "fdr_obs"
widths <- c(26, 5, 5, 5, 5, 5, 4, 7, 6, 6, 5, 3, 6, 6, 6, 6, 5, 4, 19)
stateTable <- data.table("name" = state.name, "abv" = state.abb)
stateTable <- rbind(stateTable, data.table("name" = c("Virgina", "West Virgina", "Puerto Rico", "US Virgin Islands", "Guam", "a", "b"), 
                                           "abv" = c("VA", "WV", "PR", "VI", "GU", "PR", "VI")))
errorTable <- data.table(matrix(0, nrow = 6500, ncol = 3))
errorIndex <- 1
setnames(errorTable, c("year", "month", "day"))

obsReports <- lapply(years, FUN = function(x){
  returnList <- paste0(x, "-01-01") %>% 
    as.Date() %>% yearDays() %>%
    vector(mode = "list")
  returnList <- lapply(1:length(returnList), FUN = function(y){
    currentDate <- paste(x , y) %>% strptime(format="%Y %j") %>% as.Date()
    myMonth <- ifelse(nchar(month(currentDate)) == 1,
                      paste0(0, month(currentDate)),
                      month(currentDate))
    myDay <- ifelse(nchar(mday(currentDate)) == 1,
                      paste0(0, mday(currentDate)),
                      mday(currentDate))
    extention <- ifelse(currentDate >= txtDate, ".txt", ".dat" )
    url <- paste0(urlBase, x, "/", myMonth, "/", myDay, "/", fileName, extention)
    dayTable <- tryCatch({
      dayTable <- read.fwf(url, widths = widths, comment.char = "", stringsAsFactors = F) %>% data.table() %>% tableCleaner()
      dayTable[, c("year", "month", "day") := list(x, myMonth, myDay) %>% lapply(FUN = as.numeric)]
      return(dayTable)
    }, error = function(e){
      errorTable[errorIndex, c("year", "month", "day") := list(x, myMonth, myDay) %>% lapply(FUN = as.numeric)]
      errorIndex <<- errorIndex + 1
      print(e)
      NULL
    })
    return(rbindlist(dayTable))
  })
})
errorTable <- errorTable[year != 0,]
save(obsReports, file = "ercObs.RData")
obsReports <- lapply(obsReports, rbindlist)
obsReports <- rbindlist(obsReports)
obsReports <- obsReports[State == "CO",]
obsReports[, stationName := gsub("\\d", "", Station)]
obsReports[, Station := gsub("\\D+", "", Station)]
obsReports[, Station := as.numeric(Station)]

for(j in c("Elev", "Lat", "Long", "Tmp", "RH", "Wind", "PPT", "ERC", "BI", "SC", "KBDI",
           "HUN", "THOU", "TEN", "ADJ")){
  set(obsReports, j = j, value = as.numeric(obsReports[[j]]))
}

z <- obsReports[Station %in% c(51508, 51606, 51510, 51703) & year == 2012 & month == 7 & day == 1, ERC] %>% as.numeric() *
  c(.09, .3, .31, .3) 

# Plot Stations on WS Map
stationPoints <- unique(obsReports, by = "Station")
stationPoints[, Long := -Long]
stationPoints <- stationPoints[, c("Station", "Elev", "Lat", "Long", "State", "stationName")]
stationPoints[, point.ID := 1:nrow(stationPoints)]
stationPoints <- SpatialPointsDataFrame(coords = stationPoints[, c("Long", "Lat")],
                                        data = stationPoints, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
stationPoints <- spTransform(stationPoints, crs(watershedMap))

watershedMapB <- gBuffer(watershedMap, width = 10000, byid = T, quadsegs = 10)

ercStationKey <- raster::extract(watershedMapB, stationPoints) %>% data.table()
ercStationKey <- ercStationKey[, c("point.ID", "NAME", "HUC8")]
ercStationKey <- merge(ercStationKey, stationPoints, by = "point.ID")
ercStationKey[, c("Long.1", "Lat.1") := NULL]

obsReports <- obsReports[, c("Station", "ERC", "year", "month", "day")]
setnames(obsReports, "Station", "station")
setkey(obsReports, station)
ercStationKey <- ercStationKey[, c("Station", "stationName", "Lat", "Long", "Elev", "HUC8", "NAME")]
setnames(ercStationKey, c("Station", "Lat", "Long", "Elev"), c("station", "lat", "long", "elev"))
setkey(ercStationKey, station)
saveRDS(obsReports, "fhiweb/data/erc/coERC.RDS")
saveRDS(ercStationKey, "FHIweb/data/erc/ercStationKey.RDS")


tableCleaner <- function(dayTable){
  dayTable <- trim(dayTable)
  tableStart <- which(substr(dayTable$V1, 1, 3) == "***", arr.ind = T)[1]
  dayTable <- dayTable[tableStart:nrow(dayTable),]
  setnames(dayTable, c("Station", as.character(dayTable[1, 2:ncol(dayTable)])))
  dayTable[, State := ""]
  state <- NA
  i <- 1
  while(i <= nrow(dayTable)){
    currentRow <- dayTable[i,]
    if(is.na(currentRow$Station)){
      dayTable <- dayTable[-i,]
    }else if(substr(currentRow$Station, 1, 3) == "***"){
      state <- gsub("\\*", "", currentRow$Station) %>% trim()
      state <- stateTable[which.min(adist(state, stateTable$name)), abv]
      state <- ifelse(is.null(state), gsub("\\*", "", currentRow$Station) %>% trim(),
                      state)
      dayTable <- dayTable[-i,]
    }else{
      dayTable[i, State := state]
      i <- i + 1
    }
    
  }
  return(dayTable)
}
  


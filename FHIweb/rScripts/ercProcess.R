source("FHIweb/rScripts/functionScripts/ercFunctions.R")

watershedMap <- readOGR(dsn = "f:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")

## Website base

years <- 2000:2017
txtDate <- as.Date("2009-06-04")


obsReports <- lapply(years, FUN = function(x){
  returnList <- paste0(x, "-01-01") %>% 
    as.Date() %>% yearDays() %>%
    vector(mode = "list")
  returnList <- lapply(1:length(returnList), FUN = function(y){
    currentDate <- paste(x , y) %>% strptime(format="%Y %j") %>% as.Date()
    myMonth <- currentDate %>% month() %>% twoDigit()
    myDay <- currentDate %>% day() %>% twoDigit()
    extention <- ifelse(currentDate >= txtDate, ".txt", ".dat" )
    dayTable <- tryCatch({
      readERC(x, myMonth, myDay, extention)
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
obsReports <- lapply(obsReports, rbindlist)
obsReports <- rbindlist(obsReports)
obsReports %<>% obsCleaner()


# Plot stations on WS Map
stationPoints <- unique(obsReports, by = "station")
stationPoints[, Long := -Long]
stationPoints <- stationPoints[, c("station", "Elev", "Lat", "Long", "State", "stationName")]
stationPoints[, point.ID := 1:nrow(stationPoints)]
stationPoints <- SpatialPointsDataFrame(coords = stationPoints[, c("Long", "Lat")],
                                        data = stationPoints, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
stationPoints <- spTransform(stationPoints, crs(watershedMap))

watershedMapB <- gBuffer(watershedMap, width = 10000, byid = T, quadsegs = 10)

ercStationKey <- raster::extract(watershedMapB, stationPoints) %>% data.table()
ercStationKey <- ercStationKey[, c("point.ID", "NAME", "HUC8")]
ercStationKey <- merge(ercStationKey, stationPoints, by = "point.ID")
ercStationKey[, c("Long.1", "Lat.1") := NULL]

obsReports <- obsReports[, c("station", "ERC", "year", "month", "day")]
ercStationKey <- ercStationKey[, c("station", "stationName", "Lat", "Long", "Elev", "HUC8", "NAME")]
setnames(ercStationKey, c("Lat", "Long", "Elev"), c("lat", "long", "elev"))
setkey(ercStationKey, station)

saveRDS(obsReports, "fhiweb/data/erc/coERC.RDS")
saveRDS(ercStationKey, "FHIweb/data/erc/ercStationKey.RDS")



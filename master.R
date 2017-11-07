library(data.table)
library(magrittr)
library(ggplot2)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(deldir)
library(dismo)
library(rnoaa)
library(LaF)

source("R/coopFunctions.R")
source("R/FHI_functions.R")

countyMap <- readOGR(dsn = "c:/Users/admcc/Documents/ACES/Maps/County/tl_2016_us_county.shp", 
                     layer = "tl_2016_us_county")

countyMap <- countyMap[countyMap$STATEFP == '08',]

load("data/coops.RData")

coopLocs <- data.frame(sapply(coops, with, loc))
coopLocs <- data.table("Name" = names(coopLocs), 
                       "code" = data.frame(sapply(coops, with, code))[,1], 
                       t(coopLocs[1,]), t(coopLocs[2,]))
coopLocs[, code := as.numeric(as.character(code))]

coordinates(coopLocs) <- ~long + lat
proj4string(coopLocs) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


## Create Thisen Polygons around Coop Stations
coopTess <- voronoi(coopLocs@coords)
proj4string(coopTess) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


locTessKey <- data.table(coopLocs %over% coopTess)
locTessKey[, station := as.numeric(rownames(locTessKey))]
setnames(locTessKey, "id", "tess")



##Load Station List and Inventory
stationList <- ghcnd_stations()
stationList <- data.table(stationList)
stationList[, recordLen := last_year - first_year]
stationList <- stationList[substr(id, 1, 2) == "US",]
stationList <- stationList[substr(id, 3, 3) != "1",]

stationList <- stationList[element == "PRCP",]


stationList <- stationList[latitude <= 41.00344 & 
                            latitude >= 36.99245 &
                            longitude >= -109.06020 &
                            longitude <= -102.04152]

for(i in 1:nrow(stationList)){
  tempString <- strsplit(stationList[i, name], " ")[[1]]
  num <- grepl("\\d", tempString)
  stationList$simpleName[i] <- ifelse(length(grep(TRUE, num)) == 0,
                                   do.call(paste, as.list(tempString)),
                                   paste(tempString[1:(grep(TRUE, num)[1] - 1)], collapse = " ", sep = ""))
}

stationList <- stationList[id %in% findKeeperStations(stationList),]

stationFiles <- paste0(stationList$id, ".dly")
presentFiles <- dir("data/ghcnd_all")
stationFiles <- stationFiles[stationFiles %in% presentFiles]
stationFiles <-paste0("data/ghcnd_all/", stationFiles) 
myNames <- c("id", "year", "month", "element", gsub(" ", "", apply(expand.grid(c("value", "mflag", "qflag", "sflag"), 1:31), 1, 
                                                                   paste, collapse="", sep = ""), fixed = T))


stationData <- lapply(stationFiles, function(x){ 
                      file <- laf_open_fwf(x, column_widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)),
                        column_types = c("character", "integer", "integer", "character", 
                                       rep(c("integer", "character", "character", "character"), 31)),
                        column_names = myNames)
                      file <- file[,]
                      return(file)
                      })

stationTable <- rbindlist(stationData)
stationList <- stationList[id %in% unique(stationTable$id)]
stationList <- stationList[id %in% findKeeperStations(stationList),]
stationMap <- stationList

coordinates(stationMap) <- ~longitude + latitude
proj4string(stationMap) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


plot(countyMap)
plot(stationMap, add = T)

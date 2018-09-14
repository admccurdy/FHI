# Load Libraries---------------------------------------------------------------
library("ncdf4")
library("raster")
library("rgdal")
library(data.table)
library(foreach)
library(doParallel)
library(dplyr)
library(ggplot2)
library(MASS)
cl <- makePSOCKcluster(4)
registerDoParallel(cl, 4)

# Load Maps--------------------------------------------------------------------
stateMap <- readOGR("F:/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                    layer = "tl_2016_us_state")
stateMap <- stateMap[stateMap$STATEFP == "08",]
watershedMap <- readOGR("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")
rasterTemplate <- raster("FHIweb/data/dayMetAnn/daymet_v3_prcp_annttl_1980_na.nc4")

# Reproject and crop
stateMap <- spTransform(stateMap, crs(rasterTemplate))
watershedMap <- spTransform(watershedMap, crs(rasterTemplate))
rasterTemplate <- crop(rasterTemplate, stateMap)
rasterTemplate <- setValues(rasterTemplate, 1:280578)
watershedKey <- extract(rasterTemplate, watershedMap)
names(watershedKey) <- as.character(watershedMap@data$HUC8)


# Create List of data.tables for daymet data in CO
measure <- c("prcp", "tmax", "tmin")
fileBase <- "FHIweb/data/dayMetAnn/"
fileList <- dir(fileBase)
dayMetCO <- vector("list", 4)
names(dayMetCO) <- measure
years <- 1980:2016

for(i in measure){
  myList <- grep(i, fileList)
  dayMetList <- lapply(myList, FUN = function(x){
    print(x)
    currentFile <- fileList[x]
    myYear <- as.numeric(substr(currentFile, nchar(currentFile) - 10, nchar(currentFile) - 7))
    print(currentFile)
    myRaster <- raster(paste0(fileBase, currentFile))
    myRaster <- crop(myRaster, stateMap)
    returnList <- data.table(grid = as.vector(rasterTemplate), value = as.vector(myRaster),
                             year = myYear)
  })
  dayMetCO[[i]] <- rbindlist(dayMetList)
}

saveRDS(dayMetCO, "fhiweb/data/dayMetAnn/dayMet.RDS")

precipRFV <- dayMetCO[[1]][grid %in% watershedKey[["14010004"]]]
precipClear <- dayMetCO[[1]][grid %in% watershedKey[["10190004"]]]

for(i in years){
  plot(density(precipRFV[year == as.character(i), value]))
}

precipYear <- precipRFV %>% group_by(year) %>% summarise(avg = mean(value))
clearAvg <- precipClear %>% group_by(year) %>% summarise(avg = mean(value))
plot(density(clearAvg$avg))

annimasAvg <- dayMetCO[[1]][grid %in% watershedKey[["14080104"]]] %>% group_by(year) %>% summarise(avg = mean(value))
plot(density(annimasAvg$avg))

eagleAvg <- dayMetCO[[1]][grid %in% watershedKey[["14010003"]]] %>% group_by(year) %>% 
  summarise(avg = mean(value)) 
plot(density(eagleAvg$avg))

temp <- precipRFV %>% group_by(year) %>% summarise(avg = mean(value))
rfvGrid <- unique(precipRFV$grid)
plot(density(temp$avg))
mean(temp$avg)
sd(temp$avg)
qnorm(.995, 810.65, 138.36)
gammaRFV <- fitdistr(temp$avg, "gamma")


plot(density(precipRFV[grid == sample(rfvGrid, 1), value]))

aspenPrecip <- read.csv("FHIweb/data/aspenPrecip.csv")
plot(density(aspenPrecip$precip))

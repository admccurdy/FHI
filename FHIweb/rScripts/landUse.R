library(raster)
library(maptools)
library(rgeos)
library(data.table)
library(dplyr)
library(ncdf4)
library(rgdal)
library(foreach)
library(doParallel)

cl <- makeCluster(8)
registerDoParallel(cl)

lcChange <- raster("FHIweb/data/Colo_change/nlcd_2001_to_2011_landcover_1.tif")

# Load Maps--------------------------------------------------------------------
stateMap <- readOGR("c:/Users/admcc/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                    layer = "tl_2016_us_state")
stateMap <- stateMap[stateMap$STATEFP == "08",]
watershedMap <- readOGR("c:/Users/admcc/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")

currentWaterShed <- 


rasterTemplate <- raster("FHIweb/data/Colo_change/nlcd_2001_to_2011_landcover_1.tif")

# Reproject and crop
watershedMap <- spTransform(watershedMap, crs(rasterTemplate))
rasterTemplate <- crop(rasterTemplate, watershedMap)

watershedLC <- foreach(i = 1:nrow(watershedMap@data), .packages = c("raster")) %dopar%
  divideLanduse(watershedMap[i,], rasterTemplate)  



divideLanduse <- function(watershedPoly, landUseRaster){
  crop(landUseRaster, watershedPoly)
}
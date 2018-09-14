# Load Libraries---------------------------------------------------------------
library("raster")
library("rgdal")
library(data.table)
library(tidyverse)

# Load Maps--------------------------------------------------------------------
stateMap <- readOGR("F:/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                    layer = "tl_2016_us_state")
stateMap <- stateMap[stateMap$STATEFP == "08",]
watershedMap <- readOGR("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")
watershedMap$origArea <- area(watershedMap)
forestTypes <- raster("f:/Documents/ACES/Maps/forestTypes/conus_foresttype.img")

# Reproject and crop
stateMap <- spTransform(stateMap, crs(forestTypes))
watershedMap <- spTransform(watershedMap, crs(forestTypes))
watershedMap <- crop(watershedMap, stateMap)
watershedMap$co_area <- area(watershedMap)

forestTypes <- crop(forestTypes, stateMap)
forestByWatershed <- raster:::extract(forestTypes, watershedMap)
watershedNames <- watershedMap@data$NAME
watershedForest <- vector("list", length(forestByWatershed))
for(i in 1:length(forestByWatershed)){
  temp <- table(forestByWatershed[[i]]) %>% data.table()
  setnames(temp, c("id", "count"))
  total = sum(temp$cou)
  temp[, proportion := count / sum(count)]
  temp[, watershed := watershedNames[i]]
  watershedForest[[i]] <- temp
}
watershedForest <- rbindlist(watershedForest)
watershedForest[, forested := ifelse(id == 0, F, T)]
percentForest <- watershedForest %>% group_by(watershed, forested) %>%  summarise (n = sum(count)) %>%
  mutate(freq = n / sum(n))
percentForest <- percentForest %>% left_join(watershedMap@data %>% select(NAME, origArea, co_area), by = c("watershed" = "NAME"))
percentForest %<>% mutate(percentCO = co_area / origArea)

names(watershedKey) <- as.character(watershedMap@data$HUC8)

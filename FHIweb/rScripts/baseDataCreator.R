watershedMap <- readOGR("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")
waterShedsCO <- data.table("name" = watershedMap@data$NAME,
                           "HUC8" = watershedMap@data$HUC8)
saveRDS(waterShedsCO, "fhiweb/data/basedata/waterShedsCO.RDS")


# Load Maps--------------------------------------------------------------------
stateMap <- readOGR("F:/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                    layer = "tl_2016_us_state")
stateMap <- stateMap[stateMap$STATEFP == "08",]

rasterTemplate <- raster("FHIweb/data/dayMetAnn/daymet_v3_prcp_annttl_1980_na.nc4")




# Reproject and crop
stateMap <- spTransform(stateMap, crs(rasterTemplate))
watershedMap <- spTransform(watershedMap, crs(rasterTemplate))
rasterTemplate <- crop(rasterTemplate, stateMap)
rasterTemplate <- setValues(rasterTemplate, 1:280578)
watershedKey <- extract(rasterTemplate, watershedMap)
names(watershedKey) <- as.character(watershedMap@data$HUC8)
saveRDS(watershedKey, "fhiweb/data/basedata/watershedKey.RDS")

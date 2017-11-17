library(gdalUtils)

baseURL <- "http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/GeoTIFF_30arcsec/MOD17A3_Science_NPP_"
years <- 2000:2015
nppTifs <- vector("list", length(years))
fileExt <- ".tif"
colo_ws <- st_read("c:/Users/admcc/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp")
colo_ws <- st_transform(colo_ws, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
colo_ws <- as(colo_ws, "Spatial")
fileLocation <- "c:/Users/admcc/Documents/ACES/FHI/FHIweb/data/NPP/globalNPP/npp"
for(i in 1:length(years)){
  download.file(url = paste0(baseURL, years[i], fileExt), 
                        destfile = paste0(fileLocation, 
                                          years[i], fileExt), mode = "wb"
                        )
  rast <- raster(paste0(fileLocation, years[i], fileExt)) 
  nppTifs[[i]] <- projectRaster(crop(rast, colo_ws), crs = globalCRS)
}
names(nppTifs) <- years

watershedMap <- spTransform(watershedMap, crs(nppTifs[[1]]))
nppTable <- vector("list", length(years))
# calclualte watershed total productivity
for(i in 1:length(years)){
  rastList <- extract(nppTifs[[i]], watershedMap)
  rastList <- lapply(rastList, FUN = function(x){
    data.table(totalNPP = sum(x, na.rm = T), year = years[i])
  }) %>% rbindlist()
  nppTable[[i]] <- cbind(rastList, watershedMap@data[, c("NAME", "HUC8")])
}

nppTable <- merge(nppTable %>% filter(year <= 2012) %>% 
                    group_by(HUC8) %>% summarise(meanNPP = mean(totalNPP) ), 
                  nppTable, by = "HUC8", all.y = T) %>% data.table()
nppTable[, c("anomoly", "anomolyPer") := list(totalNPP - meanNPP, (totalNPP - meanNPP) / meanNPP)]

nppTable <- rbindlist(nppTable)


save(nppTifs, file = "fhiweb/data/NPP/npp_colo.tif")

# Code to Read NPP files shouldn't need to be used again---------------------

nppPath <- "FHIweb/data/NPP/"
nppFiles <- dir(nppPath)
nppPre <- "HDF4_EOS:EOS_GRID:"
nppPost <- ":MOD_Grid_MOD17A3:Npp_1km"
rfv <- st_read("c:/Users/admcc/Documents/ACES/Maps/watershed/rfv.shp")
rfv <- st_transform(rfv, proj4string(rast))
rfvSP <- as(rfv, "Spatial")

origWD <- getwd()
setwd(nppPath)
for(i in 1:length(nppFiles)){
  year <- as.numeric(substr(nppFiles[i], 10, 13))
  gdal_translate(paste0(nppPre, nppFiles[i], nppPost), 
                 dst_dataset = paste0("npp", year, ".tif"))
}
setwd(origWD)

years <- 2000:2015
nppList <- lapply(years, FUN = function(x){
  rast <- raster(paste0(nppPath, "npp", x, ".tif"))
  rastValues <- extract(rast, rfvSP)
  return(rastValues[[1]])
})
names(nppList) <- years

nppTable <- lapply(nppList, FUN = function(x){
  data.table("NPP" = sum(x, na.rm = T), "year" = 1)
})
nppTable <- nppTable %>% rbindlist()
nppTable[, year := years]
nppTable[, anomoly := NPP - mean(NPP[1:14])]
nppTable[, anomolyPer := anomoly / NPP]

write.csv(nppTable, file = "FHIweb/data/NPP/npp.csv", row.names = F)

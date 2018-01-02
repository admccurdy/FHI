library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)



uniqueDays <- unique(nppDT[, yearDay])


results <- foreach(j = 1:length(uniqueDays), 
                   .packages = c("data.table", "rgdal", "gdalUtils")) %dopar%{
  x <- uniqueDays[j]
  current <- nppDT[yearDay == x,]
  
  for(i in 1:nrow(current)){
    sds <- get_subdatasets(paste0(nppDir, current[i, file]))
    gdal_translate(sds[2], dst_dataset = paste0(destDir, x, "_", i, ".tif"))
  }
  
  mosaic_rasters(gdalfile = paste0(destDir, x, "_", 1:nrow(current), ".tif"), 
                 dst_dataset = paste0(destDir, "/mergeTif/", x, "merged", ".tif"), of = "GTiff")
  file.remove(paste0(destDir, x, "_", 1:nrow(current), ".tif"))
}

mergeNPP <- dir(paste0(destDir, "/mergeTif/"))
nppYears <- unique(substr(mergeNPP, 1, 4))

rasterResults <- foreach(i = 1:length(nppYears), .packages = c("data.table", "rgdal", "gdalUtils", "raster")) %dopar% {
  rasterNames <- mergeNPP[substring(mergeNPP, 1, 4) == nppYears[i]]
  myRasters <-  lapply(rasterNames, FUN = function(x){
    raster(paste0(destDir, "/mergeTif/", x))
  })
  myRasters <- stack(myRasters)
  myRasters <- calc(myRasters, fun = sum)
}                                          

# rfvSin <- spTransform(rfvSP, crs(rasterResults[[1]]))
nppSums <- foreach(i = 1:length(rasterResults), .packages = c("data.table", "raster")) %dopar%{
  rfvRast <- extract(rasterResults[[i]], rfvSin)[[1]]
  sum(rfvRast)
}

nppTableCalc <- data.table("npp" = unlist(nppSums)[2:17], "years" = nppYears[2:17])
ggplot(nppTableCalc, aes(x = years, y = npp, group = 1)) + geom_line()

lapply(unique(nppDT[, yearDay]), FUN = function(x){
  print(x)
  current <- nppDT[yearDay == x,]
  for(i in 1:nrow(current)){
    sds <- get_subdatasets(paste0(nppDir, current[i, file]))
    gdal_translate(sds[2], dst_dataset = paste0(destDir, i, ".tif"))
  }
  rasterNames <- dir(destDir)
  myRasters <- lapply(rasterNames, FUN = function(x){
    raster(paste0(destDir, x))
  })
  myRasters$fun <- mean
  returnRaster <- do.call(merge, myRasters)
  crs(returnRaster) <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  returnRaster <- crop(returnRaster, watershedMap)
  returnRaster <- projectRaster(returnRaster, crs = myCRS)
  returnRaster <- crop(returnRaster, watershedMap_orig)
  writeRaster(returnRaster, paste0("FHIweb/data/NPP_tif//", current[1, yearDay], ".tif"))
})

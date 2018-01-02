library(gdalUtils)
library(RSelenium)
library(XML)
library(rts)
library(httr)

baseURL <- "http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/GeoTIFF_30arcsec/MOD17A3_Science_NPP_"
years <- 2000:2015
nppTifs <- vector("list", length(years))
fileExt <- ".tif"
colo_ws <- st_read("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp")
colo_ws <- st_transform(colo_ws, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
colo_ws <- as(colo_ws, "Spatial")
fileLocation <- "F:/Documents/ACES/FHI/FHIweb/data/NPP/globalNPP/npp"
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
rfv <- st_read("F:/Documents/ACES/Maps/watershed/rfv.shp")
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
saveRDS(nppTable, "FHIweb/data/NPP/npp.RDS")

url <- "https://e4ftl01.cr.usgs.gov/MOLT/MOD17A2H.006/2001.05.01/BROWSE.MOD17A2H.A2001121.h25v09.006.2015144095009.2.jpg"
download.file(url = url, destfile = "temp.hdf")


auth <- function() {
  # authentication function for any GET requests
  httr::authenticate(user = "admccurdy",
                     password = "Adam1986")
}
### New

nppPath <- "FHIweb/data/NPP2/"
h <- c("09", "10")
v <- c("05", "04")

##Fire up Selenium
driver <- rsDriver(port = as.integer(4443))
remDr <- driver[["client"]]


##Start and navigate to the site
address <- "https://search.earthdata.nasa.gov/granules/download.html?project=5881302648&collection=C203669722-LPDAAC_ECS"
remDr$navigate(address)
webElems <- remDr$findElements("css", "[href]")
urls_loc <- unlist(sapply(webElems, function(x){x$getElementAttribute("href")}))
for(i in urls_loc){
  GET(i, write_disk("F:/Documents/ACES/FHI/temp3.hdf"), auth())
}

watershedMap_orig <- readOGR(dsn = "F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")
watershedMap <- spTransform(watershedMap_orig, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
myCRS <- crs(watershedMap_orig)
nppDir <- "FHIweb/data/NPP3/"
destDir <- "F:/tempTIF/"
nppFiles <- dir(nppDir)
nppDT <- data.table("file" = nppFiles,
                    "year" = substr(nppFiles, 11, 14),
                    "DoY" = substr(nppFiles, 15, 17))
nppDT[, yearDay := paste0(year, DoY)]


nppDT <- nppDT[year %in% c(2001:2011),]
lapply(unique(nppDT[, yearDay]), FUN = function(x){
  print(x)
  current <- nppDT[yearDay == x,]
  
  for(i in 1:nrow(current)){
    sds <- get_subdatasets(paste0(nppDir, current[i, file]))
    gdal_translate(sds[2], dst_dataset = paste0(destDir, x, "_", i, ".tif"))
  }
  

})











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


mosaic_rasters(gdalfile = paste0(destDir, rasterNames), dst_dataset = "2007113Merge.tif", of = "GTiff")
gdalwarp("2007113Merge.tif", "2007113repro.tif", t_srs = globalProj4Str)

nppTifDir <- "fhiweb/data/NPP_tif/"
nppTifs <- dir(nppTifDir)
nppYears <- unique(substr(nppTifs, 1, 4))

npp_annual <- lapply(nppYears, FUN = function(x){
  tifFiles <- nppTifs[substr(nppTifs, 1, 4) == x]
  myRasters <- lapply(tifFiles, FUN = function(y){
    raster(paste0(nppTifDir, y)) 
  }) %>% stack() %>% calc(fun = sum)
})

## This is a test

nppYears <- 2007:2011
gpp_Test <- lapply(nppYears, FUN = function(x){
  tifFiles <- nppTifs[substr(nppTifs, 1, 4) == x]
  myRasters <- lapply(tifFiles, FUN = function(y){
    raster(paste0(nppTifDir, y)) 
  }) %>% stack()
})

writeRaster(npp_annual[[1]], filename = "nppCalc.tif")


names(npp_annual) <- nppYears

rfvNPP <- lapply(npp_annual, extract, y = rfvSP)
rfvNPP <- data.table("year" = names(npp_annual), "npp" = sapply(rfvNPP, FUN = function(x)sum(x[[1]])))

rfv <- st_read("F:/Documents/ACES/Maps/watershed/rfv.shp")
rfv <- st_transform(rfv, proj4string(npp_annual[[1]]))
rfvSP <- as(rfv, "Spatial")

origWD <- getwd()
setwd(nppPath)
for(i in 1:length(nppFiles)){
  year <- as.numeric(substr(nppFiles[i], 10, 13))
  gdal_translate(paste0(nppPre, nppFiles[i], nppPost), 
                 dst_dataset = paste0("npp", year, ".tif"))
}

b1 <- getHdf(product = "MOD17A2H", begin = "2016.12.01", end = "2017.01.01",
             tileH = 18:19, tileV = 4)

ModisDownload()
ModisDownload(x=x,h= 17, v = 5,dates='2011.05.01',mosaic=F,proj=F)

z <- mosaicHDF(paste0(nppDir, current[, file]), "new.hdf", 
               bands_subset = 2)
getSds("FHIweb/data/npp2/MOD17A2H.A2017313.h09v04.006.2017325172357.hdf")
z <- raster(readGDAL(,as.is = T ))



library(raster)
library(ncdf4)
library(magrittr)
library(data.table)
library(rgdal)
library(rvest)
source("FHIweb/rScripts/functionScripts/dayMetFunctions.R")

# Retrivie a list of the years available for download
catalogURL <- "https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1328/tiles/catalog.html"
years <- read_html(catalogURL) %>% html_nodes("table") %>% html_nodes("a") %>% html_text() %>% 
  substr(1,4) %>% as.numeric()
years <- years[!is.na(years)]
elements <- c("tmax", "tmin")

destDir <- "F:/Documents/ACES/FHI/FHIweb/data/dayMetDay/"

for(i in elements){
  for(j in years){
    if(!file.exists(dayMetFileName(destDir, i, j))){
      dlDayMetTile(tiles = coTiles, element = i, year = j, destDir = destDir)  
    }
  }
}

# Create Raster Template, if no files have been downloaded this isn't going to work
rasterTemplate <- raster(file.path(destDir, "1980_tmax.tif"))
rasterTemplate <- setValues(rasterTemplate, 1:ncell(rasterTemplate))
watershedMap <- readOGR("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
                        layer = "wbdhu8_a_co")
watershedMap <- spTransform(watershedMap, crs(rasterTemplate))
watershedKey <- extract(rasterTemplate, watershedMap)
names(watershedKey) <- as.character(watershedMap@data$HUC8)
watershedKeyT <- vector("list", length(watershedKey))
for(i in 1:length(watershedKey)){
  watershedKeyT[[i]] <- data.table(grids = watershedKey[[i]], HUC8 <- names(watershedKey)[i])
}

# Process dayMet Files
files <- grep(pattern = ".xml", x = dir(destDir), value = T, invert = T)
# files <- data.table(year = substr(files, 1, 4), element = substr(files, 6, 9))
dayMetDT <- lapply(files, function(x){
  myRaster <- stack(paste0(destDir, x))
  returnData <- lapply(1:nlayers(myRaster), function(y){
    rasterToDT(rasterTemplate, myRaster[[y]], year = substr(x, 1, 4), day = y,
               element = substr(x, 6, 9))
  })
  return(rbindlist(returnData))
})

fileList <- vector("list", length(files))
for(i in 1:length(files)){
  currentFile <- files[i]
  myRaster <- stack(paste0(destDir, currentFile))
  returnData <- lapply(1:nlayers(myRaster), function(y){
    rasterToDT(rasterTemplate, myRaster[[y]], year = substr(currentFile, 1, 4), day = y,
               element = substr(currentFile, 6, 9))
  })
  fileList[[i]] <- rbindlist(returnData)
}

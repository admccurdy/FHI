# Create a list of colorado tiles
coTiles <- c(11736:11739)
coTiles <- c(coTiles, coTiles - 180, coTiles -360)

# Load a map of colorado and convert to daymet projection
rasterCRS <- "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +lat_2=60 +ellps=WGS84"
coMap <- readOGR("F:/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                 layer = "tl_2016_us_state")
coMap <- coMap[coMap$STATEFP == "08",]
coMap <- spTransform(coMap, CRS(rasterCRS))

dlDayMetTile <- function(tiles, year, element, destDir){
  # Downloads a set of daily daymet tiles for a specific year an element
  # merges the tiles and then saves the result rater to the dest drive
  #
  # Args:
  #   tiles: vector of tile numbers to be downloaded. see:
  #     https://daymet.ornl.gov/gridded/ to find desired tiles #s
  #   year: year to download currently only able to handle a single year
  #   element: element to download should be one of tmax, prcp, srad, swe, tmin, vp
  #   destDir: file path to save merged rasters
  #
  # Returns:
  #   nothing
  validElements <- c("tmax", "prcp", "srad", "swe", "tmin", "vp")
  fileExt <- ".nc"
  if(!tolower(element) %in% validElements){
    print("invalid element, element must be one of the following:")
    print(validElements %>% paste(sep = " "))
  }
  baseURL <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/tiles/"
  myRasters <- lapply(tiles, function(x){
    url <- paste0(baseURL, year, "/", x, "_", year, "/", element, fileExt)  
    fileName <- paste0(x, fileExt)
    completed <- F
    whileCounter <- 0
    while(!completed){
      completed <- tryCatch({
        download.file(url, fileName, mode = "wb", quiet = T)
        TRUE
      }, warning = function(w){
        FALSE
      }, error = function(e){
        Sys.sleep(5)
        FALSE
      })
      whileCounter <- whileCounter + 1
      if(whileCounter >= 5)completed <- T
    }
    return(stack(fileName))
  })
  mergeRaster <- do.call(merge, myRasters)
  mergeRaster <- crop(mergeRaster, coMap)
  writeRaster(mergeRaster, dayMetFileName(destDir, element, year), overwrite = T)
  file.remove(paste0(tiles, ".nc"))
}

dayMetFileName <- function(directory, element, year){
  return(file.path(directory, paste0(year, "_", element, ".tif")))
}

rasterToDT <- function(gridRaster, dataRaster, ...){
  additionalVals <- list(...)
  if(any(additionalVals %>% names() %>% sapply(nchar) == 0)){
    stop("Additional arguments must be named")
  }
  returnTable <- data.table(grid = as.vector(rasterTemplate), value = as.vector(dataRaster))
  for(i in names(additionalVals)){
    returnTable[, (i) := additionalVals[[i]]]
  }
  return(returnTable)
}



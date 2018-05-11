library(ncdf4)
library(raster)
library(plyr)
library(magrittr)
library(lubridate)
library(rvest)
library(rgdal)
library(dplyr)

fileBase <- "_37V_CO_FT_"
SMMR <- "SMMR"
SSMI <- "SSMI"
fileExt <- ".tif"
myDir <- "FHIweb/data/ffDays"
rasterCRS <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs"
urlBase <- "http://luna.ntsg.umt.edu/data/FT_ESDR/DAILY_GEOTIFF/"
coMap <- readOGR("F:/Documents/ACES/Maps/State/tl_2016_us_state.shp",
                    layer = "tl_2016_us_state")
coMap <- coMap[coMap$STATEFP == "08",]
coMap <- spTransform(coMap, CRS(rasterCRS))

tifPage <- read_html(paste0(urlBase, SSMI))
availableYears <- tifPage %>% html_nodes("table") %>% html_nodes("a") %>% html_text()
availableYears %<>% as.numeric() %>% na.omit()

startYear <- 1979
endYear <- max(availableYears)
dir.create(file.path(myDir), showWarnings = F)

if(dir(myDir) %>% length() == 0){
  for(i in startYear:endYear){
    lastDay <- ifelse(leap_year(i), 366, 365)
    for(j in 1:lastDay){
      dlFrostDay(i, j)
    }
  }
}else{
  files <- dir(myDir)
  dates <- tibble(years = substr(files, 1, 4) %>% as.numeric(), days = substr(files, 6, 8) %>% as.numeric()) %>%
    group_by(years) %>% summarize(days = max(days))
  datesNeeded <- tibble(years = max(dates$years):endYear, start = 1)
  datesNeeded %<>% mutate(end = ifelse(leap_year(years), 366, 365))
  datesNeeded[1, "start"] <- dates[which.max(dates$years), "days"]
  for(i in datesNeeded$years){
    start <- datesNeeded %>% filter(years == i) %>% pull(start)
    end <- datesNeeded %>% filter(years == i) %>% pull(end)
    for(j in start:end){
      dlFrostDay(i, j)
    }
  }
}

dlFrostDay <- function(year, day){
  dataSource <- ifelse(year <= 1986, SMMR, SSMI)
  day <- formatC(day, width = 3, format = "d", flag = "0")
  url <- paste0(urlBase, dataSource, "/", year, "/", dataSource, fileBase, year, "_day", day, fileExt)
  fileName <- paste0("temp.", fileExt)
  download.file(url, fileName, mode = "wb")
  myRaster <- raster(fileName)
  crs(myRaster) <- rasterCRS
  myRaster <- crop(myRaster, coMap)
  writeRaster(myRaster, file.path(myDir, paste0(year, "_", day, fileExt)), options = "COMPRESS=DEFLATE", overwrite = T)
  file.remove(fileName)
  Sys.sleep(1)
}

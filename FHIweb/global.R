library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(odbc)
library(DBI)
library("ncdf4")
library("raster")
library("rgdal")
library(MASS)
library(rgeos)
library(sf)
library(leaflet)

source("rScripts/scoreMod.R")
source("rScripts/FHI_functions.R")

years <- 1980:2016


# Load SQL Connection----------------------------------------------------------
dbiConn <- dbConnect(odbc(), 
                     driver = "SQL Server",
                     server = "tcp:foresthealthindex.database.windows.net,1433",
                     database = "FHI", 
                     uid = "amccurdy@foresthealthindex",
                     pwd = "Aces2k12$")

# Load Maps--------------------------------------------------------------------
# stateMap <- readOGR("c:/Users/admcc/Documents/ACES/Maps/State/tl_2016_us_state.shp",
#                     layer = "tl_2016_us_state")
# stateMap <- stateMap[stateMap$STATEFP == "08",]
# watershedMap <- readOGR("c:/Users/admcc/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp",
#                         layer = "wbdhu8_a_co")
# rasterTemplate <- raster("FHIweb/data/dayMetAnn/daymet_v3_prcp_annttl_1980_na.nc4")
# 
# # Reproject and crop
# stateMap <- spTransform(stateMap, crs(rasterTemplate))
# watershedMap <- spTransform(watershedMap, crs(rasterTemplate))
# rasterTemplate <- crop(rasterTemplate, stateMap)
# rasterTemplate <- setValues(rasterTemplate, 1:280578)
# watershedKey <- extract(rasterTemplate, watershedMap)
# names(watershedKey) <- as.character(watershedMap@data$HUC8)
# waterShedsCO <- data.table("name" = watershedMap@data$NAME,
#                            "HUC8" = watershedMap@data$HUC8)
# 
# rs <- dbSendQuery(dbiConn, "SELECT * FROM userObs")
# existingData <- dbFetch(rs)
# load("FHIweb/data/insectData.RData")

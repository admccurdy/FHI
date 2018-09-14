library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(odbc)
library(DBI)
library("raster")
library("rgdal")
library(MASS)
library(rgeos)
library(sf)
library(leaflet)
library(ks)
library(dtplyr)


source("rScripts/scoreMod.R")
source("rScripts/methodMods.R")
source("rScripts/FHI_functions.R")
source("rScripts/shinyFunctions.R")

years <- 1980:2016
precipBase <- tempBase <- list("start" = 1980, "end" = 2000)
nppBase <- list("start" = 2000, "end" = 2010)
ercBase <- list("start" = 2000, "end" = 2010)
methodOptions <- list("Quantile" = "quant", "FHI" = "FHI", "Trend" = "trend", "Quantile-2" = "quant2")

# Load constants

# 1cfs for 24 hrs to acre ft
# http://water.nv.gov/programs/planning/dictionary/convert2.pdf
cfs_acreFt <- 1.9835

# load static data
nppTable <- readRDS("data/NPP/npp.RDS")
snoTelKey <- readRDS("data/snotel/snoTelKey.RDS")
snoMetrics <- readRDS("data/snotel/snoMetrics.RDS") # Load in snotel data
dayMetCO <- readRDS("data/dayMetAnn/dayMet.RDS")
coERC <- readRDS("data/erc/coERC.rds")
ercStationKey <- readRDS("data/erc/ercStationKey.rds")
waterShedsCO <- readRDS("data/baseData/waterShedsCO.RDS")
gaugeKey <- readRDS("data/streamflow/coGaugeSites.rds")
dailyDischarge <- readRDS("data/streamflow/coDailyFlow.rds")
watershedKey <- readRDS("data/baseData/watershedKey.RDS")
insectSF2 <- readRDS("data/insects/insectData.RDS")
clipShapeDT <- readRDS("data/insects/clipShapeDT.RDS")
avgAnDis <- readRDS("data/streamflow/avgAnDis.rds")
coPeak <- readRDS("data/streamflow/coPeak.rds")
gaugeSites <- readRDS("data/streamflow/coGaugeSites.rds")

# Load SQL Connection----------------------------------------------------------
# dbiConn <- dbConnect(odbc(),
#                      driver = RSQLServer::SQLServer(),
#                      server = "tcp:foresthealthindex.database.windows.net,1433",
#                      database = "FHI",
#                      uid = "amccurdy@foresthealthindex",
#                      pwd = "Aces2k12$")
#
# rs <- dbSendQuery(dbiConn, "SELECT * FROM userObs")
# existingData <- dbFetch(rs)
# existingData <- data.table(matrix(0, ncol = 5, nrow = 5))
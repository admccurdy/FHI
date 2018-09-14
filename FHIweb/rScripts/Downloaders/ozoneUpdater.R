library(rvest)
library(lubridate)
library(data.table)
library(dplyr)
library(magrittr)
library(readr)
library(snowfall)
library(dtplyr)
source("F:/Documents/ACES/FHI/FHIweb/rScripts/FHI_functions.R")
source("F:/Documents/ACES/FHI/FHIweb/rScripts/processFunctions.R")

currentOzone <- readRDS("F:/Documents/ACES/FHI/fhiweb/data/webOzone/rawOzone.RDS")
ozoneStations <- readRDS("F:/Documents/ACES/FHI/fhiweb/data/webOzone/ozoneStations.RDS")
aqsKey <- read_csv("F:/Documents/ACES/FHI/FHIweb/data/webOzone/aqStations.csv")
aqsKey %<>% mutate(aqsID = formatC(aqsID, width = 7, format = "d", flag = "0"))

co_aqsKey <- right_join(ozoneStations %>% as_tibble(), aqsKey, c("uniqueID" = "aqsID")) %>% data.table()

maxAspen <- max(currentOzone[uniqueID == "0970007", date])
dlDays <- seq(from = maxAspen, to = (Sys.Date() - 1) %>% as_date(), by = 1)

sfInit(parallel = T, 6)
sfLibrary(data.table)
sfLibrary(lubridate)
sfLibrary(magrittr)
sfLibrary(rvest)
sfLibrary(readr)
sfLibrary(dplyr)
sfExport("twoDigit")

newOzone <- sfLapply(dlDays, fun = dlOzone)

newOzone <- rbindlist(newOzone)
setnames(newOzone, "variable", "nameCO")
newOzone <- left_join(newOzone, co_aqsKey)
setnames(newOzone, c("hour", "value"), c("time", "O3_ppm"))
newOzone[, c("state", "county", "lat", "long", "stationName", "nameCO", "FIPS_s", "FIPS_c", "siteNum") := NULL]
newOzone %<>% mutate(day = day(date), month = month(date), year = year(date), O3_ppm = O3_ppm/1000, flag = "")
newOzone <- rbindlist(list(currentOzone, newOzone), use.names = T)
newOzone <- unique(newOzone)

plantOzone <- o3_wrapper(newOzone)

saveRDS(newOzone, "F:/Documents/ACES/FHI/fhiweb/data/webOzone/rawOzone.RDS")
saveRDS(plantOzone, "F:/Documents/ACES/FHI/fhiweb/data/webOzone/ozoneMetrics.RDS")


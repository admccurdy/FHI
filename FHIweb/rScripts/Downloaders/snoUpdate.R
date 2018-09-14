library(data.table)
library(dplyr)
library(magrittr)
library(RCurl)
library(XML)
library(rvest)
library(lubridate)
library(readr)
library(dtplyr)
source("R/coopFunctions.R")

# Load Previous Data
snoMetrics <- readRDS("FHIweb/data/snotel/snoMetrics.RDS")
snoKey <- readRDS("FHIweb/data/snotel/snoTelKey.RDS")
siteURL <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/"
fileExt <- ".csv.gz"
ftpList <- getURL(siteURL, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
years <- strsplit(ftpList, "\r*\n")[[1]] %>% substr(1,4) %>% as.numeric() %>% na.omit()

currentYear <- year(Sys.Date())
newSno <- loadYears(snoKey$id, c((currentYear - 2):currentYear))
newSno %<>% snoProcess()

#Merge Max
newMax <- rbindlist(list(snoMetrics$max[!(waterYear %in% unique(newSno$max$waterYear))], 
                         newSno$max),
                    use.names = T)
newApril <- rbindlist(list(snoMetrics$april[!(waterYear %in% unique(newSno$april$waterYear))],
                           newSno$april),
                      use.names = T)
snoMetrics <- list(max = newMax, april = newApril)
saveRDS(snoMetrics, "FHIweb/data/snotel/snoMetrics.RDS")

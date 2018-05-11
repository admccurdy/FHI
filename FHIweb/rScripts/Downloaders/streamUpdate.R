library(dataRetrieval)
library(data.table)
library(dplyr)
library(rgeos)
library(sp)
library(rgdal)
library(magrittr)

currentDaily <- readRDS("FHIweb/data/StreamFlow/coDailyFlow.rds")
gaugeKey <- readRDS("FHIweb/data/StreamFlow/coGaugeSites.rds")
currentDaily <- currentDaily[!(grepl("P ", qualityFlag, ignore.case = F) | 
                                 grepl("P", qualityFlag, ignore.case = F, fixed = T))]
dailyLimit <- currentDaily[, .(maxDate = max(date)), by = site_no]
newDaily <- vector("list", length = nrow(dailyLimit))
for(i in 1:nrow(dailyLimit)){
  newDaily[[i]] <- readNWISdv(dailyLimit[i, site_no], 
                         parameterCd = parameter, 
                         startDate = dailyLimit[i, maxDate]) %>% data.table()
}

newDaily %<>% rbindlist() %>% processNWIS()

coDaily <- rbindlist(list(currentDaily, newDaily), use.names = T)
avgAnDis <- dailyToAnnual(coDaily, gaugeKey)

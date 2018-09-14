library(dataRetrieval)
library(data.table)
library(dplyr)
library(rgeos)
library(sp)
library(rgdal)
source("FHIweb/rScripts/processFunctions.R")

watershedMapB <- readOGR("data/watershedMapB.shp", layer = "watershedMapB")
parameter <- c("00060")

coSites <- whatNWISsites(stateCD = "CO", parameterCd = parameter) %>% data.table()
coDaily <- readNWISdv(coSites$site_no, parameter) %>% data.table()


setkey(coDaily, "site_no")
siteRange <- coDaily %>% group_by(site_no) %>% summarise(startDate = min(Date), endDate = max(Date))
coSites <- coSites %>% inner_join(siteRange)
coSites <- coSites %>% filter(year(endDate) == 2017)
coDaily <- coDaily[.(coSites$site_no),]
coDaily <- coDaily %>% data.table()
coDaily %<>% processNWIS(coDaily)

setnames(coSites, c("dec_lat_va", "dec_long_va"), c("lat", "long"))
gaugeKey <- pointExtracter(coSites, watershedMapB)
gaugeKey <- gaugeKey[, c("site_no", "station_nm", "site_tp_cd", "lat", "long", "startDate", "endDate", "HUC8", "NAME")]

avgAnDis <- dailyToAnnual(coDaily, gaugeKey)

coPeak2 <- readNWISpeak(gaugeKey$site_no) %>% data.table()
coPeak %<>% peakProcess(gaugeSites = gaugeKey)

saveRDS(gaugeKey, "FHIweb/data/StreamFlow/coGaugeSites.rds")
saveRDS(coDaily, "FHIweb/data/StreamFlow/coDailyFlow.rds")
saveRDS(coPeak, "FHIweb/data/streamflow/coPeak.rds")
saveRDS(avgAnDis, "FHIweb/data/streamflow/avgAnDis.rds")

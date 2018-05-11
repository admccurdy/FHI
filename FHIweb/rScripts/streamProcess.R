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

avgAnDis <- coDaily %>% filter(!is.na(discharge_cfs)) %>%group_by(site_no, year) %>% summarise(avgCFS = mean(discharge_cfs))
avgAnDis[, over1 := avgCFS > 1]
sites <- avgAnDis %>% group_by(site_no) %>% summarise(over1 = all(over1)) %>% filter(over1) %>% pull(site_no)

avgAnDis <- avgAnDis[site_no %in% sites, ][,over1 := NULL]
avgAnDis <- avgAnDis %>% left_join(gaugeSites)
avgAnDis <- avgAnDis[, .(site_no, year, station_nm, avgCFS, HUC8)]
setnames(avgAnDis, c("station_nm", "avgCFS"), c("name", "value"))
avgAnDis <- as.data.table(avgAnDis)
setkey(avgAnDis, HUC8, site_no)

coPeak <- readNWISpeak(coSites$site_no) %>% data.table()
coPeak <- coPeak[site_no %in% gaugeKey$site_no,]
coPeak <- coPeak[, list(site_no, "date" = peak_dt, "discharge_cfs" = peak_va,
                        "year" = year(peak_dt), day = strftime(peak_dt, format = "%j"))]
coPeak <- coPeak %>% left_join(gaugeSites)
coPeak <- coPeak[, .(site_no, year, value = day, name = station_nm, HUC8)]
coPeak <- coPeak[!is.na(value),]
coPeak <- as.data.table(coPeak)
coPeak[, value := as.numeric(value)]

setkey(coPeak, site_no, HUC8)

saveRDS(gaugeKey, "FHIweb/data/StreamFlow/coGaugeSites.rds")
saveRDS(coDaily, "FHIweb/data/StreamFlow/coDailyFlow.rds")
saveRDS(coPeak, "FHIweb/data/streamflow/coPeak.rds")
saveRDS(avgAnDis, "FHIweb/data/streamflow/avgAnDis.rds")

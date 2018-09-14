library(dataRetrieval)
library(data.table)
library(dplyr)
library(magrittr)

# Read in previously downloaded data--------------------------------
currentDaily <- readRDS("FHIweb/data/StreamFlow/coDailyFlow.rds")
gaugeKey <- readRDS("FHIweb/data/StreamFlow/coGaugeSites.rds")
coPeak <- readRDS("FHIweb/data/StreamFlow/coPeak.rds")
parameter <- c("00060")

# Download and Process and new peak data----------------------------
currentPeak <- coPeak[, .(maxYear = max(year)), by = site_no]
currentPeak[, maxYear := maxYear + 1]
newPeak <- vector("list", nrow(currentPeak))
for(i in 1:nrow(currentPeak)){
  newPeak[[i]] <- readNWISpeak(currentPeak[i, site_no],
                               startDate = paste(currentPeak[i, maxYear], "01", "01", sep = "-")) %>%
    data.table()
}
newPeak <- rbindlist(newPeak, use.names = T, fill = T)
newPeak <- newPeak[, -1]
newPeak %<>% peakProcess(gaugeSites = gaugeKey)
coPeak <- rbind(coPeak, newPeak)


currentDaily <- currentDaily[!(grepl("P ", qualityFlag, ignore.case = F) | 
                                 grepl("P", qualityFlag, ignore.case = F, fixed = T))]
dailyLimit <- currentDaily[, .(maxDate = max(date)), by = site_no]


newDaily <- vector("list", length = nrow(dailyLimit))
for(i in 1:nrow(dailyLimit)){
  newDaily[[i]] <- readNWISdv(dailyLimit[i, site_no], 
                         parameterCd = parameter, 
                         startDate = dailyLimit[i, maxDate]) %>% data.table()
}

newDaily %<>% rbindlist() 
newDaily %<>% processNWIS()
coDaily <- rbind(currentDaily, newDaily)
avgAnDis <- dailyToAnnual(coDaily, gaugeKey)

saveRDS(coPeak, "FHIweb/data/StreamFlow/coPeak.rds")
print(paste(nrow(newPeak), "Peak records updated"))
saveRDS(coDaily, "FHIweb/data/StreamFlow/coDailyFlow.rds")
print(paste(nrow(newDaily), "Daily streamflow records updated"))
saveRDS(avgAnDis, "FHIweb/data/streamflow/avgAnDis.rds")
print("average annual discharge records upaded with new dailys")
print(paste("All records updated on", Sys.time(), Sys.timezone()))
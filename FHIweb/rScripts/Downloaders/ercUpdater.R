source("FHIweb/rScripts/functionScripts/ercFunctions.R")

# Load existing Data
obsReports <- readRDS("fhiweb/data/ERC/coERC.RDS")
ercStationKey <- readRDS("FHIweb/data/erc/ercStationKey.RDS")
obsReports[, date := ymd(paste0(
  year, 
  month %>% twoDigit(), 
  day %>% twoDigit()))]
startDate <- max(obsReports$date) + 1
endDate <- Sys.Date() - 1
dateList <- seq(startDate, endDate, "day")

newObs <- lapply(dateList, function(x){
  myDay <- day(x) %>% twoDigit()
  myMonth <- month(x) %>% twoDigit()
  myYear <- year(x) %>% twoDigit()
    dayTable <- tryCatch({
      readERC(myYear, myMonth, myDay, ".txt")
    }, error = function(e){
      errorTable[errorIndex, c("year", "month", "day") := list(year(x), month(x), day(x)) %>% lapply(FUN = as.numeric)]
      errorIndex <<- errorIndex + 1
      print(e)
      NULL
    })
    return(dayTable)
})

newObs %<>% rbindlist()
newObs %<>% obsCleaner()
newObs <- newObs[, c("station", "ERC", "year", "month", "day")]
obsReports <- rbind(obsReports[, -"date"], newObs)
saveRDS(obsReports, "fhiweb/data/ERC/coERC.RDS")

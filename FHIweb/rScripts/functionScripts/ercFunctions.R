## Load Libraries
library(data.table)
library(Hmisc)
library(dtplyr)
library(dplyr)
library(rgeos)
library(lubridate)
source("FHIweb/rScripts/FHI_functions.R")
urlBase <- "http://www.wfas.net/archive/www.fs.fed.us/land/wfas/archive/"
fileName <- "fdr_obs"
widths <- c(26, 5, 5, 5, 5, 5, 4, 7, 6, 6, 5, 3, 6, 6, 6, 6, 5, 4, 19)

errorTable <- data.table(matrix(0, nrow = 6500, ncol = 3))
errorIndex <- 1
setnames(errorTable, c("year", "month", "day"))


stateTable <- data.table("name" = state.name, "abv" = state.abb)
stateTable <- rbind(stateTable, data.table("name" = c("Virgina", "West Virgina", "Puerto Rico", "US Virgin Islands", "Guam", "a", "b"), 
                                           "abv" = c("VA", "WV", "PR", "VI", "GU", "PR", "VI")))

tableCleaner <- function(dayTable){
  dayTable <- gdata::trim(dayTable)
  tableStart <- which(substr(dayTable$V1, 1, 3) == "***", arr.ind = T)[1]
  dayTable <- dayTable[tableStart:nrow(dayTable),]
  setnames(dayTable, c("Station", as.character(dayTable[1, 2:ncol(dayTable)])))
  dayTable[, State := ""]
  state <- NA
  i <- 1
  while(i <= nrow(dayTable)){
    currentRow <- dayTable[i,]
    if(is.na(currentRow$Station)){
      dayTable <- dayTable[-i,]
    }else if(substr(currentRow$Station, 1, 3) == "***"){
      state <- gsub("\\*", "", currentRow$Station) %>% gdata::trim()
      state <- stateTable[which.min(adist(state, stateTable$name)), abv]
      state <- ifelse(is.null(state), gsub("\\*", "", currentRow$Station) %>% gdata::trim(),
                      state)
      dayTable <- dayTable[-i,]
    }else{
      dayTable[i, State := state]
      i <- i + 1
    }
    
  }
  return(dayTable)
}


readERC <- function(year, month, day, ext){
  url <- paste0(urlBase, year, "/", month, "/", day, "/", fileName, ext)
  dayTable <- read.fwf(url, widths = widths, comment.char = "", stringsAsFactors = F) %>% data.table() %>% tableCleaner()
  dayTable[, c("year", "month", "day") := list(year, month, day) %>% lapply(FUN = as.numeric)]
  return(dayTable)
}

obsCleaner <- function(obsReports){
  obsReports <- obsReports[State == "CO",]
  obsReports[, stationName := gsub("\\d", "", Station)]
  obsReports[, Station := gsub("\\D+", "", Station)]
  obsReports[, Station := as.numeric(Station)]
  
  for(j in c("Elev", "Lat", "Long", "Tmp", "RH", "Wind", "PPT", "ERC", "BI", "SC", "KBDI",
             "HUN", "THOU", "TEN", "ADJ")){
    set(obsReports, j = j, value = as.numeric(obsReports[[j]]))
  }
  setnames(obsReports, "Station", "station")
  setkey(obsReports, station, year, month, day)
  return(obsReports)
}
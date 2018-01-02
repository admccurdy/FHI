findNearestCoop <- function(lat,long, coopTess, locTessKey){
  myPoint <- data.frame("long" = long, "lat" = lat)
  coordinates(myPoint) <- ~long + lat
  proj4string(myPoint) <- proj4string(coopTess)
  return(locTessKey[tess == (myPoint %over% coopTess), station])
}

retriveCOOPdata <- function(simpleName, stationList, stationTable){
  mySimpleName <- simpleName
  stations <- stationList[simpleName == mySimpleName,]
  if(nrow(stations) == 1){
    returnFrame <- stationTable[id == stations[1,id]]
  }else{
    primaryRecord <- which.max(stations$recordLen)
    if(stations[primaryRecord, last_year] == 2017 & stations[primaryRecord, recordLen] > 40){
      returnFrame <- stationTable[id == stations[primaryRecord, id]]
    }else{
      stations <- stations[order(recordLen, decreasing = TRUE)]
      yearsCaptured <- NULL
      i <- 1
      returnList <- vector("list", nrow(stations))
      while(!all(min(stations$first_year):max(stations$last_year) %in% yearsCaptured)){
        stationYears <- stations[i, first_year]:stations[i, last_year]
        stationYearsUsed <- stationYears[!stationYears %in% yearsCaptured]
        yearsCaptured <- c(yearsCaptured, stationYearsUsed)
        returnList[[i]] <- stationTable[id == stations[i, id] & year %in% stationYearsUsed]
        i <- i + 1
      }
      returnFrame <- rbindlist(returnList)
      
    }
  }
  returnFrame[, primaryStation := ifelse(id == stations[primaryRecord, id], TRUE, FALSE)]
  return(returnFrame)
}
  

findKeeperStations <- function(stationList){
  keepList <- NULL
  for(i in unique(stationList$simpleName)){ 
    stations <- stationList[simpleName == i,]
    firstYear <- min(stations$first_year)
    lastYear <- max(stations$last_year)
    if(lastYear == 2017 & firstYear < 1970){
      stationRecord <- NULL
      for(j in 1:nrow(stations)){
        stationRecord <- c(stationRecord, stations[j, first_year]:stations[j, last_year])
      }
      if(all(firstYear:lastYear %in% stationRecord)){
        positions <- grep(i, stationList$simpleName)
        longRecord <- any(grep(firstYear, stations$first_year) %in% grep(lastYear, stations$last_year))
        if(longRecord){
          keepList <- c(keepList, stationList[simpleName == i & 
                                                first_year == firstYear &
                                                last_year == lastYear, id])
        }else{
          keepList <- c(keepList, stationList[positions, id])
        }
      }
    }
  }
  return(keepList)
}

loadDLY <- function(IDs){
    myDir <- "F:/Documents/ACES/FHI/data/ghcnd_all/"
  # stationList <- dir(myDir)
  # dlList <- IDs[!paste0(IDs, ".dly") %in% stationList]
  # if(!is.null(dlList)){
    lapply(IDs, function(x){
      download.file(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", x, ".dly"), 
                    destfile = paste0(myDir, x, ".dly")) 
      })
  # }
  
  fileNames <- paste0(myDir, IDs, ".dly")
  myNames <- c("id", "year", "month", "element", gsub(" ", "", apply(expand.grid(c("value", "mflag", "qflag", "sflag"), 1:31), 1, 
                                                                     paste, collapse="", sep = ""), fixed = T))
  stationData <- lapply(fileNames, function(x){ 
    file <- LaF::laf_open_fwf(x, column_widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)),
                         column_types = c("character", "integer", "integer", "character", 
                                          rep(c("integer", "character", "character", "character"), 31)),
                         column_names = myNames)
    file <- file[,]
    return(data.table(file))
  })
  names(stationData) <- IDs
  return(stationData)
}

stationMedian <- function(stationData){
  medianData <- stationData[element %in% c("PRCP", "SNWD", "WESD"), .("median" = median(value, na.rm = T)), by = c("element", "day", "month")]
  medianData <- rbind(medianData, stationData[element %in% c("TAVG", "TMIN", "TOBS", "TMAX"), .("median" = mean(value, na.rm = T)), by = c("element", "day", "month")])
  stationData <- merge(stationData, medianData, by = c("element", "month", "day"))
  # setnames(stationData, c("value.x", "value.y"), c("value", "median"))
}

precipCumSum <- function(stationData){
  precip <- stationData[element == "PRCP",]
  precip <- precip[order(year,  month, day)]
  precip <- rbindlist(lapply(unique(precip$year), function(x){
    yearP <- rbind(precip[year == x & month >= 10,], precip[year == x + 1 & month < 10,])
    yearP <- yearP[!is.na(value), mySum := cumsum(value)]
  }))
  stationData <- merge(stationData, precip[, c("value", "id") := NULL], by = c("element", "year", "month", "day"), all.x = TRUE)
  stationData[, value := ifelse(is.na(mySum), value, mySum)]
  stationData[, mySum := NULL]
}

returnMeltedValues <- function(station, element = "all"){
  myElement <- element
  if(element != "all"){
    station <- station[element == myElement,]
  }
  station <- station[, names(station)[names(station) %in% c("id", "year", "month", "element", 
                                                            paste("value", 1:31, sep = ""))], with = F]
  station <- melt(station, id.vars = c("id", "year", "month", "element"))
  station[, variable := as.character(variable)]
  station[, variable := as.numeric(substr(variable, ifelse(nchar(variable)==6, nchar(variable), nchar(variable) - 1), nchar(variable)))]
  station[value == -9999, value := NA]
  setnames(station, "variable", "day")
}

stationPlotter <- function(stationData, elements, scale = F, startDate = "all", endDate = "all", title = "",
                           plotMedian = T, ylims = NULL){
  stationData <- stationData[element %in% toupper(elements),]
  if(scale){
    stationData[, value := value/median]
    stationData[is.infinite(value), value := 0]
  }
  
  if(is.null(ylims)){
    ylims <- c(min(stationData$value), max(stationData$value))
  }
  
  if(startDate == "all")startDate <- min(as.numeric(stationData$date))
  if(endDate == "all")endDate <- max(as.numeric(stationData$date))
  stationData[, date := paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                               ifelse(nchar(day) == 1, paste0("0", day), day))]
  stationData <- stationData[order(date),]

  
  plotData <- stationData[date <= endDate & date >= startDate & !is.na(value),]
  
  if(plotMedian){
    plotData <- rbindlist(list(plotData, stationData[!is.na(median) & date %in% (as.numeric(max(plotData$date)) - 9999):(endDate-10000), 
                                                     .(date, median, element)] ), use.names = T, fill = T)
    plotData[date <= (endDate - 10000), date := as.character(as.numeric(date) + 10000)]
    plotData[, datePrint := paste0(substr(date, 1, 4),"-", substr(date, 5,6), "-", substr(date, 7,8)) ]
    
    ggplot(plotData, aes(x =as.factor(datePrint), y = value, color = element)) + 
      geom_line(aes(group = element)) + geom_line(aes(y = median, group = element), linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = .5)) + 
      scale_x_discrete(breaks = plotData[seq(1, length(datePrint), by = 30), datePrint]) + 
      ylab("Inches of Precip/SWE") + xlab("Date") + ggtitle(title) + ylim(ylims)
  }else{
    plotData[, datePrint := paste0(substr(date, 1, 4),"-", substr(date, 5,6), "-", substr(date, 7,8)) ]
    ggplot(plotData, aes(x =as.factor(datePrint), y = value, color = element)) + 
      geom_line(aes(group = element)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = .5)) + 
      scale_x_discrete(breaks = plotData[seq(1, length(datePrint), by = 30), datePrint]) + 
      ylab("Inches of Precip/SWE") + xlab("Date") + ggtitle(title)+ ylim(ylims)
  }
}

tempToF <- function(stationData){
  stationData[, value := as.numeric(value)]
  stationData[element %in% c("TMAX", "TAVG", "TMIN"), value := (value/10 * 9/5) + 32]
}

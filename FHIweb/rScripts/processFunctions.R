pointExtracter <- function(point, polygon){
  if(!any(class(point) %in% c("SpatialPointsDataFrame", "SpatialPoints"))){
    point <- as.data.table(point)
    point <- SpatialPointsDataFrame(coords = point[, c("long", "lat")],
                                    data = point, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  if(proj4string(point) != proj4string(polygon)){
    point <- spTransform(point, raster::crs(polygon))
  }
  point$point.ID <- 1:nrow(point)
  pointKey <- raster::extract(polygon, point) %>% data.table()
  pointKey[, poly.ID := NULL]
  pointKey <- merge(pointKey, point, by = "point.ID")
  pointKey[, c("long.1", "lat.1") := NULL]
  return(pointKey)
}

# Ozone Functions-------------------------
completeness <- function(measures, date){
  expectedValues <- days_in_month(date[1]) * 12
  actualValues <- sum(measures)
  return(actualValues / expectedValues)
  
}

w126Calc_M <- function(O3_ppm, date, day, time){
  tempTable <- data.table(O3_ppm = O3_ppm, time = time, date = date, day = day)
  tempTable <- tempTable[time %in% 8:19 & !is.na(O3_ppm),]
  mDay <- days_in_month(date[1])
  completeness <- nrow(tempTable) / (mDay * 12)
  minVal <- max(min(tempTable$O3_ppm), .005)
  if(is.infinite(minVal)){
    tableLen <- ceiling(mDay * .75)
    tempTable <- data.table(O3_ppm = .005, time = rep(8:19, tableLen) %>% sort(), date = date[1], day = rep(1:tableLen, 12))
    completeness <- nrow(tempTable) / (mDay * 12)
  }
  while(completeness < .75){
    missingDays <- setdiff(1:mDay, tempTable$day)
    if(length(missingDays) != 0){
      tempTable <- rbind(tempTable, data.table(O3_ppm = minVal, time = 8:19, 
                                               date = as.Date(paste0(year(date[1]), "-", month(date[1]), "-", missingDays[1])),
                                               day = missingDays[1]))
    }else{
      dayCounts <- tempTable[, j = list(count = .N), by = day][count < 12,]
      currentDay <- tempTable[day == dayCounts[1, day]]
      missingHours <- setdiff(8:19, currentDay[, time])
      tempTable <- rbind(tempTable, data.table(O3_ppm = minVal, time = missingHours, date = currentDay[1, date],
                                               day = currentDay[1, day]))
    }
    completeness <- nrow(tempTable) / (mDay * 12)
  }
  tempTable[,w126 := (O3_ppm * (1 / (1 + (4403 * exp(-126 * O3_ppm)))))]
  return(list("w126M" = sum(tempTable$w126), "completeness" = completeness))
}

w126Calc_D <- function(ozoneTable){
  ozoneTable <- ozoneTable[time %in% c(8:19),]
  ozoneTable[,w126 := (O3_ppm * (1 / (1 + (4403 * exp(-126 * O3_ppm)))))]
  return(data.table(date = ozoneTable$date[1], w126 = sum(ozoneTable$w126), dataPoints = nrow(ozoneTable)))
}
readCoAO3 <- function(filePath){
  columns <- tryCatch({
    print(filePath)
    mySheet <- read_excel(filePath, sheet = "Source", skip = 1)
    return(ncol(mySheet))
  }, warning = function(w){
    mySheet <- read_excel(filePath, sheet = "Source", skip = 1)
    return(ncol(mySheet))
  }, error = function(e){
    return(-1)
  })
  
  if(columns > 0){
    print(columns)
    mySheet <- read_excel(filePath, sheet = "Source", skip = 1, 
                          col_types = c("date", rep("text", columns - 1)))
    mySheet <- mySheet %>% dplyr::select(-matches("X")) %>% dplyr::select(-3)
    mySheet <- as.data.table(mySheet)
    setnames(mySheet, c("date", "flag", "ppb"))
    mySheet[, flag := ifelse(grepl("^[A-Za-z]+$", flag), flag, NA)]
    mySheet <- mySheet[!(is.na(flag) & is.na(ppb)), ]
    mySheet[, flag := NULL]
    mySheet[, c("time", "day", "month", "year") := list(hour(date),
                                                        day(date),
                                                        month(date),
                                                        year(date))]
    
    mySheet[, date := as.POSIXct(format(date, format = "%Y-%m-%d"))]
    mySheet[, date := as.POSIXct(substr(date, 1, 10))]
    mySheet[, ppb := as.numeric(ppb)]
  }else{
    mySheet <- read_excel(filePath, skip = 8) %>% data.table()
    if("ppb__1" %in% names(mySheet))mySheet[, ppb__1 := NULL]
    setnames(mySheet, c("date", "time", "ppb", "tempC"))
    mySheet[, c("day", "month", "year") := list(day(date),
                                                month(date),
                                                year(date))]
    mySheet[, ppb := ifelse(ppb < 0, NA, ppb)]
    mySheet[, tempC := ifelse(tempC < -99, NA, tempC)]
    mySheet[, time := as.numeric(
      ifelse(time != 0, substr(time, 1, nchar(time) - 2),
             time))]
  }
}

#Stream Function----------------------------
processNWIS <- function(dailyData){
  setnames(dailyData, c("X_00060_00003", "X_00060_00003_cd", "Date"), c("discharge_cfs", "qualityFlag", "date"))
  dailyData[, agency_cd := NULL]
  dailyData[, discharge_cfs := ifelse(discharge_cfs == -999999, NA, discharge_cfs)]
  dailyData[, year := year(date)]  
}

dailyToAnnual <- function(dailyData, gaugeSites){
  avgAnDis <- dailyData %>% filter(!is.na(discharge_cfs)) %>%group_by(site_no, year) %>% 
    summarise(avgCFS = mean(discharge_cfs)) %>% data.table()
  avgAnDis[, over1 := avgCFS > 1]
  sites <- avgAnDis %>% group_by(site_no) %>% summarise(over1 = all(over1)) %>% filter(over1) %>% pull(site_no)
  
  avgAnDis <- avgAnDis[site_no %in% sites, ][,over1 := NULL]
  avgAnDis <- avgAnDis %>% left_join(gaugeSites) %>% data.table()
  avgAnDis <- avgAnDis[, .(site_no, year, station_nm, avgCFS, HUC8)]
  setnames(avgAnDis, c("station_nm", "avgCFS"), c("name", "value"))
  setkey(avgAnDis, HUC8, site_no)
  return(avgAnDis)
}

peakProcess <- function(peakData, gaugeSites){
  peakData <- peakData[site_no %in% gaugeKey$site_no,]
  peakData <- peakData[, list(site_no, "date" = peak_dt, "discharge_cfs" = peak_va,
                          "year" = year(peak_dt), day = strftime(peak_dt, format = "%j"))]
  peakData <- peakData %>% left_join(gaugeSites) %>% data.table()
  peakData <- peakData[, .(site_no, year, value = day, name = station_nm, discharge_cfs, HUC8)]
  peakData <- peakData[!is.na(value),]
  peakData[, value := as.numeric(value)]
  setkey(peakData, site_no, HUC8)
  return(peakData)
}

dlOzone <- function(dlDate){
  urlBase <- "https://www.colorado.gov/airquality/param_summary.aspx?parametercode=44201&seeddate="
  urlEnd <- "&export=True"
  dateText <- paste0(month(dlDate) %>% twoDigit(), "%2f", day(dlDate) %>% twoDigit(), "%2f", year(dlDate))
  url <- paste0(urlBase, dlDate, urlEnd)
  ozoneTable <- read_html(url) %>% 
    html_nodes(xpath = '//*[(@id = "txbExport")]') %>% html_text() %>% read_lines()
  ozoneTable <- ozoneTable[grepl("\\t", ozoneTable)]
  ozoneTable <- read.table(text = ozoneTable[1:25], sep = "\t", header = T, stringsAsFactors = F) %>% data.table()
  for(j in 1:length(ozoneTable))set(ozoneTable, j = j, value = as.integer(ozoneTable[[j]]))
  ozoneTable[, date := dlDate]
  ozoneTable <- melt(ozoneTable, id.vars = c("hour", "date"), verbose = F)
  return(ozoneTable)
}

plantOzone_3m <- function(plantOzone_M){
  PO_M3 <- lapply(1:nrow(plantOzone_M), FUN = function(x){
    tempTable <- plantOzone_M[x,]
    myMonths <- tempTable$month + 2
    if(myMonths > 12){
      year2 <- tempTable$year + 1
      myMonths2 <- 1:(myMonths %% 12)
      myMonths <- tempTable$month:12
      tempTable <- plantOzone_M[uniqueID == tempTable$uniqueID  & ((month %in% myMonths & year == tempTable$year) | 
                                                                     (month %in% myMonths2 & year == year2))]
    }else{
      tempTable <- plantOzone_M[uniqueID == tempTable$uniqueID &
                                  year == tempTable$year &
                                  month %in% tempTable$month:myMonths]
    }
    setorder(tempTable, year, month)
    myReturn <- data.table(uniqueID = tempTable[1, uniqueID],
                           startM = tempTable[1, month], 
                           endM = (tempTable[1, month] + 2) %% 12,
                           year = tempTable[1, year], 
                           w126 = ifelse(nrow(tempTable) == 3, sum(tempTable$w126M), NA),
                           N100 = ifelse(nrow(tempTable) == 3, sum(tempTable$N100), NA))
    return(myReturn)
  }) %>% rbindlist()
  
  
  
  PO_M3[, endM := ifelse(endM == 0, 12, endM)]
  
  # Find Yearly three month maxes
  PO_final <- PO_M3[, j = list(w126_max = max(w126, na.rm = T),
                               N100_max = max(N100, na.rm = T)), 
                    by = c("uniqueID", "year")]
  PO_final <- PO_final[!is.infinite(w126_max) & !is.na(w126_max) &
                         !is.infinite(N100_max) & !is.na(N100_max),]
  return(PO_final)
}

o3_wrapper <- function(rawHourData){
  cow126 <- rawHourData[, j = w126Calc_M(O3_ppm, date, day, time),
                     by = c("uniqueID", "month", "year")]
  N100 <- rawHourData[, j = list(N100 = sum(O3_ppm >= .1, na.rm = T)),
                   by = c("uniqueID", "month", "year")]
  
  plantOzone_M <- merge(cow126, N100, by = c("uniqueID", "month", "year"), all = T)
  
  plantOzoneFinal <- plantOzone_3m(plantOzone_M)
  return(plantOzoneFinal)
}
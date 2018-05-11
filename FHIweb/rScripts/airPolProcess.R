library(data.table)
library(dplyr)
library(dtplyr)
library(jsonlite)
library(httr)
library(lubridate)
library(readxl)
library(rgdal)
source("FHIweb/rScripts/processFunctions.R")

 # Process EPA Ozone Data---------------------------------

fileDir <- "FHIweb/data/EPA Ozone"
# fileList <- dir(fileDir)
# lapply(fileList, function(x){
#   filePath <- paste0(fileDir, "/", x)
#   unzip(filePath, exdir = "FHIweb/data/EPA Ozone")
#   file.remove(filePath)
# })

# Read in State CSV files
csvList <- dir(fileDir)
csvList <- csvList[grepl(".csv", csvList)]
coOzone <- lapply(csvList, function(x){
  filePath <- paste0(fileDir, "/", x)
  tempCSV <- fread(filePath)
  return(tempCSV[`State Code` == "08",])
})

# Create large list of all years
coOzone <- rbindlist(coOzone)
for(j in c("Longitude", "Latitude")){
  set(coOzone, j = j, value = coOzone[[j]] %>% round(digits = 3))
}
coOzone[, uniqueID := paste0(`County Code`, `Site Num`)]

# Create ozone station list
ozoneStations <- coOzone[, .(`State Code`, `County Code`, `Site Num`, Latitude, 
                             Longitude, Datum, `State Name`, `County Name`, uniqueID)]
setnames(ozoneStations, c("FIPS_s", "FIPS_c", "siteNum", "latitude", "longitude", "datum",
                    "state", "county", "uniqueID"))
ozoneStations <- unique(ozoneStations)
ozoneStations <- split(ozoneStations, by = "uniqueID")

ozoneStations <- ozoneStations %>% lapply(function(x){
  if("WGS84" %in% x$datum){
    x <- x[datum == "WGS84",]
  }
  myReturn <- x[, datum := NULL]
  groupVars <- setdiff(names(myReturn), c("latitude", "longitude"))
  if(nrow(myReturn) != 1){
    myReturn <- myReturn[, j = list(latitude = mean(latitude), longitude = mean(longitude)),
                     by = c(groupVars)]
  }
  return(myReturn)
}) %>% rbindlist(use.names = T)

# Rename State Ozone list and clean up columns
coOzone <- coOzone[, .(uniqueID, `Date Local`, `Time Local`, `Sample Measurement`, Qualifier)]
setnames(coOzone, c("uniqueID", "date", "time", "O3_ppm", "flag"))

coOzone <- coOzone[, j = list(O3_ppm = mean(O3_ppm), flag = paste(flag, collapse = ",")), 
            by = list(uniqueID, date, time)]

coOzone[ , date := as.Date(date)]
coOzone[, time := substr(time, 1, 2) %>% as.numeric()]
coOzone[, c("day", "month", "year") := list(day(date), month(date), year(date))]


# Process City of Aspen Ozone Data-------------------------
ozoneDir <- "FHIweb/data/CoA_Ozone/"
ozoneFolders <- paste0(ozoneDir, dir(ozoneDir))

ozoneData <- lapply(ozoneFolders, FUN = function(x){
  ozoneFiles <- paste0(x, "/", dir(x))
  myReturn <- lapply(ozoneFiles, FUN = function(y){
    readCoAO3(y)
  })
  return(rbindlist(myReturn))
})

ozoneData <- lapply(ozoneData, FUN = function(x){
  x[, date := as.POSIXct(substr(date, 1, 10))]
})
ozoneData <- rbindlist(ozoneData, use.names = T, fill = T)
write.csv(ozoneData, "aspenOzone.csv", row.names = F)

# Create a unique id to merge with AQS data
ozoneData[, c("uniqueID", "flag") := list("0978161", "")]
ozoneData[, O3_ppm := ppb / 1000]

# Removing temp for now...we might want to use it later but for now it makes merging hard
ozoneData[, c("tempC", "ppb") := NULL]
setcolorder(ozoneData, names(coOzone))
ozoneData[, date := as.Date(date)]

 # Combine City and EPA Ozone---------
coOzone <- rbindlist(list(coOzone, ozoneData), use.names = T)

# Rename ozoneStation for later use with point extractor
setnames(ozoneStations, c("latitude", "longitude"), c("lat", "long"))

# Add aspen station to rest of state
ozoneStations <- rbind(ozoneStations, data.table("FIPS_s" = "08", "FIPS_c" = "097", "siteNum" = "8161",
                                                 "lat" = 39.196248, "long" = -106.836380, "state" = "Colorado",
                                                 "county" = "Pitkin", "uniqueID" = "0978161"))

# Calculate monthly w126 and n100
cow126 <- coOzone[, j = w126Calc_M(O3_ppm, date, day, time),
                  by = c("uniqueID", "month", "year")]
N100 <- coOzone[, j = list(N100 = sum(O3_ppm >= .1, na.rm = T)),
                by = c("uniqueID", "month", "year")]

plantOzone_M <- merge(cow126, N100, by = c("uniqueID", "month", "year"), all = T)

# Calculate 3-month N100 and w126
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


# Assign Stations to Watersheds

ozoneKey <- pointExtracter(ozoneStations, watershedMapB)

# Save ozone values key file
ozoneFull <- merge(PO_final, ozoneKey, by = "uniqueID", allow.cartesian = T)
saveRDS(ozoneKey, "FHIweb/data/webOzone/ozoneKey.RDS")
saveRDS(PO_final, "FHIweb/data/webOzone/ozoneMetrics.RDS")


#Creates 3 year w126 statistic NOT RUN
PO_final <- split(PO_final, by = "uniqueID")


PO_final <- PO_final %>% lapply(FUN = function(x){
  if(nrow(x) < 3){
    returnList <- NULL
  }else{
    returnList <- vector("list", nrow(x) - 2)
    setorder(x, year)
    for(i in 3:nrow(x)){
      returnTable <- x[year %in% x[i, year]:x[i, year - 2],]
      returnList[[i-2]] <- data.table(w126 = mean(returnTable$w126_max), year = x[i, year])
    }
  }
  return(rbindlist(returnList))
})

lapply(names(PO_final), function(x){
  if(nrow(PO_final[[x]]) != 0){
    PO_final[[x]][, uniqueID := x]  
  }
})
 
PO_final <- rbindlist(PO_final)


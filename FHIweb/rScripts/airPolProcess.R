library(data.table)
library(dplyr)
library(dtplyr)
library(jsonlite)
library(httr)
library(lubridate)
library(readxl)
library(rgdal)
library(snow)
library(doSNOW)
source("FHIweb/rScripts/processFunctions.R")
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# sfInit(parallel = T, 8)
# sfLibrary(data.table)
 # Process EPA Ozone Data---------------------------------

fileDir <- "FHIweb/data/EPA Ozone"
fileList <- dir(fileDir)
# lapply(fileList, function(x){
#   filePath <- paste0(fileDir, "/", x)
#   unzip(filePath, exdir = "FHIweb/data/EPA Ozone")
#   file.remove(filePath)
# })

# Read in State CSV files
csvList <- dir(fileDir)
csvList <- csvList[grepl(".csv", csvList)]
# sfExport("csvList", "fileDir")
coOzone <- lapply(csvList, function(x){
  filePath <- paste0(fileDir, "/", x)
  tempCSV <- fread(filePath)
  return(tempCSV[`State Code` == "08" | `State Code` == 8,])
})

# Create large list of all years
coOzone <- rbindlist(coOzone)
for(j in c("Longitude", "Latitude")){
  set(coOzone, j = j, value = coOzone[[j]] %>% round(digits = 3))
}

set(coOzone, j = "County Code", value = coOzone[['County Code']] %>% formatC(width = 3, format = "d", flag = "0"))
set(coOzone, j = "Site Num", value = coOzone[['Site Num']] %>% formatC(width = 4, format = "d", flag = "0"))

coOzone[, uniqueID := paste0(`County Code`, `Site Num`)]

# Create ozone station list
ozoneStations <- coOzone[, .(`State Code`, `County Code`, `Site Num`, Latitude, 
                             Longitude, Datum, `State Name`, `County Name`, uniqueID)]
setnames(ozoneStations, c("FIPS_s", "FIPS_c", "siteNum", "latitude", "longitude", "datum",
                    "state", "county", "uniqueID"))
ozoneStations[, FIPS_s := as.numeric(FIPS_s)]
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

coOzone[ , date := as_date(date)]
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
ozoneData[, c("uniqueID", "flag") := list("0970007", "")]
ozoneData[, O3_ppm := ppb / 1000]

# Removing temp for now...we might want to use it later but for now it makes merging hard
ozoneData[, c("tempC", "ppb") := NULL]
setcolorder(ozoneData, names(coOzone))
ozoneData[, date := as_date(date)]

 # Combine City and EPA Ozone---------
coOzone <- rbindlist(list(coOzone, ozoneData), use.names = T)

# Rename ozoneStation for later use with point extractor
setnames(ozoneStations, c("latitude", "longitude"), c("lat", "long"))

# Add aspen station to rest of state
ozoneStations <- rbind(ozoneStations, data.table("FIPS_s" = "08", "FIPS_c" = "097", "siteNum" = "8161",
                                                 "lat" = 39.196248, "long" = -106.836380, "state" = "Colorado",
                                                 "county" = "Pitkin", "uniqueID" = "090007"))
saveRDS(coOzone, "FHIweb/data/webOzone/rawOzone.RDS")
saveRDS(ozoneStations, "FHIweb/data/webOzone/ozoneStations.RDS")



ozoneKey <- pointExtracter(ozoneStations, watershedMapB)

# Save ozone values key file
ozoneFull <- merge(PO_final, ozoneKey, by = "uniqueID", allow.cartesian = T)
saveRDS(ozoneKey, "FHIweb/data/webOzone/ozoneKey.RDS")
saveRDS(PO_final, "FHIweb/data/webOzone/ozoneMetrics.RDS")

temp <- 


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


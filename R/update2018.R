library(dataRetrieval)

snowTelData <- snowTelData %>% left_join(snoTelKey)
snowTelData <- data.table(snowTelData)

snowTelIDs <- c("USS0007K09S", "USS0006K04S", "USS0006K30S")

snoTelInt <- snowTelData[id %in% snowTelIDs,]
snoTelInt <- snoTelInt[waterYear %in% c(2015, 2016)]

snoTelInt[, date := as.Date(paste0(year, "/", month, "/", day), format = "%Y/%m/%d")]
snoTelInt[, valueIN := round(value/10 * .0393701, 1)]


# Calculate max
snoTelInt %>% group_by(name, waterYear) %>% summarise(max(valueIN, na.rm = T))


# Calculate start of snowmelt
for(i in unique(snoTelInt$name)){
  for(j in unique(snoTelInt$waterYear)){
    data <- snoTelInt[name == i & waterYear == j,]
    data <- data[order(year, month, day),]
    data <- data[!is.na(valueIN),]
    data[, melt := 0]
    for(k in 1:nrow(data)){
      data[k, melt := data[k, valueIN] - data[k + 4, valueIN]]
      
    }
    print(data[melt > 1,][1,])
    print(as.Date(paste0(j, "/10/1"), format = "%Y/%m/%d"))
    print(as.Date(paste0(j-1, "/10/1"), format = "%Y/%m/%d") - data[melt > 1,][1, date])
  }
}

# Calculate snow 50
for(i in unique(snoTelInt$name)){
  for(j in unique(snoTelInt$waterYear)){
    data <- snoTelInt[name == i & waterYear == j & month < 6 & month > 2,]
    max50 <- max(data$valueIN, na.rm = T) / 2
    data[, snow50 := valueIN - max50]
    data <- data[order(year, month, day),]
    print(data[snow50 <= 0,][1,])
    print(as.Date(paste0(j-1, "/10/1"), format = "%Y/%m/%d") - data[snow50 <= 0,][1, date])
  }
}

sites <- c("09073300", "09085000", "09081600", "09080400")
names <- c("Difficult", "GWS", "Crystal", "Fryingpan")
siteDF <- cbind.data.frame(sites, names) %>% data.table()
setnames(siteDF, "sites", "site_no")

parameter <- c("00060")
daily <- readNWISdv(sites, parameter, startDate = "2016-01-01", endDate = "2016-12-31") %>% data.table()
peak <- readNWISpeak(sites, startDate = "2016-01-01", endDate = "2016-12-31") %>% 
  data.table()
peak <- merge(peak, siteDF, by = "site_no", all.x = T)
peak[, days := strftime(peak_dt, format = "%j")]
setnames(daily, "X_00060_00003", "discharge")
daily <- merge(daily, siteDF, by = "site_no", all.x = T)

daily[, dailyAcFt := discharge*60*60*24/43560/1000]
daily[, Date := as.Date(Date, format = "%Y-%m-%d")]
daily <- split(daily, by = "site_no")
daily <- lapply(daily, FUN = function(x)x[, cuml := cumsum(x$dailyAcFt)])

Qs <- lapply(daily, FUN = function(x){
  annual <- sum(x$dailyAcFt, na.rm = T)
  q50 <- annual * .50
  q80 <- annual * .8
  q20 <- annual * .2
  myReturn <- rbind(x[cuml > q20,][1,], x[cuml > q50,][1,], x[cuml > q80,][1,])
  myReturn[, days:= strftime(Date, format = "%j")]
  myReturn[, metric := c("q20", "q50", "q80")]
})

maxQ <- lapply(daily, FUN = function(x){
  myReturn <- x[discharge == max(discharge),][1,]
  myReturn[, days := strftime(Date, format = "%j")]
})

# Precip/Temp

rfvWeather <- loadDLY(c("USC00053359", "USC00050372"))
rfvWeather <- lapply(rfvWeather, returnMeltedValues)
names(rfvWeather) <- c("Glenwood", "Aspen")
lapply(rfvWeather, FUN = function(x) sum(x[year == 2016 & element == "PRCP", value], na.rm = T))
calcFF(rfvWeather$Aspen)
calcTempThresh(rfvWeather$Aspen, 85)
calcTempThresh(rfvWeather$Glenwood, 95)
calcTempThresh(rfvWeather$Aspen, 0, T)
calcTempThresh(rfvWeather$Glenwood, 10, T)

# Drought Stress
dstress <- fread("fhiweb/data/droughtStress.csv")
myCols <-  c("TMEAN", "DEWP", "WETB")
for(j in myCols){
  set(dstress, i = NULL, j = j, value = (dstress[[j]] - 32) * (5 / 9))
}
dstress[, date := as.Date(strptime(paste("2016", day_of_year), format="%Y %j"))]
dstress[, actual_vpd := 6.11 * 10^
          ((7.5 * DEWP)/(237.7 + DEWP))]
dstress[,saturated := 6.11 * 10^
          ((7.5 * TMEAN)/(237.7 + TMEAN))]
dstress[, vpd := (saturated - actual_vpd) / 10]

library(snht)
joinedStations <- lapply(unique(stationList$simpleName), function(x){
  z <- retriveCOOPdata(x, stationList, stationTable)
  z[, simpleName := x]
  return(z)
})
joinedStations <- rbindlist(joinedStations)
stationsHCN <- stationList[wmo_id == "HCN",]
dataHCN <- joinedStations[id %in% stationsHCN$id,]

stationList2 <- ghcnd_stations()
stationList2 <- data.table(stationList2)
stationList2[, recordLen := last_year - first_year]
stationList2 <- stationList2[substr(id, 1, 2) == "US",]
stationList2 <- stationList2[substr(id, 3, 3) != "1",]
stationList2 <- stationList2[state == "CO",]

stationList2 <- stationList2[element == "PRCP",]


testData <- rbind(joinedStations[simpleName == "GUFFEY",], dataHCN)
testData[simpleName == "GUFFEY", id := "USC00053656"]
testHCN <- rbind(stationsHCN, stationList2[226,], fill = T)
testHCN[18, 13] <- "GUFFEY"
distanceM <- dist(testHCN[, .(latitude, longitude)])
distanceM <- as.matrix(distanceM)
testData <- returnMeltedValues(testData, "TMAX")


### Code to reduce to close stations
keeperStations <- c("USC00051294", "USC00051528", "USC00050848", "USC00053656")
testData <- testData[id %in% keeperStations]
testHCN <- testHCN[id %in% keeperStations]
distanceM <- dist(testHCN[, .(latitude, longitude)])
distanceM <- as.matrix(distanceM)
testData <- returnMeltedValues(testData, "PRCP")



testData[, time := paste0(year, ifelse(nchar(month) == 2, month, paste0("0", month)), 
                    ifelse(nchar(day) == 2, day, paste0("0", day)))]
testData[, c("day", "month", "year", "element") := NULL]
setnames(testData, c("id", "value"), c("location", "data"))
testData <- data.frame(testData)
distanceM <- data.frame(distanceM)

distanceM <- as.matrix(distanceM)
colnames(distanceM) <- testHCN$id
rownames(distanceM) <- testHCN$id
testData$time <- as.numeric(testData$time)

out2 <- pairwiseSNHT(testData, distanceM, k=2, period=200,
                     crit=100, returnStat=F)

temp <- data.table(out2$data)

robustSNHTunequal()
robustSNHT()

distance <- dist()
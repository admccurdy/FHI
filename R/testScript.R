aspenStation <- read.csv("data/aspenTemps.csv")
aspenStation <- data.table(aspenStation)
aspenStation <- aspenStation[, c("ELEVATION", "LATITUDE", "LONGITUDE", "Decade") := NULL]
aspenStation <- melt(aspenStation, id.vars = c("STATION_NAME", "DATE", "Year"))
aspenStation[, c("month", "day") := list(substr(DATE, 5,6), substr(DATE, 7,8))]
aspenStation[, DATE := NULL]
aspenStation[, month := as.numeric(month)]
setnames(aspenStation, "variable", "element")
setnames(aspenStation, "STATION_NAME", "id")

aspenFF <- calcFF(aspenStation)
aspenFF <- fread("data/aspenFF.csv")
aspenFF2 <- calcFF(aspenStation)


aspenFFBase <- calcBase(aspenFF, baseStart = 1940, baseEnd = 1969)
aspenFFScore <- calcScoreTable(aspenFFBase)
print(calcScore(2010, 2015, aspenFFScore, aspenFF))
ffScore <- calcBase(aspenFF, baseStart = 1940, baseEnd = 1969) %>%
  calcScoreTable() %>% calcScore(startYear = 2010, endYear = 2015, scoreData = aspenFF)


aspenHiT <- calcTempThresh(aspenStation, 85, F)
aspenHiT[, station := "one"]
aspenHiT[year %in% 1980:2016, station := "two"]
ggplot(aspenHiT, aes(x = year, y = High_T_Days, color = station)) + geom_point() + stat_smooth(method = "lm")

aspenHiTBase <- calcBase(aspenHiT, baseStart = 1940, baseEnd = 1969)
aspenHiTScore <- calcScoreTable(aspenHiTBase)
print(calcScore(2006, 2015, aspenHiTScore, aspenHiT))

aspenLowT <- calcTempThresh(aspenStation, 0, T)

## Retrive Station
testStation <- retriveCOOPdata("BOULDER", stationList, stationTable)
meltedStation <- returnMeltedValues(testStation)


## calc FF Days
ffDays <- calcFF(meltedStation)

## calc Baseline Mean + SD
ffBase <- calcBase(ffDays, 40)

## calc score table
ffScore <- calcScoreTable(ffBase)

## Current FF Score
print(calcScore(2016, ffScore, ffDays))

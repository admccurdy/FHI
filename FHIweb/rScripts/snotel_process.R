
# Load list of GHCND Station
load("c:/Users/admcc/Documents/ACES/Data/ghcndStations.RData")
watershedMap <- st_read("c:/Users/admcc/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp")

# Subset into for snowtel and turn into simple feature
snowTelList <- stationList[state == "CO",]
snowTelList <- snowTelList[substr(id, 3, 3) == "S",]
snowTelList <- snowTelList[element == "WESD",]
snowTelList <- st_as_sf(x = snowTelList, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84")
snowTelList <- snowTelList %>% st_transform(crs = st_crs(watershedMap))

# download .dly files from the internets this will fetch the most update .dly records
snowTelData <- loadDLY(snowTelList$id)
snowTelData <- lapply(snowTelData, returnMeltedValues)
snowTelData <- snowTelData %>% rbindlist()
snowTelData <- snowTelData[element == "WESD",]
snowTelData[, waterYear := ifelse(month > 9, year + 1, year)]

# Create variabe to identify complete years and merge with snowtel data
snotelComplete <- snowTelData %>% filter(day == 1) %>% group_by(id, waterYear) %>%
  summarise(complete = sum(month))
snowTelData <- merge(snowTelData, snotelComplete, by = c("id", "waterYear"))
snowTelData <- snowTelData[, complete := ifelse(complete == 78, T, F)]

#Ideintify which waterhseds snotel stations are in
snowWS <- st_intersects(watershedMap, snowTelList)
snowTelList <- as.data.table(snowTelList)
watershedMap <- as.data.table(watershedMap)

snowWS <- lapply(1:length(snowWS), FUN = function(x){
  print(x)
  if(length(unlist(snowWS[x])) != 0 ){
    data.table("name" = watershedMap[x, "NAME"], "HUC8" = watershedMap[x, "HUC8"],
               "station" = snowTelList[unlist(snowWS[x]), "id"])
  }else{
    NULL
  }
}) %>% rbindlist()
setnames(snowWS, c("wsName", "HUC8", "id"))

# Create Summary metrics


snowMax <- snowTelData[, complete := NULL] %>% group_by(waterYear, id) %>% slice(which.max(value)) %>% data.table()
snowMax <- snowMax[value != 0 & !is.na(value),]
snowApril <- snowTelData[day == 1 & month == 4,]


snoMetrics <- list("april" = snowApril, "max" = snowMax)

snowWS <- merge(snowWS, snowTelList[, c("id", "elevation", "name", "geometry")], by = "id", all.x = T)
snowTelKey <- snowWS

# save files
save(snowTelKey, file = "fhiweb/data/snotel/snoTelKey.RData")
save(snoMetrics, file = "fhiweb/data/snotel/snoMetrics.RData")


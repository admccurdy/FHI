
# Load list of GHCND Station
watershedMap <- st_read("F:/Documents/ACES/Maps/hydrologic_units/wbdhu8_a_co.shp")
stationList <- readRDS("FHIweb/data/baseData/stationList.RDS")

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
snoMetrics <- snoProcess(snowTelData)

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
snowWS <- merge(snowWS, snowTelList[, c("id", "elevation", "name", "geometry")], by = "id", all.x = T)
snoTelKey <- snowWS

# save files
saveRDS(snoTelKey, file = "fhiweb/data/snotel/snoTelKey.RDS")
saveRDS(snoMetrics, file = "fhiweb/data/snotel/snoMetrics.RDS")


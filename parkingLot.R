####Code to dl daymet data doesn't work####
years <- 1980:2016
plbaseURL <- "https://daac.ornl.gov/orders/5264873aea4679f0f9883aaab6634b38/Daymet_V3_Annual_Climatology/data/daymet_v3_"
timeFrame <- "_ann"
measure <- "prcp"
summaryFun <- "ttl_"
summaryFun <- c("ttl_", "avg_") #ttl for total and avg for average"
suffix <- "_na.nc4"
path <- paste0(baseURL, y, timeFrame, z, x, suffix)
y <- "prcp"
x <- "1980"
z <- "ttl_"

fileBase <- "FHIweb/data/dayMetAnn/"

for(x in years){
  for(y in measure){
    for(z in summaryFun){
      path <- paste0(baseURL, y, timeFrame, z, x, suffix)
      fileName <- paste0("FHIweb/data/dayMetAnn/", x, y, ".nc4")
      print(path)
      print(fileName)
      download.file(path, "fileName.nc4")
    }
  }
}

temp <- insectSF2[NAME == "Roaring Fork" & year %in% 2016:2016]
temp[, DCA1 := as.factor(as.character(DCA1))]
st_geometry(temp) <- temp$geometry
insectColor = colorFactor("Set1", temp$DCA1)
temp <- st_transform(temp, "+proj=longlat +datum=WGS84")
leaflet() %>%
  addProviderTiles("Esri.NatGeoWorldMap",
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addPolygons(data = temp, stroke = F,
              fillColor = ~insectColor(DCA1),
              fillOpacity = .5) %>%
    addLegend(pal = insectColor, values = temp$DCA1 )

temp <- read.csv("F:/Documents/ACES/Maps/watershed/Export_Output.csv")
temp2 <- read.csv("F:/Documents/ACES/Maps/watershed/Export_Output2.csv")

temp2 <- temp2 %>% group_by(DCA1) %>% summarise(area = sum(F_AREA))
temp2 <- temp2 %>% data.table()
temp2[, acres := area * 0.000247105]

temp <- temp %>% group_by(IDS_attrib_AGNT_NM) %>% summarise(area = sum(F_AREA))
temp <- temp %>% data.table()
temp[, acres := area * 0.000247105]



tempClip <- spTransform(watershedMap, globalCRS)
tempMap <- crop(insect2015, tempClip[tempClip@data$NAME == "Roaring Fork",])
writeOGR(obj = tempMap, dsn = "R", layer = "tempMap", driver="ESRI Shapefile")                

tempMap$acres <- raster::area(tempMap) / 1000000 * 247.105
temp3 <- tempMap@data %>% group_by(DCA1) %>% summarise(area = sum(acres))fhii


watershedUtils <- extract(coloradoUtils, spTransform(watershedMap, crs(coloradoUtils)))
names(watershedUtils) <- watershedMap$NAME


utilityProps <- lapply(1:length(watershedUtils), FUN = function(x){
  returnTable <- watershedUtils[[x]] %>% table() %>% data.table()
  returnTable[, name := names(watershedUtils)[x]]
  returnTable[, proportion := N / sum(N)]
})
utilityProps <- rbindlist(utilityProps)
setnames(utilityProps, ".", "utility")
utilityProps[, utility := as.integer(utility)]
utilityProps <- merge(utilityProps, coloradoUtilKey, by.x = "utility",
                      by.y = "utilityNum", all.x = T)
writeRaster(coloradoUtils, "coloradoUtils", format = "GTiff")


base <- "F:/Documents/ArcGIS/temp/ZonalSt_"
arcTables <- vector("list", 16)
for(i in 1:16){
  arcTables[[i]] <- read.dbf(paste0(base, i-1, ".dbf"), as.is = T) %>% data.table()
}
arcTables <- arcTables %>% rbindlist()
arcTables[, year := 2000:2015]
arcTables[, SUM := SUM / 100]

arcTables[, anomolyArc := SUM - mean(SUM[1:14])]
arcTables[, anomolyArcPer := anomolyArc / SUM]

microbenchmark::microbenchmark(
temp <- merge(snowApril, snowWS[, c("id", "wsName", "HUC8", "name")], by = "id"),
temp <- snowWS %>% dplyr::select(id, wsName, HUC8, name) %>% right_join(snowApril),
temp2 <- snowWS[snowApril, .(id, wsName, HUC8, name, waterYear, year, month, day, value)])

temp <- snowMax %>% left_join(snoTelKey) %>% data.table()
temp <- temp[name %in% c("Independence Pass",  "Mc Clure Pass", "Kiln"),]
snowMatrix <- sapply(unique(temp$name), FUN = function(x){as.integer(temp[name == x, value])})

names(snowMatrix) <- c("ind", "kiln", "mcp")
snowMatrix <- snowMatrix %>% as.data.frame()
rfvModel <- lm(data = snowMatrix[10:37,], formula = ind~., family = "Gamma")
predict.lm(rfvModel, snowMatrix[1:9,])

temp <- data.table("a" = sample(c(1,2), 100, T), b = runif(100, 1, 100), c = sample(c(3,4), 100, T))
groupCol <- c("a", "c")

temp %>% group_by_at(groupCol) %>% summarise(value = mean(b))

library(snowfall)
library(rmapshaper)
library(microbenchmark)
load("FHIweb/data/CRS.RData")

insectFolder <- "f:/Documents/ACES/Maps/Insect/"
insectKey <- fread("f:/Documents/ACES/FHI/FHIweb/data/inscetKey_2.csv")
insectKey <- insectKey[!is.na(as.numeric(Code)) & Code != "",]
insectKey[, Category := ifelse(Category == "", V5, Category)]
insectKey[, c("V5", "V6") := NULL]
setnames(insectKey, "Code", "agent")
insectKey[, agent := as.numeric(agent)]

clipShape <- spTransform(watershedMap, globalCRS)
clipShapeSF <- st_as_sf(clipShape)
clipShapeSF <- st_transform(clipShapeSF, st_crs(returnMap))





temp$DCA1 <- as.factor(as.character(temp$DCA1))
temp <-st_intersection(returnMap, clipShapeSF)
temp <- st_simplify(insectSF[[23]], dTolerance = 1)
temp <- st_join(insectSF[[21]], clipShapeSF)
clipShapeSF <- st_transform(clipShapeSF, st_crs(insectSF[[1]]))
clipShapeSimple <- clipShapeSF[, c("NAME", "HUC8")]

microbenchmark(ggplot(temp[temp$NAME == "Roaring Fork",]) + 
                 geom_sf(),
ggplot(st_intersection(insectSF[[23]], clipShapeSF[clipShapeSF$NAME == "Roaring Fork",])) + 
  geom_sf(), times = 1)
temp2 <- st_simplify(temp2)

insectYears <- dir(insectFolder)

insectMaps <- lapply(insectYears, loadInsectMap, insectFolder = insectFolder,
                     clipShape = clipShape)
names(insectMaps) <- insectYears
insectSF <- lapply(insectMaps, st_as_sf)
insectSF <- lapply(1:length(insectSF), FUN = function(x){
  myReturn <- data.table(st_join(insectSF[[x]], clipShapeSimple))
  myReturn[, year := insectYears[x]]
})
insectSF2 <- rbindlist(insectSF, fill = T)
saveRDS(insectSF2, "FHIweb/data/insects/insectData.RDS")


insectFrames <- lapply(1:2, FUN = function(x){
  insectAreas(insectMaps[[x]], insectYears[x], clipShape)
})

loadInsectMap <- function(year, insectFolder, clipShape = NA){
  fileName <- paste0("r2", substr(year, 3,4), "_dmg")
  filePath <- paste0(insectFolder, year, "/")
  # returnMap <- st_read(paste0(filePath, fileName, ".shp"))
  returnMap <- readOGR(paste0(filePath, fileName, ".shp"),
                     layer = fileName)
  if(proj4string(returnMap) != globalProj4Str){
    returnMap <- spTransform(returnMap, globalCRS)
  }
  if(!is.na(clipShape)){
    if(length(clipShape) > 1){
      clipShape$dissolve <- 1
      clipShape <- gUnaryUnion(clipShape, id = clipShape@data$dissolve)
      clipShape <- ms_simplify(clipShape)
    }
    returnMap <- crop(returnMap, clipShape)
  }
  return(returnMap)
}

insectAreas <- function(insectMap, year, sortMap = NA){
  if(is.na(sortMap)){
    returnFrame <- summarizeInsectMap(insectMap)
  }else{
    returnFrame <- lapply(1:length(sortMap), FUN = function(x){
      print(x)
      currentFrame <- sortMap[x, ] %>% crop(x = insectMap) 
      if(!is.null(currentFrame)){  
        currentFrame <- summarizeInsectMap(currentFrame)
        currentFrame[, c("HUC8", "name") := list(as.character(sortMap@data[x, "HUC8"]),
                                               as.character(sortMap@data[x, "NAME"]))]
      }
      if(!is.null(currentFrame)){
        currentFrame[, "year" := year ]  
      }
      return(currentFrame)
    }) %>% rbindlist()
  }
  returnFrame[, agent := as.numeric(agent)]
  returnFrame <- merge(x = returnFrame, y = insectKey, by = "agent", all.x = T)
  return(returnFrame)
}

summarizeInsectMap <- function(insectMap){
  insectMap$areaKM2 <- raster::area(insectMap) / 1000000
  insectMap$areaAC <- insectMap$areaKM2 * 247.105
  insectDF <- insectMap@data %>% group_by(DCA1) %>% 
    summarise(areaKM = sum(areaKM2), areaAC = sum(areaAC)) %>% 
    data.table()
  setnames(insectDF, c("agent", "areaKM2", "areaAC"))
  insectDF[, percent := areaKM2/sum(insectDF$areaKM2)]
}

parallelAreas <- function(a){
  insectAreas(insectMaps[a], insectYears[a], clipShape)
}

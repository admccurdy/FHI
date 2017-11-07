# Load data-----------------------------------------------
# Make sure helper functions below are loaded
coloradoUtilKey <- fread("FHIweb/data/Colo_Utils/Power Utility Key.csv")
eiaKey <- fread("FHIweb/data/Colo_Utils/EI_Util.csv")
eiaKey <- merge(eiaKey, coloradoUtilKey, by.x = "Colo_Name", by.y = "utilityName", all = T)
coloradoUtils <- raster("FHIweb/data/Colo_Utils/coloradoutilities.tif")
coloradoPops <- readOGR("c:/Users/admcc/Documents/ACES/Maps/colo_Pop/tabblock2010_08_pophu.shp",
                        layer = "tabblock2010_08_pophu") %>% st_as_sf()

# Simplify colorado pops-------------------------------------------
# This step takes a long time
coloradoPops <- ms_simplify(coloradoPops)
coloradoPops <- st_transform(coloradoPops, globalProj4Str)

# Clean up utility map---------------------------------------------
# significant work was done in arcmap after this to produce the final product
coloradoUtils[1:64,][coloradoUtils[1:64,] == 30] <- NA
coloradoUtils[,1:100][coloradoUtils[,1:100] == 30] <- NA
coloradoUtils[2000:nrow(coloradoUtils),][coloradoUtils[2000:nrow(coloradoUtils),] == 30] <- NA
coloradoUtils[,2543:ncol(coloradoUtils)][coloradoUtils[,2543:ncol(coloradoUtils)] == 30] <- NA
coloradoUtilsMat <- as.matrix(coloradoUtils)

for(i in 2:(ncol(coloradoUtilsMat)-1)){
  print(i)
  for(j in 2:(nrow(coloradoUtilsMat) - 1)){
    if(!is.na(coloradoUtilsMat[j,i])){
      if(!is.na(coloradoUtilsMat[j, i + 1]) & !is.na(coloradoUtilsMat[j, i -1])){
        if(coloradoUtilsMat[j,i] != coloradoUtilsMat[j, i + 1] &
           coloradoUtilsMat[j,i] != coloradoUtilsMat[j, i - 1]){
              coloradoUtilsMat[j,i] <- coloradoUtilsMat[j, i + sample(c(-1,1), 1)]

        }
      }
      if(!is.na(coloradoUtilsMat[j + 1, i]) & !is.na(coloradoUtilsMat[j - 1, i])){
        if(coloradoUtilsMat[j,i] != coloradoUtilsMat[j + 1, i] &
           coloradoUtilsMat[j,i] != coloradoUtilsMat[j - 1, i]){
              coloradoUtilsMat[j,i] <- coloradoUtilsMat[j + sample(c(-1,1), 1) , i]
        }
      }
    }
  }
}

utilsNA <- which(is.na(coloradoUtilsMat), arr.ind = T)
utilsNumber <- which(!is.na(coloradoUtilsMat), arr.ind = T)
for(i in 1:nrow(utilsNA)){
  print(i)
  myNeighbors <- try(matrixNeighbors(coloradoUtilsMat, c(utilsNA[i,1],utilsNA[i,2]), 8), T)
  if(sum(is.na(myNeighbors)) <= 2 & class(myNeighbors) != "try-error"){
    coloradoUtilsMat[utilsNA[i, 1], utilsNA[i, 2]] <- myMode(myNeighbors)
  }
}

for(i in 1:nrow(utilsNumber)){
  myNeighbors <- try(matrixNeighbors(coloradoUtilsMat, c(utilsNumber[i,1],utilsNumber[i,2]), 8), T)
  if(sum(myNeighbors == coloradoUtilsMat[utilsNumber[i, 1], utilsNumber[i, 2]], na.rm = T) <= 3){
    coloradoUtilsMat[utilsNumber[i, 1], utilsNumber[i, 2]] <- as.numeric(myMode(myNeighbors))
  }
}

coloradoUtilsMat[,2864:ncol(coloradoUtilsMat)][coloradoUtilsMat[,2864:ncol(coloradoUtilsMat)] == 25] <- NA
coloradoUtilsMat[,2864:ncol(coloradoUtilsMat)][coloradoUtilsMat[,2864:ncol(coloradoUtilsMat)] == 28] <- NA


# Load Util Mat in from Arc----------------------------------------------------
coloGridPops <- st_read(dsn = "FHIweb/data/Colo_Utils/watershedUtilPop.shp")
coloGridPops <- data.table(coloGridPops)
setnames(coloGridPops, "SUM_new_po", "pop")
waterShedPops <- coloGridPops %>% group_by(HUC8) %>% summarise(watershedPop = sum(pop))
utilityPops <- coloGridPops %>% group_by(gridcode) %>% summarise(utilityPop = sum(pop))
coloGridPops <- merge(coloGridPops, utilityPops, by = "gridcode", all.x = T)
coloGridPops <- merge(coloGridPops, waterShedPops, by = "HUC8", all.x = T)


# Extract EIA years------------------------------------------------------------
eiaDirectory <- "FHIweb/data/Colo_Utils/EIA_Data/"
eiaFolders <- dir(eiaDirectory)
for(i in eiaFolders){
  year <- substr(i, 5, 8)
  dir.create(paste0(eiaDirectory, year))
  unzip(zipfile = paste0(eiaDirectory, i), exdir = paste0(eiaDirectory, year))
  file.remove(paste0(eiaDirectory, i))
}
tableName <- "table6"

eiaData <- lapply(2004:2015, FUN = function(x){
  ext <- ifelse(x >= 2015, ".xlsx", ".xls")
  eiaSheet <- openEnergy(paste0(eiaDirectory, x, "/", tableName, ext ), x) %>% data.table()
  eiaSheet <- processEIA(eiaSheet, eiaKey)
  eiaSheet <- eia_by_shed(eiaSheet, ws_grid_pop, x)
})

eiaData <- eiaData %>% rbindlist(use.names = T)

# Helper Functions---------------------------------------------
matrixNeighbors <- function(mat, coords, neighbors = 4){
  row <- coords[1]
  col <- coords[2]
  return <- NULL
  for(i in c(-1:1)){
    for(j in c(-1:1)){
      return <- c(return, mat[row + i, col + j])
    }
  }
  if(neighbors == 8){
    return <- return[- 5]
  }else{
    return <- return[seq(2, 8, by = 2)]
  }
  return(return)
}
myMode <- function(x) {
  ux <- unique(x)
  ux <- ux[which.max(tabulate(match(x, ux)))]
  ifelse(ux == "NA", NA, ux)
}
processEIA <- function(eiaYear, utilKey){
  eiaYear <- eiaYear[state == "CO"]
  eiaYear[, c("state", "ownership", "price") := NULL]
  cols <- c("customers", "sales_mwth", "revenue")
  for(i in c("customers", "sales_mwth", "revenue")){ 
   set(eiaYear, j = i, value = as.numeric(eiaYear[[i]]))
  }
  # eiaYear[, .SD := lapply(.SD, function(x)
  #   {as.numeric(gsub(",", "", x))}), .SDcols = c("customers", "sales_mwt", "revenue")]
  eiaYear[, revenue := revenue * 1000]
  eiaYear[, perCapitaUse := sales_mwth / customers]
  eiaYear <- merge(eiaYear, utilKey, by = "utility", all.x = T)
}

eia_by_shed <- function(eiaYear, ws_grid_pop, Year){
  ws_grid_pop <- merge(eiaYear, ws_grid_pop, by = "gridcode", all = F)
  returnFrame <- lapply(unique(ws_grid_pop$HUC8), FUN = function(x){
    z <- ws_grid_pop[HUC8 == x,]
    myReturn <- data.table("elecUse" = sum((z$pop / z$watershedPop) * z$perCapitaUse),
                           "ws_name" = z[1, NAME],
                           "ws_Pop" = z[1, watershedPop],
                           "HUC8" = z[1, HUC8])
  }) %>% rbindlist()
  returnFrame[, year := Year]
  return(returnFrame)
}

openEnergy <- function(path, year){
  eiaSheet <- read_excel(path, skip = 2) %>% data.table()
  if(ncol(eiaSheet) > 7)eiaSheet <- eiaSheet[, -8]
  eiaSheet <- eiaSheet[nchar(get(names(eiaSheet)[2])) == 2, ]
  if(year <= 2007){
    setnames(eiaSheet, c("utility", "state", "ownership", "customers", "revenue", "sales_mwth", "price"))  
  }else{
    setnames(eiaSheet, c("utility", "state", "ownership", "customers", "sales_mwth", "revenue",  "price"))  
  }
  
}

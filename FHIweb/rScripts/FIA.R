library(rgdal)
library(data.table)
library(dplyr)

fiaCO <- fread("FHIweb/data/FIA/CO_TREE.csv")
fiaPlots <- fread("FHIweb/data/FIA/CO_PLOT.csv")
fiaSpecies <- fread("FHIweb/data/FIA/REF_SPECIES.csv")



fiaCO <- plotPad(fiaCO)

fiaCO <- split(fiaCO, fiaCO$INVYR)


fiaPlots <- plotPad(fiaPlots)
fiaPlots <- fiaPlots[uniquePlotID %in% fiaCO$uniquePlotID,]
coordinates(fiaPlots) = ~LON + LAT
proj4string(fiaPlots) <- CRS("+init=epsg:4326")
fiaPlots <- spTransform(fiaPlots, CRS(proj4string(watershedMap)))
fiaPlots$HUC8 <- over(fiaPlots_Points, watershedMap)$HUC8 %>% as.character()
fiaPlotsDT <- as.data.frame(fiaPlots) %>% data.table



plotPad <- function(fiaData){
  fiaData[, PLOT := sprintf("%05d", PLOT)]
  fiaData[, COUNTYCD := sprintf("%03d", COUNTYCD)]
  fiaData[, uniquePlotID := paste0(COUNTYCD, PLOT)]
  return(fiaData)
}

consistentPlots <- function(fiaTree){
  fiaTree <- fiaCO[uniquePlotID %in% fiaPlotsDT[HUC8 == "14010004", uniquePlotID]]
  fiaTree[, yearPlot := paste0(INVYR, uniquePlotID)]
  repeatPlots <- unique(fiaTree, by = "yearPlot")[, .N, by = PLOT][N > 1,]
  fiaTree <- fiaTree[PLOT %in% repeatPlots$PLOT,]
  plots <- unique(fiaTree$uniquePlotID)
  
  temp <- lapply(plots, FUN = function(x){
    currentTrees <- fiaTree[uniquePlotID == x,]
    
    currentTrees %>% group_by(INVYR, SPCD) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>% data.table()
    
  })
  
}


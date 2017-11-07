calcScore <- function(startYear, endYear, scoreTable, scoreData){
  if(!is.data.table(scoreData))scoreData <- data.table(scoreData)
  myYear <- startYear:endYear
  metric <- mean(as.numeric(t(scoreData[year %in% myYear, 2])))
  score <- metric * scoreTable$m + scoreTable$b 
  score <- ifelse(score > 100, 100 - (score - 100), score)
  return(score)
}


calcScoreTable <- function(baseData, method = "a"){
  if(method == "a"){
    sd <- baseData$sd
    baseline <- baseData$mean
  }
  scores <- 1:100
  rawScores <- seq(from = (sd * 5 + baseline), to = baseline, length.out = 100)
  lineEQ <- lm(scores~rawScores)
  return(data.table("b" = lineEQ$coefficients[1], "m" = lineEQ$coefficients[2]))
}

calcBase <- function(scoreData, baseLength = 40, baseStart = NA, baseEnd = NA, method = "SD"){
  if(!is.data.table(scoreData))scoreData <- data.table(scoreData)
  if(is.na(baseStart)){
    baseStart <- min(scoreData$year)
  }
  if(is.na(baseEnd)){
    baseEnd <- baseStart + baseLength
  }
  if(baseEnd >= max(scoreData$year)){
    baseEnd <- max(scoreData$year) - 10
  }
  if(baseEnd - baseStart < 10){
    warning("Record Not Long Enough NA returned")
    myReturn <- NA
  }else{
    baseData <- as.numeric(t(scoreData[year %in% baseStart:baseEnd, 2]))
    myReturn <- list("mean" = mean(baseData), "sd" = sd(baseData))
  }
  return(myReturn)
}

calcFF <- function(meltedStation){
  meltedStation <- meltedStation[element == "TMIN"]
  meltedStation[, FF := ifelse(value > 0, 1, 0)]
  meltedStation <- meltedStation[complete.cases(meltedStation)]
  meltedStation <- meltedStation[order(year, month, day)]
  meltedStation[, rleLength := {rr <- rle(FF); rep(rr$length, rr$length)}]
  ffYear <- meltedStation[FF == 1, max(rleLength), by = year]
  setnames(ffYear, "V1", "FF_Days")
  return(ffYear)
}

calcTempThresh <- function(meltedStation, tempF, under = F){
  tempC <- (tempF - 32) * (5/9)
  tempC <- tempC * 10
  tempC <- round(tempC, 0)
  meltedStation <- meltedStation[element == ifelse(under, "TMIN", "TMAX")]
  if(under){
    meltedStation[,hiT := ifelse(value <= tempC, 1, 0)]
    myName <- "Low_T_Days"
  }else{
    meltedStation[,hiT := ifelse(value >= tempC, 1, 0)]
    myName <- "High_T_Days"
  }
  
  hiTNA <- meltedStation[month %in% c(5,6,7,8,9), sum(is.na(hiT)), by = year]
  
  #****Right now this is arbitrary but it can be refined
  hiTExclude <- hiTNA[V1 > 15, year]
  meltedStation <- meltedStation[!year %in%  hiTExclude,]
  hiTYear <- meltedStation[,sum(hiT, na.rm = T), by = year]
  setnames(hiTYear, "V1", myName)
  return(hiTYear)
}


returnMeltedValues <- function(station, element = "all"){
  myElement <- element
  if(element != "all"){
    station <- station[element == myElement,]
  }
  station <- station[, names(station)[names(station) %in% c("id", "year", "month", "element", 
                                               paste("value", 1:31, sep = ""))], with = F]
  station <- melt(station, id.vars = c("id", "year", "month", "element"))
  station[, variable := as.character(variable)]
  station[, variable := as.numeric(substr(variable, ifelse(nchar(variable)==6, nchar(variable), nchar(variable) - 1), nchar(variable)))]
  station[value == -9999, value := NA]
  setnames(station, "variable", "day")
}

longToGrid <- function(gridMatrix, longGrid){
  # Takes gridded data in a 'long' data format and grids it based on a
  # specified template. Any cell without data in the longGrid object
  # will be assigned NA
  # 
  # Args:
  #   gridMatrix: matrix template to assign the long grid data to
  #      these values should correspond with the values under a grid column in longGrid
  #   longGrid: data to assign to a grid, must have a grid column that corresponds
  #   to the gridMatrix values and a value column to use in the new matrix
  #
  # Returns: A matrix with the same dimensions as gridMatrix and values
  # from the value column in longGrid
  
  # Copy the matrix template
  origMat <- gridMatrix
  
  # Turn the matrix template into long data with a column for ordering
  gridMatrix <- data.table(grid = (as.vector(gridMatrix)), order = 1:length(gridMatrix))
  
  # Merge long matrix template with long data
  gridMatrix <- merge(gridMatrix, longGrid[, .(grid, value)], by = "grid", all.x = T )
  
  # Create a new matrix
  gridMatrix <- matrix(gridMatrix[order(order), value], nrow = nrow(origMat), ncol = ncol(origMat))
  return(gridMatrix)
}


gridToRaster <- function(grid, rasterTemplate){
  # Simple wrapper to turn a matrix into a raster using a template
  # 
  # Args:
  #   grid: data in matrix format
  #   rasterTemplate: template to use in rasterization of the matrix
  #
  # Returns: Raster of grid data
  #
  require(raster)
  return(raster(grid, template = rasterTemplate))
}

quantileScore <- function(value, params, dist){
  # Determines quantile and then calculates a score
  # 
  # Args:
  #   value: the current year's value
  #   parameters
  #
  # Returns: 1 - 100 score based on quantitle
  #
  value <- as.numeric(value)
  quantile <- do.call(paste0("p", dist), c(list(q = value), params))
  score = 100 - abs(.5 - quantile) * 2 * 100
  
  return(score)
}

trendScore <- function(year, value){
  # Calculates linear trend in data and returns score based on this
  # info
  # 
  # Args:
  #   value: the current year's value
  #   parameters
  #
  # Returns: 1 - 100 score based on quantitle
  #
  scoreTable <- data.frame("year" = year, "value" = value)
  linearSum <- summary(lm(value~year, data = scoreTable))
  summaryData <- data.frame("estimate" = linearSum$coefficients["year", 1], 
                    "pValue" = linearSum$coefficients["year", 4])
  if(summaryData$pValue > .05){
    returnScore <- 100
  }else{
    returnScore <- 100 - 100 * (abs(summaryData$estimate) / abs(sd(value)))
  }
  return(ifelse(returnScore < 0, 0, returnScore))
}
scoreClean <- function(scores, myNames){
  if(length(myNames) != length(scores)){
    myNames <- 1:length(scores)
  }
  returnFrame <- data.frame("Station" = myNames,
                            "Score" = unlist(scores))
  returnFrame <- stargazer::stargazer(returnFrame, summary = F, type = "html", rownames = F,
                                           digits = 0)
  return(returnFrame)
}

scorer <- function(myData, myYears, method, metric, basePeriod = NULL){
  funData <- copy(myData)
  if(length(funData) == 0){
    myReturn <- "No data for selected time period"
  }else{
    value <- funData %>% lapply(function(x)x[year %in% myYears, value] %>% mean())
    if(method == "quant"){
      myReturn <- quantFHI(funData, metric, value)  
    }else if(method == "FHI"){
      myReturn <- 
        lapply(funData, calcBase, baseStart = basePeriod$start, baseEnd = basePeriod$end) %>%
        lapply(calcScoreTable)
      myReturn <- lapply(1:length(myReturn), FUN = function(x){
        calcScore(scoreTable = myReturn[[x]], startYear = min(myYears), endYear = max(myYears), scoreData = funData[[x]]) %>%
          round(digits = 2)
      })
    }else if(method == "trend"){
      myReturn <- funData %>% lapply(function(x)trendScore(year = x$year, value = x$value) %>% round(digits = 2))  
    }else{
      myReturn <- "Invalid method selected"
    }
    
  }
  return(myReturn)
}
# myData <- copy(tempSave)
# validYears <- lapply(myData, "[",, year) %>% 
#   lapply(function(x)all(c(2015:2015) %in% x)) %>% unlist()

quantFHI <- function(myData, metric, value){
    if(metric == "npp"){
      param <- myData %>% lapply(FUN = function(x)list(fhat = kde(x$value)))
      myDist <- "kde"
    }else{
      if(grepl("temp", metric)){
        myData <- lapply(myData, function(x) x[, value:= value / 10 + 273.15])
        value <- lapply(value, function(x) x / 10 + 273.15)
      }
      param <- 
        tryCatch({
          myData %>% lapply(FUN = function(x)fitdistr(x$value, "gamma")[[1]])
        }, error = function(e){
          # print(paste("dist error", e))
          tryCatch({
            myData %>% lapply(FUN = function(x)list(fhat = kde(x$value)))
          }, error = function(f){
            print(f)
            return("Unable to determine quantile")
          })
        })
      myDist <- ifelse(class(param[[1]]) == "list", "kde", 
                       ifelse(class(param[[1]]) == "numeric", "gamma", "none"))
    }
    myReturn <- 
      lapply(1:length(value), FUN = function(x)quantileScore(value = value[[x]], params = param[[x]], dist = myDist) %>%
               round(digits = 2))
    return(myReturn)
}

multiStationClean <- function(myData, stations){
  myData <- myData[name %in% stations, ]
  yearData <- myData %>% group_by(name) %>% summarise(min = min(year), max = max(year))
  myData <- myData[year %in% max(yearData$min):min(yearData$max),]
  myData <- split(myData, myData$name)
  return(myData)
}

scoreDataClean <- function(myData, groupCols, scoreYears, metric){
  if(myData$year %>% length() != myData$year %>% unique() %>% length()){
    myReturn <- as_tibble(myData) %>% 
      group_by_at(groupCols) %>%
      summarise(value = mean(value)) %>% data.table()  
  }else{
    myReturn <- myData
  }
  myReturn <- if(any(class(myReturn) == "list")) myReturn else list(myReturn)
  if(metric == "tempmax"){
    tempSave <<- myReturn
  }else{
    snowSave <<- myReturn
  }
  validYears <- lapply(myReturn, "[",, year) %>% 
    lapply(function(x)all(c(scoreYears$start:scoreYears$end) %in% x)) %>% unlist()
  # return(if(any(class(myReturn) == "list")) myReturn else list(myReturn))
  print(validYears)
  return(myReturn[validYears])
}

methodCalc <- function(myData, scoreYearTable, method, metric, basePeriod = NULL){
  returnList <- vector("list", length = scoreYearTable %>% nrow())
  for(i in 1:length(returnList)){
    years <- scoreYearTable$start[i]:scoreYearTable$end[i]
    returnList[[i]] <- scorer(myData = myData, myYears = years, method = method,
                              metric = metric, basePeriod = basePeriod)
  }
  returnList <- lapply(returnList, function(x)x %>% unlist() %>% mean()) %>% unlist()
  returnList <- cbind(scoreYearTable, "value" = (returnList %>% unlist()))
  return(returnList)
}
# 
# den <- density(myData[[1]]$value)
# dat <- data.frame(x = den$x, y = den$y)
# 
# fit.params <- fitdistr(myData[[1]]$value, "gamma")
# 
# ggplot(data = dat, aes(x = x,y = y)) + 
#   geom_point(size = 1) + geom_histogram(data = myData[[1]], aes(x = value, y = ..density..))     
#   geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
#   theme_classic()
#   
# ggplot() + geom_histogram(myData[[1]], aes("value"))
# 
# temp2 <- fitdistr(myData[[1]]$value, "gamma")
# temp3 <- lapply(value, function(y) quantileScore(value = y, params = param[[1]], dist = myDist) %>%
         # round(digits = 2) )
# lapply(1:length(value), FUN = function(x)quantileScore(value = value[[x]], params = param[[x]], dist = myDist) %>%
#          round(digits = 2))  
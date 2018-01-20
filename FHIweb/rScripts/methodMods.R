methodUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("methodComp"))
  )
}

methodMod <- function(input, output, session, rawData, yearInterval, basePeriod, metric){
  validScoreData <- reactive({
    if("name" %in% names(rawData())){
      groupCols <- c("year", "name")
      myData <- multiStationClean(rawData(), unique(rawData()$name))
    }else{
      groupCols <- "year"
      myData <- rawData()
    }
    return(scoreDataClean(myData, groupCols, list("start" = 2000, "end" = 2001), metric))
  })
  
  scoreIntervals <- reactive({
    myData <- validScoreData()[[1]]
    print(myData)
    print("max")
    print(max(myData$year))
    print("min")
    print(min(myData$year))
    print("yearInt")
    print(yearInterval())
    myInterval <- as.numeric(yearInterval())
    print(myInterval)
    endYears <- seq(from = max(myData$year), to = min(myData$year),
                    by = -myInterval)
    print(endYears)
    startYears <- seq(from = max(myData$year) - myInterval + 1, to = min(myData$year),
                      by = -myInterval)
    print(startYears)
    if(length(startYears) < length(endYears)){
      startYears <- c(startYears, rep(NA, (length(endYears) - length(startYears))))
    }
    if(length(endYears) < length(startYears)){
      endYears <- c(endYears, rep(NA, (length(startYears) - length(endYears))))
    }
    myReturn <- data.table("end" = endYears, "start" = startYears) 
    myReturn <- myReturn[complete.cases(myReturn),]
    return(myReturn)
  })
  
  fhiScore <- reactive({
    methodCalc(validScoreData(), scoreIntervals(), "FHI", metric, basePeriod = basePeriod)
  })
  
  quantScore <- reactive({
    methodCalc(validScoreData(), scoreIntervals(), "quant", metric, basePeriod = basePeriod)
  })
  
  trendScore <- reactive({
    methodCalc(validScoreData(), scoreIntervals(), "trend", metric, basePeriod = basePeriod)
  })
  
  output$methodComp <- renderUI({
    tagList(
      p(fhiScore())
    )
  })
  
}
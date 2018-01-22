methodUI <- function(id){
  ns <- NS(id)
  tagList(
    # uiOutput(ns("methodComp"))
    plotOutput(ns("compareScatter")),
    plotOutput(ns("compareBar"))
  )
}

methodMod <- function(input, output, session, rawData, yearInterval, basePeriod, metric, methodSel){
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
    myInterval <- as.numeric(yearInterval())
    endYears <- seq(from = max(myData$year), to = min(myData$year),
                    by = -myInterval)
    startYears <- seq(from = max(myData$year) - myInterval + 1, to = min(myData$year),
                      by = -myInterval)
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
  
  scoreData <- reactive({
    merge(methodCalc(validScoreData(), scoreIntervals(), methodSel()[[1]], metric, basePeriod = basePeriod),
          methodCalc(validScoreData(), scoreIntervals(), methodSel()[[2]], metric, basePeriod = basePeriod),
          by = c("start", "end"))
  })
  
  output$compareScatter <- renderPlot({
    myData <- copy(scoreData())
    setnames(myData, c("start", "end", "x", "y"))
    myData <- myData %>% rename()
    print(myData)
    ggplot(myData, aes(x, y)) + geom_point() + xlab(methodSel()[[1]]) + ylab(methodSel()[[2]]) + xlim(c(0,100)) + ylim(c(0,100))
  })
  
  output$compareBar <- renderPlot({
    plotData <- copy(scoreData())
    plotData[, plotLabel := paste0(start, "-\n", end)]
    plotData <- melt(plotData, id.vars = c("start", "end", "plotLabel"))
    ggplot(plotData, aes(x = plotLabel, y = value, fill = factor(variable))) + geom_bar(stat = "identity", position = "dodge")
  })
  
  output$methodComp <- renderUI({
    print(scoreData())
    scoreData()
  })
  
}
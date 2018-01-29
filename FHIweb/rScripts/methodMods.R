methodUI <- function(id){
  ns <- NS(id)
  tagList(
    # uiOutput(ns("methodComp"))
    plotOutput(ns("compareScatter")),
    plotOutput(ns("compareBar")),
    downloadButton(ns("downloadData"), "Download Data"),
    downloadButton(ns("downloadScores"), "Download Scores"),
    tableOutput(ns("scoreTable"))
  )
}

methodMod <- function(input, output, session, rawData, methodOptions, basePeriod, metric, methodSel){
  validScoreData <- reactive({
    myData <- rawData()
    if("name" %in% names(rawData())){
      myData[, base := year %in% basePeriod$start:basePeriod$end]
      validStations <- myData %>% group_by(name) %>% summarise(base = sum(base)) %>% filter(base >= 10)
      myData <- myData[name %in% validStations$name,]
      groupCols <- c("year", "name")
      myData <- multiStationClean(rawData(), myData$name)
    }else{
      groupCols <- "year"
    }
    print("hello")
    return(scoreDataClean(myData, groupCols, list("start" = 2000, "end" = 2001), metric))
  })
  
  scoreIntervals <- reactive({
    myData <- validScoreData()[[1]]
    myInterval <- as.numeric(methodOptions()$yearsSel)
    if(methodOptions()$rollingWin){
      myInterval <- myInterval - 1
      myReturn <- data.table("start" = min(myData$year):(max(myData$year) - myInterval), "end" = 0)
      myReturn[, end := start + myInterval]
    }else{
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
    }
    return(myReturn)
  })
  
  fhiScore <- reactive({
    print("start")
    methodCalc(validScoreData(), scoreIntervals(), "FHI", metric, basePeriod = basePeriod)
    print("end")
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
  
  scoreDisplay <- reactive({
    yearData <- copy(validScoreData())
    yearData <- rbindlist(yearData) %>% group_by(year) %>% summarise(value = mean(value)) %>% data.table()
    scoreYears <- copy(scoreIntervals())
    myData <- copy(scoreData())
    years <- vector("numeric", nrow(scoreYears))
    years <- data.table("start" = scoreYears$start, "yearValue" = 0)
    for(i in 1:nrow(scoreYears)){
      myYears <- scoreYears[i, start]:scoreYears[i, end]
      years[i, yearValue := yearData[year %in% myYears, value] %>% mean()]
    }
    myData <- merge(myData, years, by = "start")
    myData[, c("mean", "median", "sd", "iqr") := 
             list(mean(yearData$value), median(yearData$value), sd(yearData$value), IQR(yearData$value))]
    return(myData)
  })
  
  output$compareScatter <- renderPlot({
    myData <- copy(scoreData())
    setnames(myData, c("start", "end", "x", "y"))
    myData <- myData %>% rename()
    ggplot(myData, aes(x, y)) + geom_point() + xlab(methodSel()[[1]]) + ylab(methodSel()[[2]]) + xlim(c(0,100)) + ylim(c(0,100))
  })
  
  output$compareBar <- renderPlot({
    temp <- scoreDisplay()
    plotData <- copy(scoreData())
    plotData[, plotLabel := paste0(start, "-\n", end)]
    plotData <- melt(plotData, id.vars = c("start", "end", "plotLabel"))
    ggplot(plotData, aes(x = plotLabel, y = value, fill = factor(variable))) + geom_bar(stat = "identity", position = "dodge")
  })
  
  output$methodComp <- renderUI({

    scoreData()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(metric, "_data.csv")
    },
    content = function(con){
      write.csv(validScoreData(), con)
    }
  )
  
  output$downloadScores <- downloadHandler(
    filename = function() {
      paste0(metric, "_scores.csv")
    },
    content = function(con){
      write.csv(scoreDisplay(), con)
    }
  )
  
  output$scoreTable <- renderTable(scoreDisplay())
  
}
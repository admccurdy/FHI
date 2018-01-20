scoreUI <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("dataPlot")),
    uiOutput(ns("graphOptions")),
    uiOutput(ns("dataInfo"))
  )
}

scoreMod <- function(input, output, session, rawData, scoreYears, basePeriod, metric){
  
  validScoreData <- reactive({
   if("name" %in% names(rawData())){
     req(input$stationNames)
     groupCols <- c("year", "name")
     myData <- multiStationClean(rawData(), input$stationNames)
   }else{
     groupCols <- "year"
     myData <- rawData()
   }
   return(scoreDataClean(myData, groupCols, scoreYears(), metric))
  })
  
  score_FHI <- reactive({
    scorer(validScoreData(), scoringYears(), "FHI", metric, basePeriod)
  })
  
  scoringYears <- reactive({
    scoreYears()$start:scoreYears()$end
  })
  
  score_quant <- reactive({
    scorer(validScoreData(), scoringYears(), "quant", metric)
  })
  
  score_trend <- reactive({
    scorer(validScoreData(), scoringYears(), "trend", metric)
    # myData <- yearData()
    # myReturn <- myData %>% lapply(function(x)trendScore(year = x$year, value = x$value) %>% round(digits = 2))  
    # return(myReturn)
  })
  
  output$dataPlot <- renderPlot({
    if("name" %in% names(graphData())){
      ggplot(graphData(), aes(x = year, y = value, color = name)) + geom_point() + stat_smooth(method = "loess")  
    }else{
      ggplot(graphData(), aes(x = year, y = value)) + geom_point() + stat_smooth(method = "loess")  
    }
  })
  
  output$graphOptions <- renderUI({
    if("name" %in% names(rawData())){
        ns <- session$ns
        myData <- rawData()
        myData[, base := year %in% basePeriod$start:basePeriod$end]
        myData <- myData %>% group_by(name) %>% summarise(base = sum(base)) %>% filter(base >= 10)
        checkboxGroupInput(ns("stationNames"), "Select Stations", 
                    myData$name, selected = myData$name)
    }
  })
  
  graphData <- reactive({
    myReturn <-rbindlist(validScoreData())
    return(myReturn)
  })
  
  output$dataInfo <- renderUI({
    tagList(
      h5("Your score based on original FHI methods is:"),
      HTML(scoreClean(score_FHI(), names(validScoreData()))),
      h5("Your score based on quantile methods is:"),
      HTML(scoreClean(score_quant(), names(validScoreData()))),
      h5("Your score based on trend methods is:"),
      HTML(scoreClean(score_trend(), names(validScoreData())))
    )
  })
}
  
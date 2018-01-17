scoreUI <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("dataPlot")),
    uiOutput(ns("graphOptions")),
    uiOutput(ns("dataInfo"))
  )
}

scoreMod <- function(input, output, session, rawData, scoreYears, basePeriod, metric){
  
  yearData <- reactive({
   if("name" %in% names(rawData())){
     req(input$stationNames)
     groupCols <- c("year", "name")
     myData <- rawData()[name %in% input$stationNames, ]
     yearData <- myData %>% group_by(name) %>% summarise(min = min(year), max = max(year))
     myData <- myData[year %in% max(yearData$min):min(yearData$max),]
     myData <- split(myData, myData$name)
   }else{
     groupCols <- "year"
     myData <- rawData()
   }
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
   # return(if(any(class(myReturn) == "list")) myReturn else list(myReturn))
  return(myReturn)
  })
  
  validScoreData <- reactive({
    myData <- yearData()
    validYears <- lapply(myData, "[",, year) %>% 
      lapply(function(x)all(c(scoreYears()$start:scoreYears()$end) %in% x)) %>% unlist()
    return(myData[validYears])
  })
  
  score_FHI <- reactive({
    print(paste("metric", metric))
    myData <- validScoreData()
    if(!length(myData) == 0){
      myReturn <- 
        lapply(myData, calcBase, baseStart = basePeriod$start, baseEnd = basePeriod$end) %>%
        lapply(calcScoreTable)
      myReturn <- lapply(1:length(myReturn), FUN = function(x){
        calcScore(scoreTable = myReturn[[x]], startYear = scoreYears()$start, endYear = scoreYears()$end, scoreData = myData[[x]]) %>%
          round(digits = 2)
      })
        
    }else{
      myReturn <- "No data for selected time period"
    }
    return(myReturn)
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
    myReturn <-rbindlist(yearData())
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
  
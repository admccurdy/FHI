scoreUI <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("dataPlot")),
    uiOutput(ns("dataInfo"))
  )
}

scoreMod <- function(input, output, session, rawData, scoreYears, basePeriod, name){
  
  yearData <- reactive({
    if(rawData()$year %>% length() != rawData()$year %>% unique() %>% length()){
      rawData() %>% group_by(year) %>% summarise(value = mean(value)) %>% data.table()  
    }else{
      rawData()
    }

  })
  
  score_FHI <- reactive({
    if(!is.nan(yearData()[year %in% c(scoreYears()$start:scoreYears()$end), value] %>% mean())){
      myReturn <- calcBase(yearData(), baseStart = basePeriod$start, baseEnd = basePeriod$end) %>%
        calcScoreTable() %>% 
        calcScore(startYear = scoreYears()$start, endYear = scoreYears()$end, scoreData = yearData()) %>%
        round(digits = 2)
    }else{
      myReturn <- "No data for selected time period"
    }
    return(myReturn)
  })
  
  score_quant <- reactive({
    myData <- yearData()
    value <-  mean(myData[year %in% scoreYears()$start:scoreYears()$end, value])
    if(!is.nan(value)){
      if(name == "npp"){
        param <- list(fhat = kde(myData$value))
        dist <- "kde"
      }else{
        if(min(myData$value)< 0)myData$value <- myData$value + abs(min(myData$value)) + 1
        param <- fitdistr(myData$value, "gamma")[[1]]
        dist <- "gamma"
      }
      myReturn <- quantileScore(value = value, params = param, dist = dist) %>% round(digits = 2)  
    }else{
      myReturn <- "No data for selected time period"
    }
    myReturn
  })
  
  score_trend <- reactive({
    trendScore(year = yearData()$year, value = yearData()$value)
  })
  
  output$dataPlot <- renderPlot({
    ggplot(yearData(), aes(x = year, y = value)) + geom_point() + stat_smooth()
  })
  
  output$dataInfo <- renderUI({
    tagList(
      h5(paste("Your score based on original FHI methods is:", score_FHI())),
      h5(paste("Your score based on quantile methods is:", score_quant())),
      h5(paste("Your score based on trend methods is:", score_trend()))
    )
  })
}
  
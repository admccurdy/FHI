scoreUI <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("dataPlot")),
    uiOutput(ns("dataInfo"))
  )
}

scoreMod <- function(input, output, session, rawData, scoreYears){
  
  yearData <- reactive({
    rawData() %>% group_by(year) %>% summarise(avg = mean(value)) %>% data.table()  
  })
  
  
  score_FHI <- reactive({
    calcBase(yearData(), baseStart = 1980, baseEnd = 2000) %>%
      calcScoreTable() %>% 
      calcScore(startYear = scoreYears()$start, endYear = scoreYears()$end, scoreData = yearData()) %>%
      round(digits = 2)
  })
  
  score_quant <- reactive({
    myData <- yearData()
    if(min(myData$avg)< 0)myData$avg <- myData$avg + abs(min(myData$avg)) + 1
    fitdistr(myData$avg, "gamma")[[1]] %>%
      quantileScore(value = mean(myData[year %in% scoreYears()$start:scoreYears()$end, avg]), 
                    dist = "gamma") %>% round(digits = 2)
  })
  
  score_trend <- reactive({
    trendScore(year = yearData()$year, value = yearData()$avg)
  })
  
  output$dataPlot <- renderPlot({
    ggplot(yearData(), aes(x = year, y = avg)) + geom_point() + stat_smooth()
  })
  
  output$dataInfo <- renderUI({

    tagList(
      h5(paste("Your score based on original FHI methods is:", score_FHI())),
      h5(paste("Your score based on quantile methods is:", score_quant())),
      h5(paste("Your score based on trend methods is:", score_trend()))
    )
  })
}
  
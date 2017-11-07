precipScoreUI <- function(id){
  ns <- NS(id)
  tagList(
    h4("Precipitation"),
    plotOutput(ns("precipPlot")),
    uiOutput(ns("precipInfo"))
  )
}

precipScores <- function(input, output, session, precipRaw, scoreYears){
  
  precipYear <- reactive({
    precipRaw() %>% group_by(year) %>% summarise(avg = mean(value)) %>% data.table()  
  })
  
  
  precipScore_FHI <- reactive({
    calcBase(precipYear(), baseStart = 1980, baseEnd = 2000) %>%
      calcScoreTable() %>% 
      calcScore(startYear = scoreYears()$start, endYear = scoreYears()$end, scoreData = precipYear()) %>%
      round(digits = 2)
  })
  
  precipScore_quant <- reactive({
    fitdistr(precipYear()$avg, "gamma")[[1]] %>%
      quantileScore(value = mean(precipYear()[year %in% scoreYears()$start:scoreYears()$end, avg]), 
                    dist = "gamma") %>% round(digits = 2)
  })
  
  precipScore_trend <- reactive({
    trendScore(year = precipYear()$year, value = precipYear()$avg)
  })
  
  output$precipScores <- renderTable({
    # 
    # fhiScores <- sapply(years, function(x){
    #   calcScore(x, x)
    # })
  })
  output$precipPlot <- renderPlot({
    ggplot(precipYear(), aes(x = year, y = avg)) + geom_point() + stat_smooth()
  })
  
  output$precipInfo <- renderUI({
  
    # fhiScore <- 
    # trendScore <- 
    # quantileScore <- 
    tagList(
      h5(paste("Your score based on original FHI methods is:", precipScore_FHI())),
      h5(paste("Your score based on quantile methods is:", precipScore_quant())),
      h5(paste("Your score based on trend methods is:", precipScore_trend()))
    )
  })
}
  
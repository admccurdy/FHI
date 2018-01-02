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
     groupCols <- c("year", "name")
   }else{
     groupCols <- "year"
   }
    if(rawData()$year %>% length() != rawData()$year %>% unique() %>% length()){
      myReturn <- as_tibble(rawData()) %>% 
        group_by_at(groupCols) %>%
        summarise(value = mean(value)) %>% data.table()  
    }else{
      myReturn <- rawData()
    }
    return(myReturn)

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
      if(metric == "npp"){
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
    if("name" %in% names(yearData())){
      print(metric)
      temp <- split(yearData(), by = "name")
      myReturn <-
        sapply(temp, FUN = function(x)trendScore(year = x$year, value = x$value))
      myReturn <- data.frame(myReturn)
    }else{
      myReturn <- trendScore(year = yearData()$year, value = yearData()$value)  
    }
    myReturn <- round(myReturn, 2)
    print(myReturn)
    return(myReturn)
  })
  
  output$dataPlot <- renderPlot({
    if("name" %in% names(yearData())){
      ggplot(graphData(), aes(x = year, y = value, color = name)) + geom_point() + stat_smooth()  
    }else{
      ggplot(yearData(), aes(x = year, y = value)) + geom_point() + stat_smooth()  
    }
  })
  
  output$graphOptions <- renderUI({
    if("name" %in% names(yearData())){
        ns <- session$ns
        checkboxGroupInput(ns("stationNames"), "Select Stations", 
                    unique(yearData()$name), selected = unique(yearData()$name))
    }
  })
  
  graphData <- reactive({
    myReturn <-yearData()[name %in% input$stationNames, ]
    return(myReturn)
  })
  
  output$dataInfo <- renderUI({
    tagList(
      h5(paste("Your score based on original FHI methods is:", score_FHI())),
      h5(paste("Your score based on quantile methods is:", score_quant())),
      h5(paste("Your score based on trend methods is:", score_trend()))
    )
  })
}
  
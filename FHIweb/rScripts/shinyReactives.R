insectsFilter <- reactive({
  returnFrame <- insectSF2[HUC8 == input$watershedSel & year %in% input$startYear:input$endYear,]
  
  st_geometry(returnFrame) <- returnFrame$geometry
  if(nrow(returnFrame) == 0){
    returnFrame <- NULL
  }else{
    returnFrame <- st_transform(returnFrame, "+proj=longlat +datum=WGS84")  
  }
  
  return(returnFrame)
})

waterShedSF <- reactive({
  clipShapeDT[clipShapeDT$HUC8 == input$watershedSel,]
})

tmaxRaw <- reactive({
  dayMetCO[["tmax"]][grid %in% watershedKey[[input$watershedSel]]]
})

tminRaw <- reactive({
  dayMetCO[["tmin"]][grid %in% watershedKey[[input$watershedSel]]]
})

nppRaw <- reactive({
  myTable <- nppTable[HUC8 == input$watershedSel,]
  data.table("year" = myTable$year, "value" = myTable$totalNPP)
})

scoreYears <- reactive(list("start" = input$startYear, "end" = input$endYear))

# Reactive used to display filtered tree observations
treeData <- reactive({
  returnData <- existingData %>%
    filter(
      year >= input$year[1],
      year <= input$year[2]
    )
  if(!is.null(input$species) && input$species != ""){
    returnData <- returnData %>% filter(speciescommon %like% input$species)
  }
  return(returnData)
})

precipRaw <- reactive({
  if(!is.null(input$watershedSel)){
    #  This code is used to pull precip data from the sql server but is currently
    #  wayyyy too slow using a .datatable loaded in memory
    # 
    # vals <- paste(watershedKey[[input$watershedSel]], collapse = ", ")
    # rs <-  sprintf("SELECT * FROM dayMetPrecip WHERE grid in (%s)", vals) %>% 
    #   dbSendQuery(conn = dbiConn)
    # precip <- dbFetch(rs)
    dayMetCO[["prcp"]][grid %in% watershedKey[[input$watershedSel]]]
  }
})

aprilSnoRaw <- reactive({
  myReturn <- snoMetrics[["april"]][snoTelKey][HUC8 == input$watershedSel,]
})

maxSnoRaw <- reactive({
  myReturn <- snoMetrics[["max"]] %>% left_join(snoTelKey) %>% 
    filter(HUC8 == input$watershedSel) %>% data.table()
})

ercRaw <- reactive({
  ercStations <- ercStationKey[HUC8 == input$watershedSel,]
  myReturn <- coERC[.(ercStations$station),] %>% split(by = "station")
  myReturn <- lapply(myReturn, FUN = function(x){
    if(all(ercBase$start:scoreYears()$end %in% unique(x$year))){
      x
    }else{
      NULL
    }
  }) %>% rbindlist()
  myReturn[, monthDay := as.integer(paste0(month, ifelse(nchar(day) < 2, paste0(0, day), day)))]
  myReturn <- myReturn[monthDay >= 315 & monthDay < 1100,] 
  setnames(myReturn, "ERC", "value")
  return(myReturn)
})

ercProcessed <- reactive({
  myReturn <- ercRaw() %>% as_tibble() %>% group_by(year, month, day) %>%
    summarise(value = mean(value))
  averages <- myReturn %>% group_by(month, day) %>% summarise(value = mean(value))
  critValue <- quantile(averages$value, .9)
  myReturn <- myReturn %>% group_by(year) %>% summarise(value = sum(value >= critValue)) %>% data.table()
})

methodCompare <- reactive({
  list(input$methodSel1, input$methodSel2)
})

methodOptions <- reactive({
  list("yearsSel" = input$yearsSel, "rollingWin" = input$rollingWin)
})

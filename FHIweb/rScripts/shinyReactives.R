insectsFilter <- reactive({
  returnFrame <- insectSF2[HUC8 == input$watershedSel & year %in% input$startYear:input$endYear,]
  
  st_geometry(returnFrame) <- returnFrame$geometry
  returnFrame <- st_transform(returnFrame, "+proj=longlat +datum=WGS84")
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

aprilSnow <- reactive({
  myReturn <- snoMetrics[["april"]][snowWS][HUC8 == input$watershedSel,]
})
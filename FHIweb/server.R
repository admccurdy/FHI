#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output) {

  callModule(scoreMod, "precip", rawData = precipRaw, scoreYears)
  callModule(scoreMod, "tmax", rawData = tmaxRaw, scoreYears)
  callModule(scoreMod, "tmin", rawData = tminRaw, scoreYears)
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
  
  insectsFilter <- reactive({
    print(input$tabs)
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
  
  scoreYears <- reactive(list("start" = input$startYear, "end" = input$endYear))

  # Table of prior tree observations
  
  output$existingTable <- renderTable({
    returnTable <- treeData()
    return(returnTable)
  })
  
  # Reactive triggered by pressing upload file button
  myData <- eventReactive(input$uploadFile, {
    inFile <- input$dataFile
    read.csv(inFile$datapath)
  })
  
  output$uploadTable <- renderTable({
    myData()
  })
  
  output$tableDownload <- downloadHandler(
    filename = "treeObs.csv",
    content = function(file){
      write.csv(treeData(), file, row.names = F)
    },
    contentType = "text/plain"
  )
  
  output$insectMap <- renderLeaflet({
    insectColor = colorFactor("Set1", as.factor(as.character(insectsFilter()$DCA1)))
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap",
                       options = providerTileOptions(noWrap = TRUE)
                     ) %>%
      addPolygons(data = insectsFilter(),stroke = F,
                  fillColor = ~insectColor(as.factor(as.character(DCA1))),
                  fillOpacity = .5) %>%
      addPolylines(data = waterShedSF(), weight = 2, color = "Black") %>%
      addLegend(pal = insectColor, values = insectsFilter()$DCA1 )
  })
  
  # output$insectMap <- renderPlot({
  #   ggplot(insectsFilter()) + geom_sf(aes(fill = year)) + 
  #     theme()
  #   bounds <- st_bbox(clipShapeDT[NAME == "Roaring Fork", geometry])
  #   rfv_base <- get_map(location = bounds)
  #   ggplot(insectSF2[NAME == "Roaring Fork" & year %in% 2013:2016]) + geom_sf(aes(fill = year)) + 
  #     theme(axis.ticks = element_blank(), axis.text = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.background = element_blank()) + geom_sf(data = clipShapeDT[NAME == "Roaring Fork",],
  #                                                         fill = NA)
  # })
  
})

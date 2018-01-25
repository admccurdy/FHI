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
  source("rScripts/shinyReactives.R", local = T)
  source("rScripts/shinyUIOut.R", local = T)
<<<<<<< HEAD

  callModule(scoreMod, "precip", rawData = precipRaw, scoreYears, precipBase, metric = "precip")
  callModule(scoreMod, "tmax", rawData = tmaxRaw, scoreYears, tempBase, metric = "tempmax")
  callModule(scoreMod, "tmin", rawData = tminRaw, scoreYears, tempBase, metric = "tempmin")
  callModule(scoreMod, "npp", rawData = nppRaw, scoreYears, nppBase, metric = "npp")
  callModule(scoreMod, "snoApril", rawData = aprilSnoRaw, scoreYears, precipBase, metric = "snoApril")
  callModule(scoreMod, "snoMax", rawData = maxSnoRaw, scoreYears, precipBase, metric = "snoMax")
  callModule(scoreMod, "erc", rawData = ercRaw, scoreYears, ercBase, metric = "erc")
  callModule(scoreMod, "critERC", rawData = ercProcessed, scoreYears, ercBase, metric = "critERC")
  
=======
  
  # callModule(scoreMod, "precip", rawData = precipRaw, scoreYears, precipBase, metric = "precip")
  # callModule(scoreMod, "tmax", rawData = tmaxRaw, scoreYears, tempBase, metric = "tempmax")
  # callModule(scoreMod, "tmin", rawData = tminRaw, scoreYears, tempBase, metric = "tempmin")
  callModule(scoreMod, "npp", rawData = nppRaw, scoreYears, nppBase, metric = "npp")
  # callModule(scoreMod, "snoApril", rawData = aprilSnoRaw, scoreYears, precipBase, metric = "snoApril")
  # callModule(scoreMod, "snoMax", rawData = maxSnoRaw, scoreYears, precipBase, metric = "snoMax")
  # callModule(scoreMod, "erc", rawData = ercRaw, scoreYears, ercBase, metric = "erc")
  # callModule(scoreMod, "critERC", rawData = ercProcessed, scoreYears, ercBase, metric = "critERC")
  # 
>>>>>>> 995d2834476d22948305c8220ba4f73e1eda9e70
  callModule(methodMod, "precip", rawData = precipRaw, methodOptions, precipBase, metric = "precip", methodCompare)
  callModule(methodMod, "tmax", rawData = tmaxRaw, methodOptions, tempBase, metric = "tmax", methodCompare)
  callModule(methodMod, "tmin", rawData = tminRaw, methodOptions, tempBase, metric = "tmin", methodCompare)
  callModule(methodMod, "npp", rawData = nppRaw, methodOptions, nppBase, metric = "npp", methodCompare)
  callModule(methodMod, "snoApril", rawData = aprilSnoRaw, methodOptions, precipBase, metric = "snoApril", methodCompare)
  callModule(methodMod, "snoMax", rawData = maxSnoRaw, methodOptions, precipBase, metric = "snoMax", methodCompare)
  callModule(methodMod, "erc", rawData = ercRaw, methodOptions, ercBase, metric = "erc", methodCompare)
  callModule(methodMod, "critERC", rawData = ercProcessed, methodOptions, ercBase, metric = "critERC", methodCompare)
  
  # Table of prior tree observations
  
  # output$existingTable <- renderTable({
  #   returnTable <- treeData()
  #   return(returnTable)
  # })
  # 
  # Reactive triggered by pressing upload file button
  myData <- eventReactive(input$uploadFile, {
    inFile <- input$dataFile
    read.csv(inFile$datapath)
  })
  
  output$uploadTable <- renderTable({
    myData()
  })
  
  # output$tableDownload <- downloadHandler(
  #   filename = "treeObs.csv",
  #   content = function(file){
  #     write.csv(treeData(), file, row.names = F)
  #   },
  #   contentType = "text/plain"
  # )
  
  output$insectMap <- renderLeaflet({
    insectColor = colorFactor("Set1", as.factor(as.character(insectsFilter()$DCA1)))
    myMap <- leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolylines(data = waterShedSF(), weight = 2, color = "Black") %>%
      leaflet::addLegend(pal = insectColor, values = insectsFilter()$DCA1 )
    
    if(!is.null(insectsFilter())){
      myMap <- myMap %>% addPolygons(data = insectsFilter(),stroke = F,
                                     fillColor = ~insectColor(as.factor(as.character(DCA1))),
                                     fillOpacity = .5)
    }
    return(myMap)
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

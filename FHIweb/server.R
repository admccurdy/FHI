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

  callModule(scoreMod, "precip", rawData = precipRaw, scoreYears, precipBase, name = "precip")
  callModule(scoreMod, "tmax", rawData = tmaxRaw, scoreYears, tempBase, name = "tmax")
  callModule(scoreMod, "tmin", rawData = tminRaw, scoreYears, tempBase, name = "tmin")
  callModule(scoreMod, "npp", rawData = nppRaw, scoreYears, nppBase, name = "npp")
  
  
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

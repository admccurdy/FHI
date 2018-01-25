output$snoPlots <- renderUI({
  if(nrow(aprilSnoRaw()) == 0){
    h4("No snotel stations for this watershed")
  }else{
    tagList(
      h4("April Snow"),
      scoreUI("snoApril"),
      h4("Max Snow"),
      scoreUI("snoMax")
    )
  }
})

output$insectMapOut <- renderUI({
  tagList(
    if(is.null(insectsFilter())){
      h4("No insect and disese data available for this watershed")
    },
    tags$style(type = "text/css", "#insectMap {height: calc(100vh - 80px) !important;}"),
    leafletOutput("insectMap", height = "700px")
  )
})
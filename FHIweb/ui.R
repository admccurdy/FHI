
library(shiny)
# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage("Tree Data",
  
  # Sidebar with a slider input for number of bins 
  tabPanel("User Inputs",
    sidebarLayout(
      sidebarPanel(
        fileInput('dataFile', "Choose CSV file",
                  accept = c("text/csv", "text/comma-seperated-values", 
                             "text/plain")),
        actionButton('uploadFile', "Upload"),
      width = 3),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("uploadTable")
      )
    )
  ),
  tabPanel("Existing Data",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "position:fixed;width:inherit;",
        sliderInput("year", "Observation Year", min(existingData$year), max(existingData$year), 
                    value = c(min(existingData$year), max(existingData$year)),
                    step = 1, sep = ""),
        sliderInput("month", "Observation Month", 1, 12, value = c(1,12), step = 1, sep = ""),
        sliderInput("day", "Observation Day", 1, 31, value = c(1,31), step = 1, sep = ""),
        textInput("species", "Tree Species, use '|' to enter multiple species"),
        downloadButton("tableDownload", "Download")
      ),
      mainPanel(
        # style = "overflow:scroll; overflow-y: hidden",
        wellPanel(
          id = "tablePanel",
          style = "overflow-x:scroll;",
          tableOutput("existingTable")
        )
      )
    )
  ), 
  tabPanel("Get FHI Score",
     sidebarLayout(
       sidebarPanel(
         width = 4,
         selectInput("watershedSel", "Select your watershed", 
                     with(waterShedsCO, split(HUC8, name))),
         selectInput("startYear", "Select Score Start Year", choices = years, selected = max(years)),
         selectInput("endYear", "Select Score End Year", choices = years, selected = max(years))
       ),
     mainPanel(
      tabsetPanel(id = "FHI Panel",
        tabPanel("Temp and Precip",
          h3("Precip"),
          scoreUI("precip"),
          h3("Tmax"),
          scoreUI("tmax"),
          h4("Tmin"),
          scoreUI("tmin"),
          h4("NPP"),
          scoreUI("npp"),
          uiOutput("snowPlots"),
          h4("ERC"),
          scoreUI("erc"),
          h4("Critical ERC"),
          scoreUI("critERC")
        ),
        tabPanel("Insect & Disease",
          uiOutput("insectMapOut")
        )
      )
     )
     )      
    )
))

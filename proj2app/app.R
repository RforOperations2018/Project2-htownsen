#

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)

# Function to pull ESRI data
getEsri <- function(url) {
  # Make Call
  g <- GET(URLencode(url))
  c <- content(g)
  readOGR(c)
}

getEsriList <- function(url) {
  # Make Call
  g <- GET(URLencode(url))
  fromJSON(content(g))$features %>% 
    unlist() %>% 
    unname()
}

url <- URLencode("https://public.gis.lacounty.gov/public/rest/services/LACounty_Dynamic/LMS_Data_Public/MapServer/146/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=city&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=true&resultOffset=&resultRecordCount=&f=json")

cities <- getEsriList(url)


# Define UI for application that creates a map
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("LA County Police Stations"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("city_select",
                                "Select a City:",
                                choices = cities,
                                selected = "Los Angeles")
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    leafletOutput("map")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ptsLoad <- reactive({
    # Build URL Query
    url <- URLencode(paste0("https://public.gis.lacounty.gov/public/rest/services/LACounty_Dynamic/LMS_Data_Public/MapServer/146/query?where=city+like+%27", gsub(" ", "+", input$city_select), "%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=json"))
    # Get Data
    sp <- getEsri(url) %>%
      spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    return(sp)
  })
  output$map <- renderLeaflet({
    sp <- ptsLoad()
    # Call Data and Build Map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = sp, popup = ~paste0("<b>", Name, "</b><br>", addrln1, "<br>", hours, "<br>", phones, "<br>", description))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

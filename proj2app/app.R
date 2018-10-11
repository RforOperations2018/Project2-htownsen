# PROJECT 2
# HALEY TOWNSEND

# 2: Building a Leaflet App with Inputs and Web API’s
# Due Date: 10/19/2018

# To bring the entire course together students will create an interactive map that includes:
# one (1) interactive map, one (1) datatable, and two (2) interactive charts or graphs. 
# All data should be pulled from a web API’s. The app should have at least two different types 
# of layers (points/markers, lines, heatmap or polygons) and at least four (4) input commands 
# and the ability for the user to download the raw data they are viewing.
# Applications should be deployed and working on shinyapps.io.


library(shiny)
library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)

# practicing with the geojson file first to see which fields are which
#hoods <- readOGR("Neighborhoods_.geojson")

# looks like the "hood" field has the name of the neighborhoods


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

# This is the URL that grabs all PGH Neighborhoods from the Geojson ESRI REST API
# WHERE HOOD IS NOT NULL
# Try to just get the neighborhood names, not the shapes in the input, need to switch return geometry to false
url <- URLencode("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebNeighborhoods/FeatureServer/0/query?where=HOOD+IS+NOT+NULL&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=HOOD&returnGeometry=false&returnCentroid=false&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=json")
hoods <- getEsriList(url)


# Define UI for application that creates a map
ui <- fluidPage(theme = shinytheme("flatly"),

                # Application title
                titlePanel("Pittsburgh Neighborhoods"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    selectInput("hoodSelect",
                                "Select a Neighborhood:",
                                choices = hoods,
                                selected = "Shadyside")
                  ),

                  # Show a plot of the generated distribution
                  mainPanel(
                    leafletOutput("map")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  pghLoad <- reactive({
    # Build URL Query
    url <- URLencode(paste0("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebNeighborhoods/FeatureServer/0/query?where=HOOD+%3D+%27",input$hoodSelect ,"%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=json"))
    # Get Data
    sp <- getEsri(url) 
    #%>% spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    return(sp)
  })
  output$map <- renderLeaflet({
    sp <- pghLoad()
    # Call Data and Build Map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = sp) #, popup = ~paste0("<b>", Name, "</b><br>", addrln1, "<br>", hours, "<br>", phones, "<br>", description))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

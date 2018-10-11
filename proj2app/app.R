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
hoods <- readOGR("Neighborhoods_.geojson")

# looks like the "hood" field has the name of the neighborhoods


# # Function to pull ESRI data
# getEsri <- function(url) {
#   # Make Call
#   g <- GET(URLencode(url))
#   c <- content(g)
#   readOGR(c)
# }
# 
# getEsriList <- function(url) {
#   # Make Call
#   g <- GET(URLencode(url))
#   fromJSON(content(g))$features %>% 
#     unlist() %>% 
#     unname()
# }
# 
# url <- URLencode("https://public.gis.lacounty.gov/public/rest/services/LACounty_Dynamic/LMS_Data_Public/MapServer/146/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=city&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=true&resultOffset=&resultRecordCount=&f=json")
# 
# cities <- getEsriList(url)
# 
# 
# # Define UI for application that creates a map
# ui <- fluidPage(theme = shinytheme("flatly"),
#                 
#                 # Application title
#                 titlePanel("LA County Police Stations"),
#                 
#                 # Sidebar with a slider input for number of bins 
#                 sidebarLayout(
#                   sidebarPanel(
#                     selectInput("city_select",
#                                 "Select a City:",
#                                 choices = cities,
#                                 selected = "Los Angeles")
#                   ),
#                   
#                   # Show a plot of the generated distribution
#                   mainPanel(
#                     leafletOutput("map")
#                   )
#                 )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   ptsLoad <- reactive({
#     # Build URL Query
#     url <- URLencode(paste0("https://public.gis.lacounty.gov/public/rest/services/LACounty_Dynamic/LMS_Data_Public/MapServer/146/query?where=city+like+%27", gsub(" ", "+", input$city_select), "%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=json"))
#     # Get Data
#     sp <- getEsri(url) %>%
#       spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#     
#     return(sp)
#   })
#   output$map <- renderLeaflet({
#     sp <- ptsLoad()
#     # Call Data and Build Map
#     leaflet() %>%
#       addTiles() %>%
#       addCircleMarkers(data = sp, popup = ~paste0("<b>", Name, "</b><br>", addrln1, "<br>", hours, "<br>", phones, "<br>", description))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

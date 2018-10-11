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

# Loading in the necessary libraries
library(shiny)
library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)
library(xml2)

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

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  # trying to get rid of the empty strings too
  json <- gsub("NaN|''", 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# This data is from the WPRDC.
# BikePGH -> Autonomous Vehicle Survey of Bicyclists and Pedestrians in Pittsburgh, 2017

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

# This is the URL that grabs all PGH Neighborhoods from the Geojson ESRI REST API
# WHERE HOOD IS NOT NULL
# Try to just get the neighborhood names, not the shapes in the input, need to switch return geometry to false
url1 <- URLencode("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebNeighborhoods/FeatureServer/0/query?where=HOOD+IS+NOT+NULL&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=HOOD&returnGeometry=false&returnCentroid=false&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=json")
hoods <- sort(getEsriList(url1))


# Define UI for application that creates a map
ui <- navbarPage("Pittsburgh Neighborhoods", theme = shinytheme("flatly"),
                 tabPanel("Interactive PGH Map",
                          leafletOutput("map", height='700px'),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",

                    selectInput("hoodSelect",
                                "Select a Neighborhood:",
                                choices = hoods,
                                selected = "Shadyside"),
                    style = "opacity: 0.92"
                          )
                 ),
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData", "Download Data Here")
                            ),
                          fluidPage(DT::dataTableOutput("table")))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  pghLoad <- reactive({
    # Build URL Query
    url1 <- URLencode(paste0("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebNeighborhoods/FeatureServer/0/query?where=HOOD+%3D+%27", gsub(' ', '+', input$hoodSelect),
                            "%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=json"))
    # Get Data
    hoodpolys <- getEsri(url1) %>% spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    return(hoodpolys)
  })
  
  watersdf <- reactive({
    # Build API Query with proper encodes
    # Also filter by the three inputs 
    # Using gsub to deal with spaces for certain factor levels like "Not familiar at all"
    url2 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22%20WHERE%20%22neighborhood%22%20%3D%27",
                   gsub(' ', '%20', input$hoodSelect), "%27"
    )
    watermarks <- ckanSQL(url2)
    
    return(watermarks)
  })
  
  output$map <- renderLeaflet({
    hoodpolys <- pghLoad()
    waters <- watersdf()
    icon.water <- makeAwesomeIcon(icon = "tint", markerColor = "blue",
                                  iconColor = "black", library = "glyphicon")
    
    # Call Data and Build Map
    leaflet(width="100%", height="100%") %>%
      addTiles() %>%
      addPolygons(data = hoodpolys) %>%
      
      # if ((dim(waters))[1]!=0) {
        addAwesomeMarkers(data = waters, lng = ~longitude, lat = ~latitude, icon = icon.water)
      # }
  })
  
  # Data Table Output containing information from the input fields
  output$table <- DT::renderDataTable({
    df <- pghLoad()
    #subset(df, select = c(SafetyRating, SurveyDate, ProvingGroundFeel, AVTechFamiliarity, ZipCode))
  })
  
  # Download data in the datatable
  # Must "Open in Browser" (the app) in order for the download to work as expected
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PGH-Neighborhood-", input$hoodSelect, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pghLoad(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

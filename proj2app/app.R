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
library(plotly)
library(ggplot2)
library(htmltools)
library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)
library(xml2)
library(gghighlight)

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

# Grab the unique water feature types from url2
watertypes <- sort(ckanUniques("513290a6-2bac-4e41-8029-354cbda6a7b7", "feature_type")$feature_type)


# Define UI for application that creates a map
ui <- navbarPage("Pittsburgh Neighborhoods", theme = shinytheme("flatly"),
                 tabPanel("Interactive Map",
                          leafletOutput("map", height='700px'),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                    # Input 1: Select Neighborhood
                    selectInput("hoodSelect",
                                "Select a Neighborhood:",
                                choices = hoods,
                                selected = "Shadyside"),
                          
                    # Input 2: Select water feature type
                    checkboxGroupInput("waterSelect",
                                   "Select Water Feature Types:",
                                   choices = watertypes,
                                   selected = c("Drinking Fountain", "Decorative", "Spray")),
                    # Input 3: Only include the most dependable water features
                    checkboxInput("bestSelect", tags$b("Check this box to only see the most dependable water features"), value = FALSE, width = '100%'),
                    style = "opacity: 0.92"
                    )
                 ),
                 tabPanel("Downloadable Table",
                          # Input 4: Option to get data for all the PGH neighborhoods
                          checkboxInput("allSelect", tags$b("Check this box to see water feature data for ALL Pittsburgh neighborhoods"), value = FALSE, width = '100%'),
                          inputPanel(
                            downloadButton("downloadData", "Download Data Here")
                          ),
                          fluidPage(DT::dataTableOutput("table"))),
                 
                 tabPanel("Transportation Plots",
                          fluidRow(
                            column(5, plotlyOutput("plot1")),
                            column(7, plotlyOutput("plot2"))
                          )
                          )
                 
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
    # Also filter by the inputs 
    # Building an IN selector
    water_filter <- ifelse(length(input$waterSelect) > 0, 
                           paste0("%20AND%20%22feature_type%22%20IN%20(%27", paste(input$waterSelect, collapse = "%27,%27"),"%27)"),
                           "")
    
    # Using gsub to deal with spaces for certain factor levels with spaces in them
    url2 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22%20WHERE%20%22neighborhood%22%20%3D%27",
                   gsub(' ', '%20', input$hoodSelect),"%27", gsub(' ', '%20', water_filter)
    )
    watermarks <- ckanSQL(url2)
      return(watermarks)
  })
  
  watersdfall <- reactive({
    # Build API Query with proper encodes
    # Also filter by the inputs 
    # Building an IN selector
    water_filter <- ifelse(length(input$waterSelect) > 0, 
                           paste0("feature_type%22%20IN%20(%27", paste(input$waterSelect, collapse = "%27,%27"),"%27)"),
                           "")
    
    # Using gsub to deal with spaces for certain factor levels with spaces in them
    # only filter by water feature type, not by neighborhood
    url2 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22%20WHERE%20%22",
                   gsub(' ', '%20', water_filter)
    )
    watermarks <- ckanSQL(url2)
    return(watermarks)
  })
  
  
  transpodf <- reactive({
    # Build API Query with proper encodes
    # Also filter by neighborhood selected
    url3 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%225d61b60b-bd25-4c33-8420-e31a9135ec6e%22%20WHERE%20%22Neighborhood%22%20%3D%27",
                   gsub(' ', '%20', input$hoodSelect), "%27"
    )
    transpo <- ckanSQL(url3)
    return(transpo)
  })
  
  transpodfall <- reactive({
    # Build API Query with proper encodes
    # Don't filter by neighborhood now
    url3 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%225d61b60b-bd25-4c33-8420-e31a9135ec6e%22"
    )
    transpo <- ckanSQL(url3)
    return(transpo)
  })
  
  
  output$map <- renderLeaflet({
    hoodpolys <- pghLoad()
    waters <- watersdf()
    icon.water <- makeAwesomeIcon(icon = "tint", markerColor = "blue",
                                  iconColor = "black", library = "glyphicon")
    
    # Call Data and Build Map
    if (nrow(waters)>0 & input$bestSelect==F){
      leaflet(width="100%", height="100%") %>%
        addTiles() %>%
        addPolygons(data = hoodpolys, popup = ~paste0("<b>", hood, "</b><br>", acres, " acres")) %>%
        addAwesomeMarkers(data = waters, lng = ~longitude, lat = ~latitude, icon = icon.water,
                          popup = ~paste0("<b>", name, "</b><br>", feature_type))
      
      # if there are no markers (water features) for a neighborhood
      # then it won't matter whether input$bestSelect is T or F
    } else if (nrow(waters)==0) {
      leaflet(width="100%", height="100%") %>%
        addTiles() %>%
        addPolygons(data = hoodpolys, popup = ~paste0("<b>", hood, "</b><br>", acres, " acres"))
      
    } 
    else if (nrow(waters)>0 & input$bestSelect==T) {
      watersbest <- filter(waters, make=='Most Dependable')

      leaflet(width="100%", height="100%") %>%
        addTiles() %>%
        addPolygons(data = hoodpolys, popup = ~paste0("<b>", hood, "</b><br>", acres, " acres")) %>%
        addAwesomeMarkers(data = watersbest, lng = ~longitude, lat = ~latitude, icon = icon.water,
                          popup = ~paste0("<b>", name, "</b><br>", feature_type, "<br>", make))
    }
    
  })
  
  output$plot1 <- renderPlotly({
    
    dat <- transpodf()
    # data for plotly pie chart

    dat$Commute.to.Work..Drive.Alone..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Drive.Alone..2010.)))
    dat$Commute.to.Work..Public.Transportation..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Public.Transportation..2010.)))
    dat$Commute.to.Work..Motorcycle..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Motorcycle..2010)))
    dat$Commute.to.Work..Walk..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Walk..2010)))
    dat$Commute.to.Work..Carpool.Vanpool..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Carpool.Vanpool..2010)))
    dat$Commute.to.Work..Taxi..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Taxi..2010.)))
    dat$Commute.to.Work..Bicycle..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Bicycle..2010.)))
    dat$Commute.to.Work..Other..2010. <-  as.numeric(as.character(gsub('%', '', dat$Commute.to.Work..Other..2010.)))
    dat$Work.at.Home..2010. <-  as.numeric(as.character(gsub('%', '', dat$Work.at.Home..2010.)))
    
    dat2 = dat[,c("Commute.to.Work..Drive.Alone..2010.",	"Commute.to.Work..Public.Transportation..2010.", "Commute.to.Work..Motorcycle..2010.",
                        "Commute.to.Work..Walk..2010.","Work.at.Home..2010.",	"Commute.to.Work..Carpool.Vanpool..2010.",
                        "Commute.to.Work..Taxi..2010.","Commute.to.Work..Bicycle..2010.","Commute.to.Work..Other..2010.")]
    
    # Make the colnames more appropriate for the plot, could have done this earlier but oh well
    names(dat2)[1]<-"Drive Alone"
    names(dat2)[2]<-"Public Transportation"
    names(dat2)[3]<-"Motorcycle"
    names(dat2)[4]<-"Walk"
    names(dat2)[5]<-"Work at Home (Don't Commute)"
    names(dat2)[6]<-"Carpool"
    names(dat2)[7]<-"Taxi"
    names(dat2)[8]<-"Bicycle"
    names(dat2)[9]<-"Other"
    
    plot_ly(dat2, labels = colnames(dat2), values = as.numeric(dat2[1,]) , type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent', showlegend = FALSE) %>%
      layout(title = paste0('How do ', input$hoodSelect, ' Residents Commute to Work?'))
    
  })
  
  # PLOT 2: Histogram of Total Street Miles, highlighting the portion of the histogram
  # corresponding to the selected neighborhood
  output$plot2 <- renderPlotly({
    dat <- transpodfall()
    hood <- dat['Neighborhood'==input$hoodSelect,]
    miles <- as.numeric(dat$Total.Street.Miles)
    milesh <- as.numeric(hood$Total.Street.Miles)
    
    ggplotly(
      ggplot(data = dat, aes(x = as.numeric(Total.Street.Miles), 
                             text = paste0("<b>", Neighborhood, ": </b>", as.numeric(Total.Street.Miles), " miles"))) +
        geom_histogram(binwidth=5, fill = "orange", color = 'white') + ggtitle(paste0("Road/Street Miles in Pittsburgh Neighborhoods")) +
      xlab("Distribution of Total Street Miles") + 
        ylab("Number of Neighborhoods (out of 90)") +
        gghighlight(Neighborhood == input$hoodSelect), tooltip = 'text' 
      )
  })
  
  
  # Data Table Output containing information from the input fields from leaflet map
  output$table <- DT::renderDataTable({
    if (input$allSelect==F) {
      dfw <- watersdf()
      subset(dfw, select = c(neighborhood, name, feature_type, make))
      
    } else {
      dfall <- watersdfall()
      subset(dfall, select = c(neighborhood, name, feature_type, make))
    }
    
  })
  
  # Download data in the datatable
  # Must "Open in Browser" (the app) in order for the download to work as expected
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PGH-Neighborhood-", input$hoodSelect, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (input$allSelect==F) {
        dfw <- watersdf()
        csvdata<- subset(dfw, select = c(neighborhood, name, feature_type, make))
        
      } else {
        dfall <- watersdfall()
        csvdata <- subset(dfall, select = c(neighborhood, name, feature_type, make))
      }
      write.csv(csvdata, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

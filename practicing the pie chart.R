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


transpo = read.csv("transportation.csv")
tshady = transpo[transpo$Neighborhood=='Shadyside',]
tshady2 = tshady[,c("Commute.to.Work..Drive.Alone..2010.",	"Commute.to.Work..Public.Transportation..2010.", "Commute.to.Work..Motorcycle..2010.",
                         "Commute.to.Work..Walk..2010.","Work.at.Home..2010.",	"Commute.to.Work..Carpool.Vanpool..2010.",	
                         "Commute.to.Work..Taxi..2010.","Commute.to.Work..Bicycle..2010.","Commute.to.Work..Other..2010.")]

vals = tshady2[2,]

plot_ly(tshady2, labels = colnames(tshady2), values = vals, type = 'pie')

#Globae=l zip map table chart

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(sf)
library(DT)
library(shinythemes)
library(shinydashboard)
library(scales)
library(flexdashboard)
library(plotly)

zip_shp <- readRDS("zip_shp.rds")
zipData <- readRDS("zipData.rds")


zip_shp@data <- zip_shp@data %>%
  mutate(cashReturn = (medianRentEst * .75) * 12 / hvi) %>%
  mutate(cashFlow = medianRentEst * .75 - paymentMonthly30) %>%
  select(zip, City, State, REMV, cashReturn, cashFlow, medianRentEst, paymentMonthly30, hvi, medianIncome, totalPopulation, Lat, Long) #%>%
#arrange(desc(cashReturn))


#Create data glossary
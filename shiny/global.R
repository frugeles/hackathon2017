library(data.table)
library(dplyr)
library(shiny)
#library(shinythemes)
library(shinydashboard)
library(scales)
library(plotly)
library(tidyr)
library(rgdal)
library(rgeos)
library(leaflet)
library(RColorBrewer)


source("./modules/data_management.R")

### dummy geodata
source("./modules/geodata_management.R")


Type <- unique(DF$Type)
Date <- unique(DF$Date)   
Time <- unique(DF$Time)   

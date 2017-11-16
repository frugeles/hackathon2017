library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(rgdal)
library(rgeos)
library(leaflet)
library(ggmap)

data_path = './data/'

Seattle <- readOGR(dsn=paste0(data_path,'seattle_zones/WGS84/'),layer='Neighborhoods')

## load data
plot(Seattle)

Seattle@data
## get centroide
centr.STL <- gCentroid(Seattle)@coords

## plot map using leaflet
##  addTiles() for classic tiles, addProviderTiles() for .. providers tiles !

leaflet(options = leafletOptions(minZoom = 5, maxZoom = 14)) %>% 
  #addTiles() %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  setView(centr.STL[1], centr.STL[2], zoom = 11) %>% 
  addPolygons(data=Seattle, weight = 1,  color="firebrick",
              opacity=1, fillOpacity = 0.1, 
              highlight = highlightOptions(
                weight = 1,
                color = "grey70",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = ~as.character(
                paste(L_HOOD, "-", S_HOOD))
  )  

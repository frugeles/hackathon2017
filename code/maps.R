#### Libraries ####

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(rgdal)
library(rgeos)
library(leaflet)
library(ggmap)
library(sp)

#### Seattle districts data and plot ####
data_path = './data/'

#Seattle <- readOGR(dsn=paste0(data_path,'seattle_zones/WGS84/'),layer='Neighborhoods')
#save.image(paste0(data_path,'Seattle.RData'))

## load data
load(file = paste0(data_path,'Seattle.RData'))
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


#### Loading of 911 calls ####
calls <- read.csv2(paste0(data_path,'call_data.csv'))




spoints <- sp::SpatialPointsDataFrame(coords      = cbind(calls$Longitude, calls$Latitude),
                                      bbox        = outlines.shape@bbox,
                                      data        = building_permits,
                                      # We know that the CRS of seatl.bounds.shape is WGS84, since the data was downloaded from the same website, we assume that the crs 
                                      # is the same for both sources (WGS84 is also the most common crs). Also, based on the plot that follows, we know that it is OK.
                                      proj4string = outlines.shape@proj4string, 
                                      match.ID    = TRUE)

outlines.shape.to.plot <- outlines.shape[sample(1000), ] # Take small sample to plot
leaflet::leaflet(outlines.shape.to.plot) %>% 
  leaflet::addTiles() %>%
  leaflet::addPolygons(color = "red") %>%
  leaflet::addCircles(lng = spoints$Longitude, lat = spoints$Latitude,
                      weight = 1, 
                      radius = 1)

matched          <- sp::over(spoints, outlines.shape)
spoints.fin@data <- cbind(spoints@data, matched$SHAPE_AREA)

save(spoints.fin, file = "./Data/building_dat.RData")





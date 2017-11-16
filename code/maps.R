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

# calls <- read.csv2(paste0(data_path,'call_data.csv'))
# calls_clean <- calls[!(as.character(calls$Latitude)==''|as.character(calls$Latitude)==' '),]
# calls_clean$Address <- as.character(calls_clean$Address)
# calls_clean$Datetime <- as.Date(calls_clean$Datetime)
# calls_clean$Longitude <- as.numeric(levels(calls_clean$Longitude))[calls_clean$Longitude]
# calls_clean$Latitude <- as.numeric(levels(calls_clean$Latitude))[calls_clean$Latitude]
# save.image(paste0(data_path,'calls.RData'))

# Example
load(paste0(data_path,'calls.RData'))
calls <- calls_clean[1:1000,]
spoints <- sp::SpatialPointsDataFrame(coords      = cbind(calls$Longitude, calls$Latitude),
                                      bbox        = Seattle@bbox,
                                      data        = calls,
                                      # We know that the CRS of seatl.bounds.shape is WGS84, since the data was downloaded from the same website, we assume that the crs 
                                      # is the same for both sources (WGS84 is also the most common crs). Also, based on the plot that follows, we know that it is OK.
                                      proj4string = Seattle@proj4string, 
                                      match.ID    = TRUE)

leaflet::leaflet(Seattle) %>% 
  leaflet::addTiles() %>%
  leaflet::addPolygons(color = "red") %>% 
  leaflet::addCircles(lng = spoints@data$Longitude, lat = spoints@data$Latitude)



# Get the district ID for the calls
calls <- calls_clean
spoints <- sp::SpatialPointsDataFrame(coords      = cbind(calls$Longitude, calls$Latitude),
                                      bbox        = Seattle@bbox,
                                      data        = calls,
                                      # We know that the CRS of seatl.bounds.shape is WGS84, since the data was downloaded from the same website, we assume that the crs 
                                      # is the same for both sources (WGS84 is also the most common crs). Also, based on the plot that follows, we know that it is OK.
                                      proj4string = Seattle@proj4string, 
                                      match.ID    = TRUE)

matched          <- sp::over(spoints, Seattle)
calls['districtID'] <- matched$OBJECTID
calls['district_name'] <- matched$S_HOOD
calls['region_name'] <- matched$L_HOOD

save(calls, file = paste0(data_path,'calls_district.RData'))


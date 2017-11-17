#### Libraries ####

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(rgdal)
library(rgeos)
library(leaflet)
library(ggmap)
library(sp)


#### Leaflet ####

data_path = './data/'

load(paste0(data_path,'calls_district.RData'))
calls <- calls[!is.na(calls$districtID),]


nb_calls_district <- calls %>% 
  group_by(districtID) %>% 
  summarise(colorTest=n()*100/nrow(calls))




# load Seattle data
load(file = paste0(data_path,'Seattle.RData'))

test <- right_join(nb_calls_district,Seattle@data,by=c('districtID'='OBJECTID'))

Seattle@data <- test

bins <- quantile(nb_calls_district$colorTest, c(0, .25, .5, .75, 1))
pal <- colorBin("RdYlBu", domain = Seattle@data$colorTest, bins = bins,
                na.color = "grey40", reverse = T)

centr.STL <- gCentroid(Seattle)@coords


l <-  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 14)) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  setView(centr.STL[1], centr.STL[2], zoom = 11) %>% 
  addLegend(pal = pal, values = round(Seattle@data$colorTest, 1), 
            opacity = 0.7, position = "bottomright", title = "Percentage of total calls to 911")



l %>%  addPolygons(data=Seattle, weight = 1, 
                   fill = ~colorTest, fillColor = ~pal(colorTest),
                   opacity=1, fillOpacity = 0.6, color=grey(0.5),
                   ## USE POPUP
                   popup = ~as.character(
                     paste(S_HOOD, L_HOOD, "<br>", "Percentage =", round(colorTest, 2)))
) 




## load map data once
  data_path <- './../data/'
  load(file = paste0(data_path,'Seattle.RData'))
  

## centroid coordinates of each regions
  centr.STL <- gCentroid(Seattle)@coords
  
## leaflet background
  l <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 14)) %>% 
    addProviderTiles("Esri.WorldImagery") %>%
    setView(centr.STL[1], centr.STL[2], zoom = 10)
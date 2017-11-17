
## load map data once
  France <- readOGR(dsn="./data/maps", layer="FRA_dept")


## create additionnal KPI's at regionId level
  
  KPI.per.reg <- reportDF %>%
    group_by(regionId, LOB, network) %>%
    summarize(NB = sum(newBusiness),
              canc = sum(cancellation),
              portfolio = sum(portfolio)) %>%
    mutate(NB.rate = NB/portfolio,
           canc.rate = canc/portfolio,
           netInflow = as.numeric(NB - canc)) %>%
    select(-c(NB, canc, portfolio)) %>%
    gather(kpi, values, c(NB.rate, canc.rate, netInflow)) %>%
    unite(tmp, LOB, network, kpi, sep = "_") %>%
    spread(tmp, values) %>%
    as.data.table()
  
## merge with @data slot to make it available for leaflet
  France@data <- left_join(France@data, KPI.per.reg, by = c("DEPT_ID" = "regionId"))
  

## centroid coordinates of each regions
  centr <- rgeos::gCentroid(France)@coords
  
## leaflet background
  l <- leaflet() %>% 
    addTiles() %>% 
    setView(centr[1], centr[2], zoom = 5) 
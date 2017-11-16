# Setup -------------------------------------------------------------------
need <- c("devtools",  "leaflet",   "raster", "rgdal",       "data.table", "downloader", 
          "eurostat",  "deldir",    "sp",     "countrycode", "RCurl",      "osrm", 
          "RSelenium", "plyr",      "dplyr",  "RJSONIO",     "stringi",    "ggthemes",
          "ggplot2",   "RSelenium", "rgeos",  "maptools",    "osmar",      "maps")
# Setup
ins  <- installed.packages()[, 1]
(Get <- need[which(is.na(match(need,ins)))])
if(length(Get) > 0) {install.packages(Get)}
eval(parse(text = paste("library(", need, ")")))

source("functions.R")

# Disable scientific notation
options(scipen = 999)

# Enter variables and policy longitude/latitude data
country      <- "Seattle, Washington"
country_fold <- "seattle"
nbr_years    <- 10
pol.dat <- read.csv("./Data/pol_dat/building_data.csv", sep = ",")
rad_dists <- c(1, 5, 10, 20)

# Directory Set-up
dir.create("./Data",                              showWarnings = FALSE)
dir.create("./Data/shape",                        showWarnings = FALSE)
dir.create(paste0("./Data/shape/", country_fold), showWarnings = FALSE)
dir.create("./Data/weather",                      showWarnings = FALSE)
dir.create("./Data/POIs/",                        showWarnings = FALSE)
dir.create("./Data/Altitude/",                    showWarnings = FALSE)
dir.create(paste0("./Data/POIs/", country_fold),  showWarnings = FALSE)

# Work a bit with the policy data -----------------------------------------
pol.dat <- pol.dat[, c("Latitude", "Longitude")]
names(pol.dat) <- c("lat", "lon")
pol.dat$lat <- round(pol.dat$lat, 3)                  # Rounding to 3 decimal results in a maximum difference of 70m (see code at end for proof)
pol.dat$lon <- round(pol.dat$lon, 3)                  # It also results in a database significantly smaller than the original
pol.dat$key <- paste0(pol.dat$lat, "_", pol.dat$lon)
pol.dat <- pol.dat[!duplicated(pol.dat$key), ]
pol.dat$latitude  <- pol.dat$lat
pol.dat$longitude <- pol.dat$lon

# Weather Data ------------------------------------------------------------
# Weather data to be acquired
y_beg     <- Sys.Date() - nbr_years * 365
y_end     <- Sys.Date()
dates     <- seq.Date(from = y_beg, to = y_end, by = 'month')

# Use weather function to acquire the data for the past 4 years
weather_data <- weather(country = country, dates = dates, by_year_month = TRUE, verbose = TRUE)
save(weather_data, file = './Data/weather/weather_seattle.RData')

# POI Data ----------------------------------------------------------------
download.file(url      = paste0("http://download.geofabrik.de/north-america/us/washington-latest.osm.pbf"),
              destfile = paste0("./Data/POIs/", country_fold, "/latest.osm.pbf"))

# Load dictionary
pois_dict <- read.csv(paste0("./Data/POIs/", country_fold, "/poi_types_dict.csv"), sep = ";") # http://wiki.openstreetmap.org/wiki/Map_Features
pois_dict <- pois_dict[!is.na(pois_dict$CODE), ]

# Jump to terminal and start up Docker ------------------------------------
# Run each of the following strings in the same sequence - VERY SLOW!!!
paste0("cd ", getwd(), "/Data/POIs/", country_fold)
paste0("docker run -t -v $(pwd):/data osrm/osrm-backend osrm-extract -p /opt/car.lua /data/latest.osm.pbf")
paste0("docker run -t -v $(pwd):/data osrm/osrm-backend osrm-contract /data/latest.osrm")
paste0("docker run -t -i -p 5000:5000 -v $(pwd):/data osrm/osrm-backend osrm-routed /data/latest.osrm")

# Load POI Data
poi <- poiLoader(mainDir = getwd(), country = country_fold)

# Run some elementary checks
cats <- as.data.frame(table(poi$category))
summary(poi$lat)
summary(poi$lon)

# First get the longtitude and latitude information from the policies.
# Get long lat data from policy data - the long lat will act as a unique key to merge back
longlat <- data.frame(lat_lon_key      = paste0(pol.dat$latitude, "_", pol.dat$longitude),
                      lat              = pol.dat$latitude,
                      lon              = pol.dat$longitude,
                      stringsAsFactors = FALSE)

# Remove all NA long/lat data
longlat <- longlat[!grepl(pattern = "NA", longlat$lat_lon_key), ]
# Take only unique lon/lat data
longlat <- longlat[!duplicated(longlat$lat_lon_key), ]

# Find OSRM crs
download.file(url = "http://download.geofabrik.de/north-america/us/washington-latest-free.shp.zip", destfile = paste0("./Data/shape/", country_fold, "/washington_shape.zip"))
unzip(paste0("./Data/shape/", country_fold, "/washington_shape.zip"), exdir = paste0("./Data/shape/", country_fold))
washington.shape <- rgdal::readOGR(dsn = paste0("./Data/shape/", country_fold)) 

# Remove long/lat that is outside of seattle boundaries
download.file(url = "https://data.seattle.gov/download/gf6u-sgut/application%2Fzip", destfile = paste0("./Data/shape/", country_fold, "/seatle_shape.zip"))
unzip(paste0("./Data/shape/", country_fold, "/seatle_shape.zip"), exdir = paste0("./Data/shape/", country_fold))
foldr <- list.dirs(path = paste0("./Data/shape/", country_fold), full.names = TRUE, recursive = TRUE)[grepl(x = list.dirs(path = paste0("./Data/shape/", country_fold), full.names = TRUE, recursive = TRUE), pattern = "WGS84")]
seatl.bounds.shape <- rgdal::readOGR(dsn = foldr) 

spoints <- SpatialPointsDataFrame(coords      = cbind(longlat$lon, longlat$lat),
                                  bbox        = seatl.bounds.shape@bbox,
                                  data        = longlat,
                                  # We know that the CRS of seatl.bounds.shape is WGS84, since the data was downloaded from the same website, we assume that the crs 
                                  # is the same for both sources (WGS84 is also the most common crs). Also, based on the plot that follows, we know that it is OK.
                                  proj4string = seatl.bounds.shape@proj4string, 
                                  match.ID    = TRUE)

plot(seatl.bounds.shape, lwd = 0.5)
plot(spoints, col = "red", cex = 0.1, lwd = 1, add = TRUE) 

matched <- sp::over(spoints, seatl.bounds.shape)
spoints <- spoints[!is.na(matched$OBJECTID), ]

# Add POI data to policy adress
# Then we need to loop over all types of POIs
POI_Data <- NULL
starter  <- 0
for (p in pois_dict$CODE) {
  print(p)
  # Filter data:
  poi_sub <- poi[which(poi$category == p), ]
  if (nrow(poi_sub) == 0) { next() }
  # Dedupe POIs with same lon/lat
  poi_sub$key <- paste0(poi_sub$lat, poi_sub$lon)
  poi_sub     <- poi_sub[!duplicated(poi_sub$key), ]
  poi_sub     <- subset(poi_sub, select = -key)
  
  poi_sh <- sp::SpatialPointsDataFrame(coords      = cbind(poi_sub$lon, poi_sub$lat), 
                                       bbox        = seatl.bounds.shape@bbox,
                                       data        = poi_sub, 
                                       proj4string = seatl.bounds.shape@proj4string, # this is true since : as.character(washington.shape@proj4string) == as.character(seatl.bounds.shape@proj4string)
                                       match.ID    = TRUE)
  
  matched <- sp::over(poi_sh, seatl.bounds.shape)
  # Remove POIs that does not fall in the gadm bounds
  poi_sh <- poi_sh[!is.na(matched$OBJECTID), ]
  
  # poi_sh@data
  # 
  # plot(seatl.bounds.shape, lwd = 0.5)
  # plot(poi_sh, col = "red", cex = 0.1, lwd = 1, add = TRUE) 
  
  if (nrow(poi_sh) == 0) { next() }
  
  # Build partitioning
  # cf. https://flowingdata.com/2016/04/12/voronoi-diagram-and-delaunay-triangulation-in-r/
  # Create shape file from POI information
  
  print("Creating voronoi polygon...")
  # Create voronoi polygon from the shape file created
  if (nrow(poi_sh) == 1) {
    voronoi_polygon <- poi_sh
  } else {
    voronoi_polygon <- SPointsDF_to_voronoi_SPolysDF(sp = poi_sh)
  }
  print("... Voronoi polygon created")
  
  # Create an example plot of a voronoi plot - only do this once
  if (p == pois_dict$CODE[1]) {
    vorplot(vp = voronoi_polygon, country)
  }
  
  # Add the POI data to the points data
  spoints_over <- over(spoints, voronoi_polygon)
  data.table::setnames(x = spoints_over, old = c("lat", "lon"), new = c("poi_lat", "poi_lon"))
  spoints_fin <- cbind(spoints@data, spoints_over)
  
  if (1 - sum(is.na(spoints_fin$poi_lat))/nrow(spoints_fin) < 0.01) { next() } # If only applicable for less than 1% of risks
  if (starter == 0) {starter <- p}
  
  # Distance and time
  spoints_fin$Dist_Short <- GeoDist(latA = spoints_fin$lat, latB = spoints_fin$poi_lat, 
                                    lngA = spoints_fin$lon, lngB = spoints_fin$poi_lon)
  spoints_fin$Dist_car <- 0
  spoints_fin$Time_car <- 0
  
  print("Calculating distances...")
  batch_size <- 100
  for (i in 1:ceiling(nrow(spoints_fin) / batch_size)) {
    print((i * batch_size) / nrow(spoints_fin))
    from <- max((i - 1) * batch_size, 1)
    to   <- min(i * batch_size, nrow(spoints_fin))
    temp <- spoints_fin[from:to, c("lat", "lon", "poi_lat", "poi_lon")]
    req  <- c(paste0("http://127.0.0.1:5000/route/v1/driving/", temp$lon, ",", temp$lat, ";", temp$poi_lon, ",", temp$poi_lat, "?overview=false"))
    result <- RCurl::getURI(req)
    
    # Parse the results
    res <- unlist(lapply(result, function(x) {
      outp <- jsonlite::fromJSON(x)
      return(paste0(outp$routes$distance, "_", outp$routes$duration))
    }))
    
    spoints_fin$Dist_car[from:to] <- gsub(x = res, pattern = "(.*)_(.*)", replacement = "\\1")
    spoints_fin$Time_car[from:to] <- gsub(x = res, pattern = "(.*)_(.*)", replacement = "\\2")
  }
  print("... Distances calculated")
  
  # Rename columns
  spoints_fin <- subset(spoints_fin, select = c("lat_lon_key", "Dist_Short", "Time_car", "Dist_car"))
  data.table::setnames(spoints_fin, c("Dist_Short", "Time_car", "Dist_car"), paste(p, c("Dist_Short", "Time_car", "Dist_car"), sep = "_"))
  
  # Merge POI data
  if (p == pois_dict$CODE[starter]) {
    POI_Data <- spoints_fin
  } else {
    POI_Data <- merge(x = POI_Data, y = spoints_fin, by = "lat_lon_key", all = TRUE)
  }
}

longlat <- merge(longlat, POI_Data, by = "lat_lon_key", all.x = TRUE)

save(longlat, file = './Data/pol_dat/pol_lon_lot_POI.RData')

# SHUT DOWN Docker if needed
# paste0("docker stop $(docker ps -a -q)")
# paste0("docker rm $(docker ps -a -q)")
# paste0("docker rmi $(docker images -q)")
### Optional - only run if you are sure you won't need to run the same container again ###
# paste0("rm ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/Docker.qcow2") 

# Next create circular polygons around each risk adress to count the number of different POIs in a certain radius around the risk adress
# https://gis.stackexchange.com/questions/79529/calculate-a-polygon-around-a-point-and-conver-to-wkt
# Notes:
# 1 - The German data uses the WGS82 crs (which is the same as the gadm crs)
# 2 - The WGS projection units is in degrees and not in km
# 3 - The EPSG projection is in km, thus we can transform the projection (temporarily) to this crs
# 4 - Create the circles on the transformed data (the distances is subjective)
# 5 - On the new data transfrom crs back to original
# 6 - The voronoi polygon is just an approximation, so sometimes the closest distance to a POI is incorrectly calculated, however, these are only for boundary cases. This will result in some instances where you will find that the number of POIs in a particular radius (let's say 10km) is 1, but the closest POI is 11km away.
pol_rad <- longlat[, c("lat_lon_key", "lon", "lat")]
sp::coordinates(pol_rad) <- ~lon + lat
sp::proj4string(pol_rad) <- seatl.bounds.shape@proj4string
pol_rad <- sp::spTransform(pol_rad, raster::crs("+init=epsg:2154"))
for (d in rad_dists) {
  pol_rad_temp <- rgeos::gBuffer(pol_rad, width = 1000 * d,  byid = TRUE)
  pol_rad_temp <- sp::spTransform(pol_rad_temp, raster::crs(seatl.bounds.shape))
  for (p in pois_dict$CODE) {
    print(paste0(d, " : ", p))
    
    # Filter data:
    poi_sub <- poi[which(poi$category == p), ]
    
    if (nrow(poi_sub) == 0) { next() }
    
    # Dedupe POIs with same lon/lat
    poi_sub$key <- paste0(poi_sub$lat, poi_sub$lon)
    poi_sub     <- poi_sub[!duplicated(poi_sub$key), ]
    poi_sub     <- subset(poi_sub, select = -key)
    
    # Remove POIs that does not fall in the gadm bounds
    poi_sh <- SpatialPointsDataFrame(coords      = cbind(poi_sub$lon, poi_sub$lat), 
                                     bbox        = seatl.bounds.shape@bbox,
                                     data        = poi_sub, 
                                     proj4string = seatl.bounds.shape@proj4string,
                                     match.ID    = TRUE)
    matched <- sp::over(poi_sh, seatl.bounds.shape)
    poi_sh <- poi_sh[!is.na(matched$OBJECTID), ]
    
    if (nrow(poi_sh) == 0) { next() }
    
    points_in <- as.data.frame(unlist(over(poi_sh, pol_rad_temp, returnList = TRUE)))
    data.table::setnames(x = points_in, "lat_lon_key")
    
    if (nrow(points_in) == 0) { next() }
    
    points_in$lat_lon_key <- as.character(points_in$lat_lon_key)
    points_in <- points_in[!is.na(points_in$lat_lon_key), ]
    totals    <- data.frame(table(points_in))
    data.table::setnames(x = totals, c("lat_lon_key", paste0(p, "_count_rad_", d)))
    if (p == pois_dict$CODE[1]) { fin_p1 <- totals } else { fin_p1 <- merge(fin_p1, totals, by = "lat_lon_key", all = TRUE) }
  }
  if (d == rad_dists[1]) { fin_p2 <- fin_p1 } else { fin_p2 <- merge(fin_p2, fin_p1, by = "lat_lon_key", all = TRUE) }
}

longlat <- merge(longlat, fin_p2, by = "lat_lon_key", all.x = TRUE)

# Decorrelate the data
outp <- decor_data(df.in = longlat, id.vars = c("lat_lon_key", "lat", "lon"))
# Check which variables to drop
outp$to_drop
# Replace original data
longlat <- outp$df.out
rm(outp)

save(longlat, file = './Data/pol_dat/pol_dat_fin.RData')































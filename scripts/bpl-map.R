# This script creates a leaflet map of EW PA Sensors and Boston Public Library locations.
library(sf)
devtools::load_all()
library(leaflet)
library(ggplot2)
library(dplyr)
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
source("revised-functions/revised_pas_leaflet.R")


MazamaSpatialUtils::setSpatialDataDir("data/spatial")
MazamaSpatialUtils::loadSpatialData()


#--------------------------------------------------------------------------------------------------
# Visualizes EW sensor and Boston Public Libraries on map.

bpl_sf <- st_read(dsn = "./inputs/BPL")

bcd <- data.frame(site = "Beaver Country Day School", lat = 42.319380, lon = -71.165025)

icons <- awesomeIcons(
  icon = 'bookmark', markerColor = "black",  library = 'ion')

# TODO: Add Beaver County Day and Grove Hall manually as grey circles. 

fetch_pas(countryCodes = c("US")) %>%
  pas_filter(label %in% sensor_catalog$`Sensor Label`) %>% 
  revised_pas_leaflet(outsideOnly = FALSE, group = "Sensors") %>%
  addCircleMarkers(lat = bcd$lat, lng = bcd$lon, radius = 10, group = "Sensors",
                   opacity = 0.8, color = "grey", fillColor = "grey", fillOpacity = 0.8) %>% 
  addAwesomeMarkers(data = bpl_sf, icon = icons, options = markerOptions(opacity = 0.7), group = "Boston Public Libraries") %>% 
  addLayersControl(baseGroups = "OpenStreetMap", overlayGroups = c("Boston Public Libraries", "Sensors")) %>% 
  mapview::mapshot(url = "outputs/graphics/bpl_widget.html")
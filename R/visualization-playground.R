# This script is for testing visualizations.
library(sf)
source("R/revised-functions/revised_con2aqi.R")
source("R/revised-functions/revised_pas_leaflet.R")

#--------------------------------------------------------------------------------------------------
# Visualizes EW sensor and Boston Public Libraries on map.

bpl_sf <- st_read(dsn = "./inputs/BPL")

bcd <- data.frame(site = "Beaver Country Day School", lat = 42.319380, lon = -71.165025)

icons <- awesomeIcons(
  icon = 'bookmark', markerColor = "black",  library = 'ion')

# TODO: Add Beaver County Day and Grove Hall manually as grey circles. 

pas %>%
  pas_filter(label %in% sensor_catalog$`Sensor Label`) %>% 
  revised_pas_leaflet(outsideOnly = FALSE, group = "Sensors") %>%
  addCircleMarkers(lat = bcd$lat, lng = bcd$lon, radius = 10, group = "Sensors",
                   opacity = 0.8, color = "grey", fillColor = "grey", fillOpacity = 0.8) %>% 
  addAwesomeMarkers(data = bpl_sf, icon = icons, options = markerOptions(opacity = 0.7), group = "Boston Public Libraries") %>% 
  addLayersControl(baseGroups = "OpenStreetMap", overlayGroups = c("Boston Public Libraries", "Sensors")) %>% 
  mapview::mapshot(url = "outputs/graphics/bpl_widget.html")

#--------------------------------------------------------------------------------------------------
# Line plot of hourly average aqi with lines for weekdays and for weekends.
week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")

example <- pat_list[[1]] %>%
  pat_createAirSensor(period = "1 hour", parameter = "pm25") %>% 
  sensor_extractData() %>% 
  select(datetime, pm25 = 2) %>% 
  mutate(aqi = con2aqi(pollutant = "pm25", con = pm25), 
         weekday = factor(weekdays(datetime)), 
         hour = format(datetime, "%H"),
         `weekend` = factor(ifelse(weekday %in% weekend, 1, 0), labels = c("workweek", "weekend")))

example %>% 
  group_by(hour, weekend) %>% 
  summarize(aqi = mean(aqi)) %>% 
  ggplot() +
  geom_line(aes(x = hour, y = aqi, group = weekend, color = weekend))

#-----------------------------------------------------------------------------------------------------
# AQI Average Hourly Line plots faceted by day of the week.
example %>% 
  group_by(weekday) %>% 
  ggplot() +
  geom_line(aes(x = hour, y = aqi, group = 1, color = weekday)) +
  guides(color = FALSE) +
  facet_wrap(~weekday)



#------------------------------------------------------------------------------------------------------









#----------------------------------------------------------------
# Visualizes EW sensors and nearby Reference monitors.

pas %>%
  pas_filter(label %in% sensor_catalog$`Sensor Label`) %>% 
  pas_leaflet(outsideOnly = FALSE, 






# This script is our hourly aggregated QA/QC system.
library(dplyr)
devtools::load_all(path = "C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
devtools::load_all()


# Making list of PA-II sensors.
pa_ii_sensors <- 
  load_SensorCatalog() %>% 
  filter(
    # Filter sensor_catalog for sensors of type PA-II.
    stringr::str_detect(string = `Sensor Type`, pattern = "PA-II"),
    # Filter sensor_catalog for deployed sensors.
    ) %>% pull(label)

# Filtering pat_list for just PA-II sensors. 
pat_list <- pat_list[names(pat_list) %in% pa_ii_sensors]

# Checks elements of pat_list whether they are or are not pats.
pat_test <- purrr::map_lgl(.x = pat_list, .f = pat_isPat)
pat_list <- pat_list[pat_test]
# Checks elements of pat_list whether time series is empty or not.
empty_test <- purrr::map_lgl(.x = pat_list, .f = function(x) nrow(pat_extractData(x)) > 1)
pat_list <- pat_list[empty_test]
# Generating hourly aggregated pat list with lots of additional data.
outliercount_list <- purrr::map(pat_list, pat_qaqc_outliercounts) %>% 
  # Removing observations that didn't pass our QAQC.
  purrr::map(.f = .qaqc_filtering)

# Creating AirSensor object. This allows us to generate AQI.
sensor_list <- purrr::map(pat_list,
                             .f = function(x) (pat_outliers(x, replace = TRUE, showPlot = FALSE) %>% pat_createAirSensor()))
# Using NowCast AQI algorithm to create AirSensor objects with AQI. 
sensor_aqi_list <- purrr::map(sensor_list, .f = PWFSLSmoke::monitor_aqi)

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

# 1. Apply pat_qc to remove invalid readings.
pat_list_qcd <- pat_list %>% 
  purrr::map(.f = pat_qc)

# Generating hourly aggregated pat list with lots of additional data.
aggstats_list <- purrr::map(pat_list_qcd, pat_qaqc_outliercounts)

sensor_list <- pat_list_qcd %>% 
  # Creating AirSensor object. This allows us to generate AQI.
  purrr::map(.f = function(x) (pat_outliers(x,
                                            replace = TRUE,
                                            showPlot = FALSE) %>%
                               pat_createAirSensor(parameter = "pm25",
                                                  channel = "ab"))
             )

saveRDS(aggstats_list, file = "data/aggstats_list.rds")
saveRDS(sensor_list, file = "data/sensor_list.rds")

# This script is our hourly aggregated QA/QC system.
library(dplyr)
devtools::load_all(path = "C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
devtools::load_all()

parameters <- c("pm25", "humidity", "temperature")

# Make list of PA-II sensors.
pa_ii_sensors <- 
  load_SensorCatalog() %>% 
  filter(
    # Filter sensor_catalog for sensors of type PA-II.
    stringr::str_detect(string = `Sensor Type`, pattern = "PA-II")
    ) %>% pull(label)

# Filter pat_list for just PA-II sensors. 
pat_list <- pat_list[names(pat_list) %in% pa_ii_sensors]

# Check elements of pat_list whether they are or are not pats.
pat_test <- purrr::map_lgl(.x = pat_list, .f = pat_isPat)
pat_list <- pat_list[pat_test]
# Check elements of pat_list whether time series is empty or not.
empty_test <- purrr::map_lgl(.x = pat_list,
                             .f = function(x) nrow(pat_extractData(x)) > 1)

pat_list <- pat_list[empty_test]

pat_list_qcd <- pat_list %>% 
  # 1. Apply pat_qc to remove invalid readings.  
  purrr::map(.f = pat_qc) %>% 
  # 2. Count and replace outliers in dataset.
  # 3. Aggregate data to hourly.
  purrr::map(pat_outliers,
             windowSize = 23,
             thresholdMin = 8,
             replace = TRUE,
             showPlot = FALSE,
             flag_columns = TRUE)

# Make a sensor object for every pat for every parameter.

# Empty list.
sensor_list <- list()

for (i in 1:length(pat_list_qcd)) {
  label <- pat_list_qcd[[i]][["meta"]][["label"]]
  sensor_list[[label]] <- list()
  # Make sensor object for each parameter.
  for (j in 1:length(parameters)) {
    parameter <- parameters[[j]]
    
    sensor_list[[label]][[parameter]] <- 
      pat_createAirSensor(pat = pat_list_qcd[[label]],
                          parameter = parameter,
                          qc_algorithm = "hourly_AB_01",
                          min_count = 20,
                          period = "1 hour")
  }
}

################ Outputs ########################
# pat_list_qcd has had invalid measurements removed, and outliers either removed or imputed.
saveRDS(pat_list_qcd, file = "data/pat_list_qcd.rds")
# sensor_list of AirSensor objects for each input pat and input parameter specified. 
saveRDS(sensor_list, file = "data/sensor_list.rds")

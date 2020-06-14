# This script calibrates a sensor with a given calibration.
library(dplyr)
library(AirSensor)

proxy_site4file <- proxy_site %>%
  stringr::str_to_lower() %>%
  stringr::str_replace_all(pattern = " ",
                           replacement = "-") %>%
  stringr::str_c(".rds", sep = "")

mod_path <- paste("data", "calibration-models", proxy_site4file, sep = "/")

mod <- readRDS(mod_path)$model

#------------------------------------------------------------------------------
# Calibrate pat_list_qcd:
pat_list_qcd <- readRDS("data/pat_list_qcd.rds")

for (i in 1:length(pat_list_qcd)) {

  # To calibrate a PAT, must calibrate each channel individually.
  data <- pat_list_qcd[[i]] %>%
    pat_extractData()

  data[["pm25_A"]] <- data %>%
    select(pm25_pa = pm25_A, temperature, humidity) %>%
    modelr::add_predictions(model = mod) %>%
    pull(pred)

  data[["pm25_B"]] <- data %>%
    select(pm25_pa = pm25_B, temperature, humidity) %>%
    modelr::add_predictions(model = mod) %>%
    pull(pred)


  pat_list_qcd[[i]][["data"]] <- data

}

saveRDS(pat_list_qcd, "data/pat_list_calibrated.rds")
#------------------------------------------------------------------------------
# Calibrate sensor_list:
sensor_list <- readRDS("data/sensor_list.rds")

for (i in 1:length(sensor_list)) {
  sensor_list[[i]][["pm25"]] <-
    AirSensor::pat_createAirSensor(pat = pat_list_qcd[[i]])
}

saveRDS(sensor_list, "data/sensor_list_calibrated.rds")

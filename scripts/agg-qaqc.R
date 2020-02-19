# This script is our hourly aggregated QA/QC system.
library(dplyr)
devtools::load_all(path = "C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
source("R/qaqc-util.R")

pa_ii_sensors <- 
  sensor_catalog %>% 
  filter(
    # Filter sensor_catalog for sensors of type PA-II.
    stringr::str_detect(string = `Sensor Type`, pattern = "PA-II"),
    # Filter sensor_catalog for deployed sensors.
    ) %>% pull(label)

pat_list <- pat_list[pa_ii_sensors]

# Checks elements of pat_list whether they are or are not pats.
pat_test <- purrr::map_lgl(.x = pat_list, .f = pat_isPat)
pat_list <- pat_list[pat_test]


# Generating new pat list by applying qaqc function.
agg_list <- purrr::map(pat_list, pat_qaqc_agg)
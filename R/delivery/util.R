

make_site_wkbk <- function(site, pat_list, sensor_catalog) {
  # Outputs an excel workbook. 
  # TODO: Improve docstring.
  # TODO: Error control system.
  pats <- get_site_pats(sensor_catalog = sensor_catalog, pat_list = pat_list, site = site)
  
  # Filtering out not pat objects.
  pats <- pats[map_lgl(pats, pat_isPat)]
  
  # Creating workbook obects.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  filename <- paste("outputs/datasets/data-deliveries/", site, "-", lubridate::today(), ".xlsx", sep = "")
  
  meta_list <- purrr::map(pats, AirSensor::pat_extractMeta)
  
  
  meta_df <- dplyr::bind_rows(meta_list) %>% 
    dplyr::select(label) %>%
    dplyr::left_join(sensor_catalog, by = c("label" = "Sensor Label")) %>% 
    dplyr::select(-c("MAC ID", "Sensor ID", "...13", "Person of Contact", "Purchasing Email",
                     "Program", "Picture of Sensor"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb = wb, sheet = "Sensor Info", x = meta_df)
  
  # Creates list of Air Sensor Objects with pm25 hourly averages.
  pm25_list <- purrr::map(pats, AirSensor::pat_createAirSensor,
                          parameter = "pm25", qc_algorithm = "hourly_AB_00") %>% 
    purrr::map(AirSensor::sensor_extractData) %>% 
    purrr::map(dplyr::rename, "Particulate Matter 2.5 (Hourly Average)" = 2)
  
  # Creates list of Air Sensor Objects with Temperature hourly averages.
  temp_list <- purrr::map(pats, AirSensor::pat_createAirSensor,
                          parameter = "temperature", qc_algorithm = "hourly_AB_00") %>% 
    purrr::map(AirSensor::sensor_extractData) %>% 
    purrr::map(dplyr::rename, "Temperature (C)" = 2)
  
  # Creates list of Air Sensor Objects with Humidity Hourly Averages
  humid_list <- purrr::map(pats, AirSensor::pat_createAirSensor,
                           parameter = "humidity", qc_algorithm = "hourly_AB_00") %>% 
    purrr::map(AirSensor::sensor_extractData) %>% 
    purrr::map(dplyr::rename, "Humidity (Pct)" = 2)
  
  
  sensors <- names(pm25_list)
  
  for (i in 1:length(sensors)){
    sensor <- sensors[[i]]
    
    df <- pm25_list[[sensor]] %>% left_join(temp_list[[sensor]], by = "datetime") %>%
      left_join(humid_list[[sensor]], by = "datetime") %>%
      mutate("Air Quality Index" = revised_con2aqi(pollutant = "pm25",
                                                   con = `Particulate Matter 2.5 (Hourly Average)`,
                                                   na.rm = TRUE))
    openxlsx::addWorksheet(wb, sheetName = sensor)
    openxlsx::writeData(wb = wb, sheet = sensor, x = df)
  }
  openxlsx::saveWorkbook(wb, file = filename)
}


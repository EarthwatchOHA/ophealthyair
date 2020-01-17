

create_PA_workbook <- function(pats, sensor_catalog) {
  # Creating excel workbook obect.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  # Vulnerable to changes in Sensor_catalog.
  meta_list <- purrr::map(pats, AirSensor::pat_extractMeta)
  
  meta_df <- dplyr::bind_rows(meta_list) %>% 
    dplyr::select(label) %>%
    dplyr::left_join(sensor_catalog, by = c("label" = "Sensor Label")) %>% 
    dplyr::select(-c("MAC ID", "Sensor ID", "Person of Contact", "Purchasing Email",
                     "Program", "Picture of Sensor"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb = wb, sheet = "Sensor Info", x = meta_df)
  
  # Creates list of Air Sensor Objects with pm25 hourly averages.
  pm25_list <- purrr::map(pats, AirSensor::pat_createAirSensor,
                          parameter = "pm25", qc_algorithm = "hourly_AB_00") %>% 
    purrr::map(AirSensor::sensor_extractData) %>% 
    purrr::map(dplyr::rename, pm25 = 2)
  
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
  
  # Creates a joined time series of PM data from all sensors at site (column names are sensor labels).
  aqi_joined <- pm25_list %>% 
    purrr::reduce(full_join, by = "datetime") %>%
    mutate_if(is.numeric, revised_con2aqi, pollutant = "pm25", na.rm = TRUE) %>% 
    `colnames<-`(c("datetime", names(pm25_list))) 
  
  openxlsx::addWorksheet(wb, sheetName = "AQI Side-by-Side")
  openxlsx::writeData(wb = wb, sheet = "AQI Side-by-Side", x = aqi_joined)
  
  sensors <- names(pm25_list)
  
  for (i in 1:length(sensors)){
    sensor <- sensors[[i]]
    
    df <-  select(aqi_joined, datetime, "Air Quality Index" = sensor) %>% 
      left_join(pm25_list[[sensor]], by = "datetime") %>% 
      left_join(temp_list[[sensor]], by = "datetime") %>%
      left_join(humid_list[[sensor]], by = "datetime")
    
    openxlsx::addWorksheet(wb, sheetName = sensor)
    openxlsx::writeData(wb = wb, sheet = sensor, x = df)
  }
  return(wb)
}
  
make_site_wkbk <- function(site, pat_list, sensor_catalog) {
  # Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
  
  # Note: that it currently just filters by min. 20 obs per hour.
    # TODO: Error control system.
  
  pats <- get_site_pats(sensor_catalog = sensor_catalog, pat_list = pat_list, site = site)
  
  # Filtering out not pat objects.
  pats <- pats[purrr::map_lgl(pats, AirSensor::pat_isPat)]
  
  # Instantiates and fills workbook object.
  wb <- create_PA_workbook(pats = pats, sensor_catalog = sensor_catalog)
  
  filename <- paste("outputs/datasets/data-deliveries/", site, "-", lubridate::today(), ".xlsx", sep = "")
  
  openxlsx::saveWorkbook(wb, file = filename)
}
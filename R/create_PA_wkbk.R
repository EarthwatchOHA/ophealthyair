create_PA_wkbk <- function(agg_list, sensor_catalog, aqi_col = TRUE) {
  # TODO: Add DocString
  
  # Creating excel workbook obect.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  sensors <- names(agg_list)
  
  # Vulnerable to changes in Sensor_catalog.
  meta_df <- sensor_catalog %>%
    filter(label %in% sensors) %>% 
    dplyr::select(-c("MAC ID", "id", "Person of Contact", "Purchasing Email",
                     "Program", "Picture of Sensor"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb, sheet = "Sensor Info", x = meta_df)
  
  agg_list_prepped <- 
    purrr::map(agg_list, .prep_agg_wkbk_data, aqi_col = aqi_col)
  
  if (aqi_col) {
    # Creates a joined time series of AQI data for each sensor at site (column names are sensor labels).
    aqi_joined <- agg_list_prepped %>% 
      purrr::reduce(full_join, by = "datetime") %>% 
      dplyr::select(datetime, contains("aqi")) %>% 
      `colnames<-`(c("datetime", names(agg_list_prepped)))
    # Writing to workbook
    openxlsx::addWorksheet(wb, sheetName = "AQI Side-by-Side")
    openxlsx::writeData(wb = wb, sheet = "AQI Side-by-Side", x = aqi_joined)
  } else {
    # Creates a joined time series of PM data for each sensor at site (column names are sensor labels).
    pm_joined <- agg_list_prepped %>% 
      purrr::reduce(full_join, by = "datetime") %>% 
      dplyr::select(datetime, contains("pm")) %>% 
      `colnames<-`(c("datetime", names(agg_list_prepped))) %>% 
      dplyr::mutate_if(is.numeric, round, digits = 2)
    # Writing to workbook
    openxlsx::addWorksheet(wb, sheetName = "PM Side-by-Side")
    openxlsx::writeData(wb = wb, sheet = "PM Side-by-Side", x = pm_joined)
  }  
  
  for (i in 1:length(sensors)) {
    sensor <- sensors[[i]]
    
    df <- agg_list_prepped[[sensor]] %>% 
      dplyr::mutate(
        date = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
        time = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
      ) %>% 
      # Removing total datetime for date and time variables. 
      dplyr::select(-datetime) %>%
      dplyr::select(date, time, pm25, outlierCount, everything()) %>% 
      # Rounding numeric variables.
      dplyr::mutate_if(is.numeric, round, digits = 2)
    
    # Wkbk sheets must have names shorter than 31 chars.
    if (nchar(sensor) > 31) {
      sensor_sheetname <- stringr::str_remove_all(sensor, " ")
      while(nchar(sensor_sheetname) > 31) {
        # Find numeric index position of middle character.
        middle_position <- round(nchar(sensor_sheetname) / 2)
        # Pastes everything before middle position to everything after middle position (effectively removing middle position character).
        sensor_sheetname <- 
          paste(
            substr(sensor, start = 1, stop = middle_position - 1),
            substr(sensor, start = middle_position + 1, stop = nchar(sensor)),
            sep = ""
          )      
      }
    }
    
    openxlsx::addWorksheet(wb, sheetName = sensor)
    openxlsx::writeData(wb = wb, sheet = sensor, x = df)
  }
  
  return(wb)
}
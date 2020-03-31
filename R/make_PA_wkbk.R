make_PA_wkbk <- function(
  outliercount_list,
  sensor_aqi_list,
  sensor_catalog,
  use_aqi = FALSE) {
  # TODO: Add DocString
  
  # Creating excel workbook obect.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  sensors <- names(outliercount_list)
  
  # Vulnerable to changes in Sensor_catalog.
  meta_df <- sensor_catalog %>%
    filter(label %in% sensors) %>% 
    dplyr::select(-c("MAC ID", "id", "Person of Contact", "Purchasing Email",
                     "Program", "Picture of Sensor", "Collocated", "Sensor Type", "Contact email"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb, sheet = "Sensor Info", x = meta_df)
  
  data_prepped <- outliercount_list %>%
    purrr::map(.f = function(x) x %>%
                                dplyr::mutate(temperature = (temperature_mean - 32) * 5/9) %>% # Fahrenheit to Celsius
                                dplyr::select("datetime", "temperature",
                                              "humidity" = "humidity_mean", "pm25"))
      
  # Loading data dictionary 
  data_dict_df <- .load_data_dictionary()
  
  if (use_aqi) {
    # Extracts data from sensor_aqi_list.
    aqi_prepped <- purrr::map(.x = sensor_aqi_list, .f = function(x) x$data)
    
    # Add aqi column to data_prepped dfs.
    data_prepped <- purrr::map2(.x = data_prepped, .y = aqi_prepped, 
                                .f = function(x, y) left_join(x = x, y = rename(y, aqi = 2), by = "datetime") %>% 
                                  rename("US AQI" = aqi))
  } else {
    # Removing aqi column from data dictionary.
    data_dict_df <- data_dict_df %>% 
      filter(!`Column Heading` == "aqi")
  }  
  
  # Creates a joined time series of PM data for each sensor at site (column names are sensor labels).
  pm_joined <- data_prepped %>% 
    purrr::reduce(full_join, by = "datetime") %>% 
    dplyr::select("datetime (UTC)" = datetime, contains("pm")) %>% 
    `colnames<-`(c("datetime (UTC)", names(outliercount_list))) %>% 
    dplyr::mutate_if(is.numeric, round, digits = 2)
  # Writing to workbook
  openxlsx::addWorksheet(wb, sheetName = "PM Side-by-Side")
  openxlsx::writeData(wb = wb, sheet = "PM Side-by-Side", x = pm_joined)
  
  for (i in 1:length(sensors)) {
    sensor <- sensors[[i]]
    
    df <- data_prepped[[sensor]] %>% 
      dplyr::mutate(
        date = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
        time = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
      ) %>% 
      # Removing total datetime for date and time variables. 
      dplyr::select(-datetime) %>%
      dplyr::select(date, "time (UTC)" = time, "humidity %" =  humidity, "temperature (C)" = temperature,
                    "PM2.5 μg/m3" =  pm25, everything()) %>% 
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
      sensor <- sensor_sheetname
    }
    openxlsx::addWorksheet(wb, sheetName = sensor)
    openxlsx::writeData(wb = wb, sheet = sensor, x = df)
  }
  
  # Write data_dict_df to sheet. 
  openxlsx::addWorksheet(wb, sheetName = "Data Dictionary")
  openxlsx::writeData(wb = wb, sheet = "Data Dictionary", x = data_dict_df)
  
  return(wb)
}

#-------------------------------------------------------------------------------------------------------------------------
.load_data_dictionary <- function() {
  # Helper function loads data dictionary for addition to workbook.
  data_dict_sheet <- readxl::read_excel(path = "inputs/data-viz-package-wkbk-dictionary.xlsx")
  return(data_dict_sheet)
}

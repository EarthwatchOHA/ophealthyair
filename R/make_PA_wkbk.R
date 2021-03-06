#' @title
#'
#' \code{make_PA_wkbk} returns a filled Workbook object from the openxlsx
#' package.
#'
#' @description Create a Workbook object filled with Particulate Matter data
#'   from aggstats_list, a pre-generated Data Dictionary Sheet.
#'
#' @param sensor_list
#' @param aqi_data
#' @param sensor_catalog
#' @param data_dict_path
#' @param use_aqi
#'
#' @return A Workbook object with several sheets. 
#'   \itemize{
#'     \item{a Sensor Info sheet, from information supplied by inputted 
#'           \code{sensor_catalog}
#'           }
#'     \item{A sheet for every element of \code{aggstats_list} (sheet
#'           name corresponding to list element name), containing sensor
#'           hourly average measurements of PM25, Temperature, Relative 
#'           Humidity, and if \code{use_aqi = TRUE}, the US AQI generated 
#'           using the NowCast algorithm provided by Mazama Science's PWFSL
#'           Smoke package monitor_aqi function. 
#'           See \code{vignette("NowCast", package = "PWFSLSmoke")} for
#'           algorithm details
#'           }
#'     \item{A PM Side-by-Side sheet with sensor hourly PM measurements 
#'           in adjacent columns, with column headings as sensor labels.
#'           }
#'     \itme{Data Dictionary Sheet from excel file specified by data_dict_path.
#'           Default is a pre-fabricated sheet used for Operation Healthy Air.
#'           }
#'
#' @example


make_PA_wkbk <- function(
  sensor_list,
  sensor_catalog,
  aqi_data = NULL,
  data_dict_path = "inputs/data-viz-package-wkbk-dictionary.xlsx"
) {
  # TODO: Add DocString
  
  # Creating excel workbook obect.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  sensors <- names(sensor_list)
  
  # Vulnerable to changes in Sensor_catalog.
  meta_df <- sensor_catalog %>%
    dplyr::filter(label %in% sensors) %>% 
    dplyr::select(-c("MAC ID", "id", "Person of Contact", "Purchasing Email",
                     "Program", "Sensor Photo", "Collocated", "Sensor Type", "Contact email"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb, sheet = "Sensor Info", x = meta_df)
  
  
  # Subset AirSensors with PM data.
  pm_data <- sensor_list %>% 
    purrr::map("pm25") %>% 
    purrr::map(.f = function(x) dplyr::rename(x$data, pm25 = 2))
  # Subset AirSensors with Temp data.  
  temp_data <- sensor_list %>% 
    purrr::map("temperature") %>% 
    purrr::map(.f = function(x) rename(x$data, temperature = 2))
  # Subset AirSensors with Humidity data.
  humidity_data <- sensor_list %>% 
    purrr::map("humidity") %>% 
    purrr::map(.f = function(x) rename(x$data, humidity = 2))
  
  # Combining all sensor variables into single dataframes for modelling.
  data <- 
    purrr::map2(.x = pm_data, .y = temp_data,
                .f = left_join, by = "datetime") %>%
    purrr::map2(.y = humidity_data, 
                .f = left_join, by ="datetime") %>% 
    purrr::map(.f = function(x) x %>%
                                dplyr::mutate(
                                  # Fahrenheit to Celsius
                                  temperature = (temperature - 32) * 5/9,
                                  day_of_week = weekdays(datetime)
                                  )
    )
  # Extracting datasets timezone.    
  timezone <- attr(data[[1]][["datetime"]], "tzone")
  # Loading data dictionary 
  data_dict_df <- .load_data_dictionary(path = data_dict_path, timezone = timezone)
  
  if ( !is.null(aqi_data) ) {
    # Add aqi column to data_prepped dfs.
    data <- purrr::map2(.x = data, .y = aqi_data, 
                                .f = function(x, y) left_join(x = x, y = rename(y, aqi = 2), by = "datetime") %>% 
                                  rename("US AQI" = aqi))
  } else {
    # Removing aqi column from data dictionary.
    data_dict_df <- data_dict_df %>% 
      filter(!`Column Heading` == "aqi")
  }  
  
  # Creates a joined time series of PM data for each sensor at site (column names are sensor labels).
  pm_joined <- pm_data %>% 
    purrr::reduce(full_join, by = "datetime") %>% 
    `colnames<-`(c("datetime", names(sensor_list))) %>% 
    dplyr::mutate_if(is.numeric, round, digits = 2)
  # Writing to workbook
  openxlsx::addWorksheet(wb, sheetName = "PM Side-by-Side")
  openxlsx::writeData(wb = wb, sheet = "PM Side-by-Side", x = pm_joined)
  
  for (i in 1:length(sensors)) {
    sensor <- sensors[[i]]
    
    df <- data[[sensor]] %>% 
      dplyr::mutate(
        date = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
        time = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
      ) %>% 
      # Removing total datetime for date and time variables. 
      dplyr::select(-datetime) %>%
      dplyr::select(date, time, "Day of Week" = day_of_week,
                    "humidity %" =  humidity, "temperature (C)" = temperature,
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
.load_data_dictionary <- function(
  path = "inputs/data-viz-package-wkbk-dictionary.xlsx",
  timezone
) {
  # Helper function loads data dictionary for addition to workbook.
  data_dict_sheet <- readxl::read_excel(path = path)
  
  time_row <- which(data_dict_sheet[, "Column Heading"] == "time")
  
  data_dict_sheet[["Description"]][[time_row]] <- paste(data_dict_sheet[["Description"]][[time_row]],
                                                        timezone, sep = " ")
  
  return(data_dict_sheet)
}

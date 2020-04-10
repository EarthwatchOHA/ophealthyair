#' @title
#'
#' \code{make_PA_wkbk} returns a filled Workbook object from the openxlsx
#' package.
#'
#' @description Create a Workbook object filled with Particulate Matter data
#'   from aggstats_list, a pre-generated Data Dictionary Sheet.
#'
#' @param aggstats_list
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
  aggstats_list,
  aqi_data,
  sensor_catalog,
  data_dict_path = "inputs/data-viz-package-wkbk-dictionary.xlsx",
  use_aqi = FALSE) {
  # TODO: Add DocString
  
  # Creating excel workbook obect.
  wb <- openxlsx::createWorkbook(creator = "Earthwatch Institute US")
  
  sensors <- names(aggstats_list)
  
  # Vulnerable to changes in Sensor_catalog.
  meta_df <- sensor_catalog %>%
    filter(label %in% sensors) %>% 
    dplyr::select(-c("MAC ID", "id", "Person of Contact", "Purchasing Email",
                     "Program", "Picture of Sensor", "Collocated", "Sensor Type", "Contact email"))
  
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb, sheet = "Sensor Info", x = meta_df)
  
  data_prepped <- aggstats_list %>%
    purrr::map(.f = function(x) x %>%
                                dplyr::mutate(temperature = (temperature_mean - 32) * 5/9) %>% # Fahrenheit to Celsius
                                dplyr::select("datetime", "temperature",
                                              "humidity" = "humidity_mean", "pm25"))
  # Extracting datasets timezone.    
  timezone <- attr(data_prepped[[1]][["datetime"]], "tzone")
  # Loading data dictionary 
  data_dict_df <- .load_data_dictionary(path = data_dict_path, timezone = timezone)
  
  if (use_aqi) {
    # Add aqi column to data_prepped dfs.
    data_prepped <- purrr::map2(.x = data_prepped, .y = aqi_data, 
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
    dplyr::select(datetime, contains("pm")) %>% 
    `colnames<-`(c("datetime", names(aggstats_list))) %>% 
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
      dplyr::select(date, time, "humidity %" =  humidity, "temperature (C)" = temperature,
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

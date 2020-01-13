fetch_SensorCatalog <- function(path = "C:/Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/SensorCatalog.xlsx") {
  # Updates Sensor Catalog .rds object from OneDrive file.
  
  sensor_catalog <- readxl::read_excel(path)

  saveRDS(sensor_catalog, "data/sensor_catalog.rds")
}

load_SensorCatalog <- function() {
  # Loads Sensor Catalog Object as DataFrame.
  sensor_catalog <- readRDS("data/sensor_catalog.rds")
}

get_pat <- function(label, id, pas, startdate = NULL, enddate = NULL, timezone = NULL) {
  # Creates a new pat object, but intended to be used iteratively, with error and warning handlers.
  # TODO: Suppress simple warnings that occur when there is missing data in the time period.
  tryCatch( {
    
    pat <- AirSensor::pat_createNew(pas = pas, id = id, label = label,
                                    startdate = startdate, enddate = enddate,
                                    timezone = timezone)
    
    print(paste(label, ": ingestion successful."))
    return(pat)
  },
  exceptionDo = function(e) {
    message(paste(label, ":", e))
    return(paste(e))
  },
  warningDo = function(w) {
    # Note that there is no notice if a warning occurs.
    pat <- AirSensor::pat_createNew(pas = pas, id = id, label = label,
                                    startdate = startdate, enddate = enddate,
                                    timezone = timezone)
    return(pat)
  })
}


fetch_pat_list <- function(pas, sensor_labels, sensor_ids = NULL,
                           startdate = NULL, enddate = NULL, timezone = NULL) {
  # Creates list of pat objects from a pas object and vector of sensor labels and sensor ID's.
  # TODO: Add logging (package loggr).
  # Use purrr::map or lapply to speed up iteration.
  pat_list <- list()
  for (i in 1:nrow(sensor_labels)){
    label <- sensor_labels[[1]][[i]]
    id <- sensor_ids[[1]][[i]]
    pat_list[[label]] <- get_pat(label = label, id = id, pas = pas, startdate = start, enddate = end)
  }
  
  filename <- paste("pat_list", "-", lubridate::today(), ".rds", sep = "")
  path <- paste("data", filename, sep = "/")
  saveRDS(pat_list, "data/pat_list")
  return(pat_list)
}

aggregate_pat_list <- function(pat_list) {
  # Accepts a list of Purple Air Time Series objects, applies pat_aggregate to each element that is a pat.
  agg_list <- list()
  for (i in 1:length(pat_list)) {
    label <- sensor_labels[i]
    pat <- pat_list[[label]]
    if (AirSensor::pat_isPat(pat)) {
      agg_list[[label]] <- AirSensor::pat_aggregate(pat = pat)
      print(paste(label, ": aggregation successful."))
    } else {
      agg_list[[label]] <- "Purple Air Time Series does not exist for this sensor."
      print(paste(label, ": Purple Air Time Series does not exist."))
    }
  }
}


make_datadelivery_wkbk <- function(site, start = NULL, end = NULL, sensor_catalog, pas) {
  # Outputs an excel workbook. 
  # TODO: Error control system.
  sensors <- filter(sensor_catalog, `Site` == site) %>% select(`Sensor Label`, `Sensor ID`)
  sensor_labels <- sensors[["Sensor Label"]]
  sensor_ids <- sensors[["Sensor ID"]]
  pat_list <- fetch_pat_list(pas = pas, sensor_labels = sensor_labels, sensor_ids = sensor_ids,
                 startdate = start, enddate = end)
  filename <- paste("outputs/datasets/data-deliveries/", site, "-", start, "-", end, ".xlsx", sep = "")
  wb <- openxlsx::createWorkbook(creator = "Earthwatch")
  meta_list <- list()
  for (i in 1:nrow(sensors)) {
    sensor <- sensors[[1]][[i]]
    if (pat_isPat(pat_list[[sensor]])) {
      meta_list[[i]] <- pat_list[[sensor]] %>% pat_extractMeta()
      agg <- pat_list[[sensor]] %>% AirSensor::pat_aggregate() %>%
        dplyr::mutate(pm25 = (pm25_A_mean + pm25_B_mean) / 2, 
                      AQI = con2aqi::con2aqi(pollutant = "pm25", con = pm25)) %>% 
        dplyr::select(datetime, "Particulate Matter 2.5 (1hr Average)" = pm25, "Air Quality Index" = AQI,
               "Temperature (Celsius)" = temperature_mean, "Relative Humidity (%)" = humidity_mean)
      openxlsx::addWorksheet(wb, sheetName = sensor)
      openxlsx::writeData(wb = wb, sheet = sensor, x = agg)
    }
  }
  meta_df <- bind_rows(meta_list) %>% 
    select(label) %>%
    left_join(sensor_catalog, by = c("label" = "Sensor Label")) %>% 
    select(-c("MAC ID", "Sensor ID", "...13", "Person of Contact", "Purchasing Email",
              "Program", "Picture of Sensor"))
  openxlsx::addWorksheet(wb, sheetName = "Sensor Info")
  openxlsx::writeData(wb = wb, sheet = "Sensor Info", x = meta_df)
  openxlsx::saveWorkbook(wb, file = filename)
}



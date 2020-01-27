fetch_SensorCatalog <- function(path = "C:/Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/SensorCatalog.xlsx") {
  # Updates Sensor Catalog .rds object from OneDrive file.
  # TODO: Standardize column names on import.
  # TODO: Parse dates.
  sensor_catalog <- readxl::read_excel(path, sheet = "Sensor Catalog") %>% 
    dplyr::rename(id = "Sensor ID", label = "Sensor Label", site = "Deploy Site")
  
  saveRDS(sensor_catalog, "data/sensor_catalog.rds")
  return(sensor_catalog)
}

load_SensorCatalog <- function() {
  # Loads Sensor Catalog Object as DataFrame.
  sensor_catalog <- readRDS("data/sensor_catalog.rds")
}

read_pas <- function() {
  # This function reads in the saved pas RDS object.
  return(readRDS("data/pas.rds"))
}

get_pat <- function(label, id, pas, startdate = NULL, enddate = NULL, timezone = NULL) {
  # Creates a new pat object, but intended to be used iteratively, with error and warning handlers.
  # TODO: Add loggr:: for error detection.
  tryCatch( 
    expr = {
      pat <- AirSensor::pat_createNew(pas = pas, id = id, label = label,
                                      startdate = startdate, enddate = enddate,
                                      timezone = timezone)
      print(paste(label, ": ingestion successful."))
      return(pat)
    },
    error = function(e) {
      message(paste(label, ":", e))
      return(paste(e))
    })
}

fetch_pat_list <- function(pas, sensor_labels, sensor_ids,
                           startdate = NULL, enddate = NULL, timezone = NULL) {
  
  # Creates list of pat objects from a pas object and vector of sensor labels and sensor ID's.
  # TODO: Add logging (package loggr).
  # TODO: Best filing and loading system (data persistence). 
  # TODO: Fix growing list. 
  
  if (!purrr::is_bare_vector(sensor_labels)) {
    sensor_labels <- unlist(sensor_labels)
  }
  if (!purrr::is_bare_vector(sensor_ids)) {
    sensor_ids <- unlist(sensor_ids)
  }
  
  suppressWarnings(expr = {
  pat_list <- purrr::map2(.x = sensor_labels, .y = sensor_ids, .f = get_pat, pas = pas,
                          startdate = startdate, enddate = enddate, timezone = timezone) %>% 
    rlang::set_names(sensor_labels)
  
  saveRDS(pat_list, "data/pat_list.rds")
  return(pat_list)
  })
}

load_pat_list <- function() {
  pat_list <- readRDS("data/pat_list.rds")
  return(pat_list)
}

aggregate_pat_list <- function(pat_list) {
  # Accepts a list of Purple Air Time Series objects, applies pat_aggregate to each element that is a pat.
  # TODO: Fix growing list.
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

get_site_pats <- function(site) {

  # This function filters utilizes the sensor_catalog Deploy Site column to filter the pat_list depending on a given site.
  # TODO: Add error control system.
  sensor_catalog <- load_SensorCatalog()
  
  if(!any(site %in% unique(sensor_catalog$site))) {
    stop("Site is not present in Sensor Catalog.")
  }
  site_sensors <-  sensor_catalog %>% filter(site == site)
  site_pat_list <- fetch_pat_list(sensor_labels = site_sensors$label, sensor_ids = site_sensors$id)
  return(site_pat_list)
}


  


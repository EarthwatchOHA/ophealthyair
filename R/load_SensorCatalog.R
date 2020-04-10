#' @title
#'
#' \code{load_SensorCatalog} returns sensor_catalog dataframe, last saved by
#' fetch_SensorCatalog from .RDS file.
#'
#' @description Internal function used for data persistence to load formatted
#'   sensor_catalog dataframe as most recently saved by fetch_SensorCatalog.
#'
#' @param path The path to the sensor_catalog .RDS file to read.

load_SensorCatalog <- function(path = "data/sensor_catalog.rds") {
  # Loads Sensor Catalog Object as DataFrame.
  sensor_catalog <- readRDS(path)
  return(sensor_catalog)
}
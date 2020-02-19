
load_SensorCatalog <- function(path = "data/sensor_catalog.rds") {
  # Loads Sensor Catalog Object as DataFrame.
  sensor_catalog <- readRDS(path)
  return(sensor_catalog)
}
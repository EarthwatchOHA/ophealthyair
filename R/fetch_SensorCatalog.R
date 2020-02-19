fetch_SensorCatalog <- function(
  catalog_path = "C:/Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/SensorCatalog.xlsx",
  output_path = "data/sensor_catalog.rds"
) {
  # Updates Sensor Catalog .rds object from OneDrive file.
  # TODO: Standardize column names on import.
  # TODO: Parse dates.
  sensor_catalog <- readxl::read_excel(catalog_path, sheet = "Sensor Catalog" ) %>% 
    dplyr::rename(id = "Sensor ID", label = "Sensor Label", site = "Deploy Site") %>% 
    mutate(`Deploy Date` = format(`Deploy Date`, format = "%Y-%m-%d"),
           `Deploy Time` = format(`Deploy Time`, format = "%H:%M"))
  
  saveRDS(sensor_catalog, output_path)
  return(sensor_catalog)
}

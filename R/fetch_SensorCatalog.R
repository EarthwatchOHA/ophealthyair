#' @title
#'
#' \code{fetch_SensorCatalog} returns a formatted dataframe from the Earthwatch
#' Sensor Catalog excel file.
#'
#' @description Internal function for the import, formatting, and persistence of
#'   Earthwatch's Sensor Catalog, containing sensor deployment metadata.
#'
#' @param catalog_path The location of the Sensor Catalog to read. Default is
#'   the absolute path of the file on Ian Ozeroff's EW issue laptop.
#' @param output_path The output filename of the .RDS copy of the sensor catalog
#'   dataframe.

fetch_SensorCatalog <- function(
  catalog_path = "C:/Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/SensorCatalog.xlsx",
  output_path = "data/sensor_catalog.rds"
) {
  # Updates Sensor Catalog .rds object from OneDrive file.
  sensor_catalog <- readxl::read_excel(catalog_path, sheet = "Sensor Catalog" ) %>% 
    dplyr::rename(id = "Sensor ID", label = "Sensor Label", site = "Deploy Site") %>% 
    mutate(`Deploy Date` = format(`Deploy Date`, format = "%Y-%m-%d"),
           `Deploy Time` = format(`Deploy Time`, format = "%H:%M"))
  
  saveRDS(sensor_catalog, output_path)
  return(sensor_catalog)
}

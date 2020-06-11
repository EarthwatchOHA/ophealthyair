#' @importFrom magrittr %>%
#'
#' @title Fetch catalog of Purple Air Sensors from semi-structured excel
#'   workbook.
#'
#'   \code{fetch_SensorCatalog} returns a formatted dataframe from the
#'   Earthwatch Sensor Catalog excel file.
#'
#' @description Internal function for the import and formatting, of
#'   Earthwatch Sensor Catalog, containing sensor deployment metadata.
#'
#' @param catalog_path The location of the Sensor Catalog to read.
#' @param output_path The output filename of the .RDS copy of the sensor catalog
#'   dataframe. Default is project data folder as "sensor_catalog.rds".
#'
#' @note fetch_SensorCatalog makes the following assumptions about the Sensor
#'   Catalog:
#'   \enumerate{
#'     \item catalog_path is a .xls or .xlsx file.
#'     \item Workbook contains a sheet named "Sensor Catalog"
#'     \item Workbook contains columns "Sensor ID", "Sensor Label",
#'     "Deploy Site", "Collocated", "Deploy Date", and "Deploy Time".
#'     \item "Deploy Date" column contains only date values formatted
#'     such as '2018-01-31', or blank values.
#'     \item "Deploy Time" column contains only time stamps formatted
#'     as '12:59' in the local time of the sensor, or blank values.
#'   }

fetch_SensorCatalog <- function(
  catalog_path,
  output_path = "data/sensor_catalog.rds"
) {
  # Updates Sensor Catalog .rds object from OneDrive file.
  sensor_catalog <- readxl::read_excel(catalog_path, sheet = "Sensor Catalog") %>%
    dplyr::rename(id = "Sensor ID", label = "Sensor Label", site = "Deploy Site") %>%
    mutate(
      Collocated  = if_else(Collocated == 1, TRUE, FALSE,
                            missing = FALSE),
      deploy_date = format(`Deploy Date`, format = "%Y-%m-%d"),
      deploy_time = format(`Deploy Time`, format = "%H:%M")
      )

  invisible(sensor_catalog)
}

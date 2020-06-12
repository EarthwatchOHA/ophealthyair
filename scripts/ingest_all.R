suppressMessages({
  library(MazamaCoreUtils)
  library(MazamaSpatialUtils)
  library(AirSensor)
  library(dplyr)
  devtools::load_all()
})
#------------------------------------------------------------------------------
# Command Line Interface

# Create Argument Parser
if ( !exists("args", mode = "list") ) {

  suppressPackageStartupMessages(library("argparse"))
  # Instantiating parser.
  parser <- ArgumentParser(
    description = paste("Ingest all sensors specified in Sensor Catalog,",
                        "specified by catalog_path.", sep = " ")m
    add_help = TRUE
    )

  # Arguments
  parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-p", "--catalog_path", type = "character",
                      required = TRUE,
                      help = paste(
                        "Absolute path of the sensor catalog to",
                        "use. Must lead to a .xls or .xlsx file.",
                        "Note: Windows Operating Systems use backslash,",
                        "instead of forward slash in file paths.",
                        "THIS WILL NOT WORK. If running on windows,",
                        "you must replace all backslashes with forward",
                        "slashes. Argument type: %(type)s.", sep = " "))

  parser$add_argument("-l", "--lookback_days", type = "double",
                      default = 30,
                      help = paste(
                        "Number of days to retroactively look",
                        "for sensors uploading to purple air.",
                        "Argument type: %(type)s [default %(default)s]",
                        sep = " "))

  parser$add_argument("-d", "--default_startdate", type = "double",
                      default = 20180101,
                      help = paste(
                        "8 digit integer representing the",
                        "default start date to use if",
                        "'Deploy Date' not specified in",
                        "Sensor Catalog. Format example is",
                        "20160131.Argument type: %(type)s.",
                        "[default %(default)s]", sep = " "
                        )
                      )

  args <- parser$parse_args()
}

#------------------------------------------------------------------------------
# Input Control:

pattern <- paste(".*.xls$", ".*.xlsx$", sep = "|")
if ( !stringr::str_detect(args$catalog_path, pattern = pattern) ) {
  stop(paste("supplied catalog_path does not point to a valid excel file.",
             "Confirm that catalog_path ends in .xls or .xlsx.",
             sep = " "))
}

if ( args$lookback_days <= 0 | !is.integer(args$lookback_days) ) {
  stop("lookback_days must be a positive integer.")
}

if ( !is.integer(args$default_startdate) |
     !(nchar(as.character(args$default_startdate)) == 8) |
     args$default_startdate <= 0 ) {

  stop("default_startdate must be an eight digit positive integer")

}
#------------------------------------------------------------------------------
# Initialize Mazama Spatial Data
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
setSpatialDataDir("data/spatial")
initializeMazamaSpatialUtils()
loadSpatialData("NaturalEarthAdm1")

countries_coded <- c("United States", "India", "Sri Lanka") %>%
  countrycode::countrycode(origin = "country.name", destination = "iso2c")

print("Loading Sensor Catalog")

sensor_catalog <- fetch_SensorCatalog(args$catalog_path)

saveRDS(sensor_catalog, "data/sensor_catalog.rds")

print("Sensor Catalog Loaded Successfully")

sensor_catalog <- sensor_catalog %>%
  mutate(
    site = stringr::str_to_lower(site)
  ) %>%
  filter(site != "undeployed")

print(paste("Loading List of All Purple Air Sensors Active within",
      args$lookback_days, "days.", sep = " "))

pas <- AirSensor::pas_createNew(countryCodes = countries_coded,
                                lookbackDays = args$lookback_days)

saveRDS(pas, "data/pas.RDS")

print("Active Sensor List Loaded  Successfully.")

# Setting up startdate and enddate variables.
startdates <- sensor_catalog$`Deploy Date` %>%
  stringr::str_replace_all(pattern = "-", replacement = "") %>%
  as.integer()

startdates[is.na(startdates)] <- args$default_startdate

enddate <- as.numeric(stringr::str_replace_all(lubridate::today(), "-", ""))
enddates <- rep.int(x = enddate, times = length(startdates))

pat_list <- fetch_pat_list(
  sensor_labels = sensor_catalog$label,
  pas = pas,
  startdate = startdates,
  enddate = enddates)

saveRDS(pat_list, "data/pat_list.rds")

library(MazamaCoreUtils)
library(MazamaSpatialUtils)
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
devtools::load_all("C://Users/iozeroff/Data-Science/ophealthyair")
library(dplyr)


#TODO: Create way to pass in start and end date.(Functionalize?)
# This script creates a new pas for all of the countries we operate in.
# ? Command Line ops?

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
setSpatialDataDir("data/spatial")
loadSpatialData("NaturalEarthAdm1")

countries_coded <- c("United States", "India", "Sri Lanka") %>%
  countrycode::countrycode(origin = "country.name", destination = "iso2c")

sensor_catalog <- fetch_SensorCatalog() %>% filter(site != "Undeployed")

pas <- fetch_pas(countryCodes = countries_coded)

fetch_pat_list(sensor_labels = sensor_catalog$label, sensor_ids = sensor_catalog$id,
               pas = pas, output_path = "data/pat_list_wk.rds")
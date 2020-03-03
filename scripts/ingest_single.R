# This script ingests only the Purple Air Timeseries for a given sensor.
# This scripts ingest only the Purple Air Time Series for the given site. 
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

# TODO: Add way to specify program.
sensor_catalog <- fetch_SensorCatalog() %>% filter(site != "Undeployed" & site == partner_site)

pas <- fetch_pas(countryCodes = countries_coded)

get_pat(label = sensor_catalog$label, id = sensor_catalog$id, pas = pas, startdate = startdate, enddate = enddate)
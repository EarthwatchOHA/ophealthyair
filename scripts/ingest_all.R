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
sensor_catalog <- fetch_SensorCatalog() %>% filter(site != "Undeployed")

pas <- fetch_pas(countryCodes = countries_coded, lookbackDays = 21)

# Setting up startdate and enddate variables.
default_startdate <- 20180101
startdates <- sensor_catalog$`Deploy Date` %>% stringr::str_replace_all(pattern = "-", replacement = "") %>% as.integer()
startdates[is.na(startdates)] <- default_startdate
# TODO: Could maybe do the enddates inside of fetch_pat_list.
enddate <- as.numeric(stringr::str_replace_all(lubridate::today(), "-", ""))
enddates <- rep.int(x = enddate, times = length(startdates))

fetch_pat_list(sensor_labels = sensor_catalog$label, sensor_ids = sensor_catalog$id, pas = pas,
               startdate = startdates, enddate = enddates)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(AirSensor)
devtools::load_all("C://Users/iozeroff/Data-Science/ophealthyair")
library(dplyr)

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
setSpatialDataDir("data/spatial")
loadSpatialData("NaturalEarthAdm1")



countries_coded <- c("United States", "India", "Sri Lanka") %>%
  countrycode::countrycode(origin = "country.name", destination = "iso2c")

sensor_catalog <- fetch_SensorCatalog() %>% filter(site != "Undeployed")

pas <- fetch_pas(countryCodes = countries_coded, lookbackDays = 7)

fetch_pat_list(sensor_labels = sensor_catalog$label, sensor_ids = sensor_catalog$id,
               pas = pas, output_path = "data/pat_list_wk.rds")
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(AirSensor)
library(dplyr)

#TODO: Create way to pass in start and end date.(Functionalize?)
# ? Command Line ops?

source("R/ingestion/util.R")

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

sensor_catalog <- fetch_SensorCatalog()

pas <- pas_load(retries = 7, archival = TRUE)

fetch_pat_list(sensor_labels = sensor_catalog$label, sensor_ids = sensor_catalog$id, pas = pas)

# Need data persistence so I can load data in other scripts.
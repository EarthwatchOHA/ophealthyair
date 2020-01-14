library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(AirSensor)
library(dplyr)

#TODO: Create way to pass in start and end date.(Functionalize?)
# ? Command Line ops?

source("R/util.R")

start <- 20191001
end <- 20200110
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

fetch_SensorCatalog()
sensor_catalog <- load_SensorCatalog()

pas <- pas_load(retries = 7, archival = TRUE) 

pat_list <- fetch_pat_list(pas = pas, sensor_labels = sensor_catalog[['Sensor Label']], 
               sensor_ids = sensor_catalog[['Sensor ID']], startdate = start, enddate = end)


test <- pat_createNew(pas = pas, label = sensor_catalog[["Sensor Label"]][[3]], 
                      id = sensor_catalog[["Sensor ID"]][[3]], startdate = start, enddate = end)

?pat_createNew

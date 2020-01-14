# This script creates a data delivery workbook for a selected site.
library(dplyr)
library(AirSensor)
library(PWFSLSmoke)


source("R/util.R")
source("R/revised_con2aqi.R")


site = "Waldorf School, Belmont MA"
start = 20200101
end = 20200110

sensor_catalog <- fetch_SensorCatalog()
sensor_labels <- sensor_catalog["Sensor Label"]
sensor_ids <- sensor_catalog["Sensor ID"]

pat_list <- fetch_pat_list(pas = pas, sensor_labels = sensor_labels, sensor_ids = sensor_ids, 
                           start = start, end = end, timezone = "America/New_York")

make_site_wkbk(site = site, pat_list = pat_list, sensor_catalog = sensor_catalog)

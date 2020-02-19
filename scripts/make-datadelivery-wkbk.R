# This script creates a data delivery workbook for a selected site.
library(dplyr)
library(ggplot2)
source("R/qaqc-util.R")
source("R/viz-util.R")
source("R/delivery-util.R")
source("R/con2aqi_single.R")
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
devtools::load_all("C://Users/iozeroff/Data-Science/ophealthyair")

# Named partner_site and not sire because can't use filter(site == site)
# Need to do for every site.
# Make two scripts. One for all sites, and one for a specific site.
partner_site = "Waldorf School, Belmont MA"

# Getting all sensor data.
# TODO: Need to get ALL sensor data (from all time).
source("scripts/ingest_all.R")

site_sensors <- load_SensorCatalog() %>% 
  filter(site == partner_site)

pat_list <- load_pat_list()[site_sensors$label]

source("scripts/agg-qaqc.R")  
# Makes data-vis-pkg dir, as well as identical compressed dir, and returns filepath to uncompressed dir.
# TODO: Sensor data is same. 
# TODO: Visualization ribbons are not lined up with points.

uncompressed_dir <- 
  make_site_dataviz_pkg(site = partner_site, agg_list = agg_list,
                        sensor_catalog = sensor_catalog, wkbk_aqi_col = TRUE)
# Deletes uncompressed dir.
unlink(uncompressed_dir, recursive = TRUE)

# This script creates a data delivery workbook for a selected site.
library(dplyr)
library(ggplot2)
library(lubridate)
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
devtools::load_all()

programs <- c("India" = "IN", "Sri Lanka" = "IN", "Southern California" = "US", "Boston" = "US")

# Named partner_site and not sire because can't use filter(site == site)
# Need to do for every site.
# Make two scripts. One for all sites, and one for a specific site.
site <- "Waldorf School, Belmont MA"

# Loads sensor_catalog
sensor_catalog <- load_SensorCatalog()

site_sensors <- sensor_catalog %>% 
  filter(site == !!site)

pat_list <- load_pat_list()[site_sensors$label]

# QAQC data. See vignettes/qaqc_pt1.rmd for details.
source("scripts/qaqc.R")  

# Get program from site.
program <- site_sensors$Program[1]
# Get country code for visualization AQI parameters.
aqi_country <- programs[program]
# Use aqi in make_site_dataviz_pkg adds a US AQI column to workbook. Only use if program is in US.
if (aqi_country == "IN") {
  include_aqi_data <- FALSE
} else if( aqi_country == "US") {
  include_aqi_data <- TRUE
}

# Attempting to converting data timezone to local time. 
# Timezone set to NULL.
timezone <- NULL
# If possible extract timezone from pat_list meta.
for (i in 1:length(pat_list)) {
  try({
   timezone <- pat_extract_TZ(pat_list[[i]]) 
  })
}

# Make data-vis-pkg dir, as well as identical compressed dir, and returns filepath to uncompressed dir.
uncompressed_dir <- 
  make_site_dataviz_pkg(site = site,
                        aggstats_list = aggstats_list, 
                        sensor_list = sensor_list,
                        include_aqi_data = include_aqi_data,
                        aqi_country = aqi_country,
                        timezone = timezone,
                        facet_covid_workweek = TRUE)

# Delete uncompressed dir.
unlink(uncompressed_dir, recursive = TRUE)

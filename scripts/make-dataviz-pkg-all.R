# This script creates a data delivery workbook for a selected site.
library(dplyr)
library(ggplot2)
library(lubridate)
devtools::load_all()
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")

output_path <- "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/Data-Visualization-Packages/"

programs <- c("India" = "IN", "Sri Lanka" = "IN", "Southern California" = "US", "Boston" = "US")

# Getting all sensor data.
pat_list <- load_pat_list()

# Named partner_site and not site because can't use filter(site == site)
sensor_catalog <- load_SensorCatalog()
sites <- unique(sensor_catalog$site)
# Removing undeployed from sites, as these sensors are not in use.
sites <- sites[sites != "Undeployed"]

# TODO: Find way to solve this duplicate label issue.
# For now simply filtering out US Embassy Delhi site.
sites <- sites[sites != "US Embassy Delhi"]
# Removing NA values, as they can sneak in here.
sites <- sites[!is.na(sites)]

for (i in 1:length(sites)) {
  
  partner_site <- sites[i]
  print(partner_site)
  site_sensors <- sensor_catalog %>% 
    filter(site == partner_site)
  
  # Getting program of site_sensors.
  program <- site_sensors$Program[1]
  # Get country code for visualization AQI parameters.
  aqi_country <- programs[program]
  
  # Use aqi in make_site_dataviz_pkg adds a US AQI column to workbook. Only use if program is in US.
  if (aqi_country == "IN") {
    use_aqi <- FALSE
  } else if( aqi_country == "US") {
    use_aqi <- TRUE
  }
  
  # Filtering pat_list for those sensors.
  pat_list <- load_pat_list()[site_sensors$label]
    
  # QAQC data. See vignettes/qaqc_pt1.rmd for details.
  source("scripts/qaqc.R")  
  
  # Attempting to converting data timezone to local time. 
  # Timezone set to NULL.
  timezone <- NULL
  # If possible extract timezone from pat_list meta.
  for (i in 1:length(pat_list)) {
    try({
      timezone <- pat_extract_TZ(pat_list[[i]]) 
    })
  }
  # Makes data-vis-pkg dir, as well as identical compressed dir, and returns filepath to uncompressed dir.
  try({uncompressed_dir <- 
    make_site_dataviz_pkg(site = partner_site,
                          aggstats_list = aggstats_list, 
                          sensor_list = sensor_list,
                          sensor_catalog = sensor_catalog,
                          use_aqi = use_aqi,
                          aqi_country = aqi_country,
                          output_directory = output_path,
                          timezone = timezone,
                          facet_covid_workweek = TRUE)
  # Deletes uncompressed dir.
  unlink(uncompressed_dir, recursive = TRUE)
  })
}


get_site_pats <- function(site, sensor_catalog, pat_list) {
  # This function filters utilizes the sensor_catalog Deploy Site column to filter the pat_list depending on a given site.
  # TODO: Strengthen error control system.
  
  if(!any(site %in% unique(sensor_catalog$site))) {
    stop("Site is not present in Sensor Catalog.")
  }
  site_sensors <-  sensor_catalog[sensor_catalog$site == site & !is.na(sensor_catalog$site), ]
  site_pats <- pat_list[site_sensors$label]
  # Filtering out not pat objects.
  site_pats <- site_pats[purrr::map_lgl(site_pats, AirSensor::pat_isPat)]
  
  return(site_pats)
}
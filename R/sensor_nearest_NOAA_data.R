#' @title 
#' 
#' @description 
#' 
#' @param sensor
#' @param hourly
#' @param precipidation
#' @param parallel
#' @param ArchiveBaseUrl
#' 
#' @return 
#' 
#' @examples 
#' 

sensor_nearest_NOAA_data <- function(
  sensor,
  hourly = TRUE,
  precipitation = TRUE,
  parallel = FALSE,
  ArchiveBaseUrl = "http://smoke.mazamascience.com/data/PurpleAir"
) {
  
  setArchiveBaseUrl(ArchiveBaseUrl)
  
  if(!sensor_isSensor(sensor)) {
    stop("sensor must be a sensor object, containing 'meta' and 'data' elements.")  
  }
  if(sensor_isEmpty(sensor)) {
    stop("sensor Meta must not be empty.")
  }
  
  # Extracting Sensor Meta
  sensor_meta <- sensor %>% 
    sensor_extractMeta()
  
  # Getting nearest NOAA sensor. 
  siteMeta <- worldmet::getMeta(lat = sensor_meta$latitude,
                                 lon = sensor_meta$longitude, n = 1)
  
  # Always specify a timezone wherever possible!
  timezone <- sensor_meta$timezone
  
  # Define the timeframe.
  start <- min(sensor$data$datetime)
  end <- max(sensor$data$datetime)
  
  if (lubridate::year(start) != lubridate::year(end)) {
    years <- seq.int(from = lubridate::year(start), to = lubridate::year(end), by = 1)
  } else {
    years <- lubridate::year(start)
  }
  
  # Load NOAA site data for the timeframe.
  siteData <- worldmet::importNOAA(code = noaa_meta$code, 
                                   hourly = hourly,
                                   precip = precipitation,
                                   PWC = FALSE,
                                   year = years,
                                   parallel = parallel) %>% 
    dplyr::filter(date >= start, date < end)
  
  
  # Creating sensor-like object.
  noaa_sensor <- list(
    "meta" = siteMeta,
    "data" = siteData
  )
  
  return(noaa_sensor)
}

#' @title 
#' 
#' @description 
#' 
#' @param sensor
#' @param aqi_country
#' @param include_map
#' 
#' 
#' @return A ggplot object.
#' 
#' @example 
#'
#'
#'

sensor_pollutionRose_dash <- function(
  sensor,
  aqi_country,
  include_map = TRUE,
  max_noaa_dist = 3
) {
  # Getting aqi data
  aqi_info <- load_aqi_info(country = aqi_country)
  # Extracting Sensor Label
  sensor_label <- sensor[["meta"]][["label"]]
  
  # Getting NOAA Data for nearest sensor.
  noaa_meta <- worldmet::getMeta(lat = sensor$meta$latitude, 
                                 lon = sensor$meta$longitude,
                                 plot = FALSE,
                                 n = 3)
  
 if ( !any(noaa_meta$dist <= max_noaa_dist) ) {
   
   smallest_dist <- round(min(noaa_meta$dist), 2)
   out <- paste("No detectable NOAA sensors within", max_noaa_dist, "km.",
                "Nearest detected NOAA sensor is", smallest_dist, "km.")
   stop(out)
 }
#------------------------------------------------------------------------------
  # Getting NOAA Data
  # Define the timeframe.
  start <- min(sensor$data$datetime)
  end <- max(sensor$data$datetime)
  
  if ( lubridate::year(start) != lubridate::year(end) ) {
    years <- seq.int(from = lubridate::year(start),
                     to = lubridate::year(end),
                     by = 1)
  } else {
    years <- lubridate::year(start)
  }
  
  # Load NOAA site data for the timeframe.
  noaa_data <- worldmet::importNOAA(code = noaa_meta[["code"]][[1]], 
                                   hourly = TRUE,
                                   precip = FALSE,
                                   PWC = FALSE,
                                   year = years) %>% 
    dplyr::filter(date >= start, date < end)
  
  windData <- noaa_data %>%
    dplyr::select(date, wd, ws)
  
  # Confirming NOAA data loaded successfully.
  if( sum(is.na(windData$wd)) == nrow(windData) ) {
    stop("No wind direction data loaded.")
  } else if( sum(is.na(windData$ws)) == nrow(windData) ) {
    stop("No wind speed data loaded.")
  }

#------------------------------------------------------------------------------
# Creating Map
if (include_map) {
  map_points <- dplyr::bind_rows(
    dplyr::select(sensor$meta, longitude, latitude),
    dplyr::select(noaa_meta, latitude, longitude),
    .id = "type") %>%
    dplyr::mutate(
      type = factor(type, labels = c("Purple Air", "NOAA"))
    )
  
  map <- ggmap::qmplot(x = longitude, y = latitude,
                       data = map_points, color = type,
                       shape = type,
                       size = 2,
                       margins = TRUE) +
    ggplot2::guides(size = FALSE)
}
# Generating Rose Plots
#------------------------------------------------------------------------------
  openair_rose_classic <- sensor_pollutionRose(sensor,
                                               windData,
                                               statistic = "prop.mean",
                                               normalize = FALSE, 
                                               cols = aqi_info$colors,
                                               breaks = aqi_info$breaks_24,
                                               type = "default")
  
  openair_rose_normal <- sensor_pollutionRose(sensor,
                                              windData,
                                              statistic = "prop.mean",
                                              normalize = TRUE, 
                                              cols = aqi_info$colors,
                                              breaks = aqi_info$breaks_24,
                                              type = "default",
                                              annotate = paste("NOAA Monitor to Sensor",
                                                               round(noaa_meta$dist, 2), "km."))

  
  # Retrieving plots from 'openair' objects.
  rose_normal <- openair_rose_normal$plot
  rose_classic <- openair_rose_classic$plot
#------------------------------------------------------------------------------
  # Arranging grobs into single image.  
  if(include_map) {
    
    layout_matrix <- rbind(c(1, 2),
                           c(3, 3))
    
    out <- gridExtra::arrangeGrob(
      rose_classic, 
      rose_normal,
      map,
      ncol = 2,
      nrow = 2,
      layout_matrix = layout_matrix,
      top = sensor_label)
    
  } else {
    layout_matrix <- rbind(c(1, 2))
    
    out <- gridExtra::arrangeGrob(rose_classic,
                                  rose_normal,
                                  nrow = 1,
                                  top = sensor_label)
  }
  
  out <- ggpubr::as_ggplot(out)
  
#------------------------------------------------------------------------------
  invisible(out)
}

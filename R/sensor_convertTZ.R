#' @title 
#' 
#' @description 
#' 
#' @param sensor
#' @param to
#' 
#' @return
#' 
#' @example
#'
#'

sensor_convertTZ <- function(sensor, to) {
  
  if ( !sensor_isSensor(sensor)) {
    stop("Sensor must be of class 'airsensor' and 'ws_monitor'.")
  }
  
  if ( !(to %in% OlsonNames()) ) {
    stop("to must be a valid timezone. Check OlsonNames() for options.")
  }
  
  data <- sensor %>% 
    sensor_extractData() %>% 
    mutate(
         datetime = lubridate::with_tz(time = .data$datetime,
                                       tzone = timezone)
    )
    
    sensor$data <- data
    
    return(sensor)
}  
  
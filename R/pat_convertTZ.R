#' @title 
#' 
#' @description 
#' 
#' @param pat
#' @param to
#' 
#' @return
#' 
#' @example
#'
#'

pat_convertTZ <- function(pat, to) {
  
  if ( !pat_isPat(pat)) {
    stop("Sensor must be of class 'pa_timeseries' and 'list'.")
  }
  
  if ( !(to %in% OlsonNames()) ) {
    stop("to must be a valid timezone. Check OlsonNames() for options.")
  }
  
  data <- pat %>% 
    pat_extractData() %>% 
    mutate(
      datetime = lubridate::with_tz(time = .data$datetime,
                                    tzone = timezone)
    )
  
  pat$data <- data
  
  return(pat)
}  

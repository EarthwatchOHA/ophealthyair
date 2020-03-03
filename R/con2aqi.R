con2aqi <- function(cons, pollutant, over_replace_value, type = NULL, na.ignore = TRUE) {
  out <- sapply(cons, con2aqi_single, pollutant = pollutant, over_replace_value = over_replace_value,
                type = type, na.ignore = na.ignore)
  return(out)
}
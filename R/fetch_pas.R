fetch_pas <- function(countryCodes, lookbackDays = 7, output_path = "data/pas.rds") {
  # TODO: Docstring
  pas <- AirSensor::pas_createNew(countryCodes = countryCodes, lookbackDays = lookbackDays)
  saveRDS(pas, output_path)
  return(pas)
}

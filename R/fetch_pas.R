#' @title
#'
#' \code{fetch_pas} returns Purple Air Synoptic (pas) dataframe.
#'
#' Wrapper around Mazama Science's AirSensor package's pas_createNew() that
#' fetches, serializes, and saves as .RDS Purple Air Synoptic dataframe. See
#' \code{vignette("pas_introduction", package = "AirSensor")} for details.
#'
#' @param countryCodes Character vector of ISO 3166-1 alpha-2 country codes of
#'   countries for which PAS data should be looked up.
#' @param lookbackDays Integer number of days to look back for Purple Air
#'   Synoptic data. Sensors that have not uploaded data to Purple Air within the
#'   number of lookback days will not be included in returned pas.
#' @param output_path The path and filename into which the pas .RDS file should
#'   be saved. Default is data/pas.rds
#'
#' @return Returns a Purple Air Synoptic dataframe of all Purple Air Sensors
#'   that have uploaded information to \url{https://www.purpleair.com/} within
#'   the \code{lookbackDays} argument. Saves a .RDS copy of the object to the
#'   location specified by \code{output_path}.

fetch_pas <- function(
  countryCodes,
  lookbackDays = 7,
  output_path = "data/pas.rds"
) {
  pas <- AirSensor::pas_createNew(countryCodes = countryCodes, lookbackDays = lookbackDays)
  saveRDS(pas, output_path)
  return(pas)
}

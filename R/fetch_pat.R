#' @title Ingest single Purple Air Time Series.
#'
#' \code{fetch_pat} returns the Purple Air Time Series specified by matching label
#' and id.
#'
#' @description This is a wrapper function around Mazama Science's AirSensor package's
#' pat_createNew with exception handling. Primarily an internal function used in
#' fetch_pat_list to iteratively  series of Purple Air Time Series objects.
#'
#' @param label The label of the sensor to fetch as registered on the Purple Air
#'   website. See \url{https://www.purpleair.com/map} for reference.
#' @param id The five or six digit unique identifier of the sensor as obtained
#'   through Purple Air registration.See \url{https://www.purpleair.com/map} for
#'   reference.
#' @param pas Purple Air Synoptic dataframe obect, from Mazama Science's
#'   AirSensor package, containing metadata for sensor uploading to Purple Air.
#'   See For more details see the vignette: \code{vignette("pas_introduction",
#'   package = "AirSensor")}.
#' @param startdate 7-digit numeric date (ex. 20200231 = 31st January, 2020) at
#'   which to begin lookup of sensor data. If none supplied, uses 7 days prior.
#' @param enddate 7-digit numeric date (ex. 20200131 = 31st January, 2020) at
#'   which to cease lookup of sensor data. If none supplied, most recent upload
#'   is used.
#' @param timezone Timezone used to interpret start and end dates. Default is
#'   UTC.
#'
#' @return If all inputs are supplied correctly, a Purple Air Timeseries (pat)
#'   object is returned for the sensor with the supplied label and id, beginning
#'   at startdate and ending at enddate. If an error occurs during the curl
#'   request of sensor data, a string of the error message is saved in place of
#'   the pat object. For more information see the vignette
#'   \code{vignette("pat_introduction", package = "AirSensor")}.
#'
#' @examples
#' \code{
#' # Returns past week of 226RAR_kitchen measurements.
#' pat <- fetch_pat(label = "226RAR_kitchen", id = 29913)
#' }
#'
#' \code{
#' # Returns 226 RAR Kitchen pat from Jan. 1, 2020 to most recent upload.
#' pat <- fetch_pat(label = "226RAR_kitchen", id = 29913, startdate = 20200101)
#' }
#'
#' \code{
#' # Returns 226 RAR Kitchen pat from Jan. 1, 2019 to Jan 1, 2020.
#' pat <- fetch_pat(label = "226RAR_kitchen", id = 29913, startdate = 20190101, enddate = 20200101)
#' }

fetch_pat <- function(
  label,
  pas,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL
) {
  # Creates a new pat object, but intended to be used iteratively, with error and warning handlers.

  tryCatch(
    expr = {
      pat <- AirSensor::pat_createNew(pas = pas, label = label,
                                      startdate = startdate, enddate = enddate,
                                      timezone = timezone)
      print(paste(label, ": ingestion successful."))
      return(pat)
    },
    error = function(e) {
      message(paste(label, ":", e))
      return(paste(e))
    })
}

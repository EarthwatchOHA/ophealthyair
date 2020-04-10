#' @title Ingest multiple Purple Air Time Series into named list.
#'
#' \code{fetch_pat_list} returns named list of Purple Air Time Series (pat).
#'
#' @description Wrapper function around fetch_pat with options for vectorized
#'   inputs used to ingest multiple Purple Air Time Series and hold in a named
#'   list. Saves a .RDS file of pat_list to file specified in output_path. This
#'   function is run every day using the sensors contained in the Earthwatch
#'   Sensor Catalog and used in conjunction with load_pat_list for data
#'   persistence.
#'
#' @param pas Purple Air Synoptic dataframe obect, from Mazama Science's
#'   AirSensor package, containing metadata for sensor uploading to Purple Air.
#'   See For more details see the vignette: \code{vignette("pas_introduction",
#'   package = "AirSensor")}.
#' @param sensor_labels Character vector of sensor labels to be fetched.
#' @param sensor_ids Integer vector of sensor id's to be fetched.
#' @param startdate Integer vector of 7-digit numeric date (ex. 20200231 = 31st
#'   January, 2020) at which to begin lookup of sensor data. Length of startdate
#'   must be equal to the length of enddate. NULL values can be passed, and will
#'   default to 7 days prior. If passing vector of length greater than 1, length
#'   must be equal to sensor_labels.
#' @param enddate Integer vector of 7-digit numeric date (ex. 20200231 = 31st
#'   January, 2020) at which to end lookup of sensor data. Length of enddate
#'   must be equal to the length of startdate. NULL values can be passed, and
#'   will default to most recent upload. If passing vector of length greater
#'   than 1, length must be equal to sensor_labels.
#' @param timezone Timezone used to interpret start and end dates. Default is
#'   UTC.
#' @param output_path The path and filename into which the pat_list .RDS file
#'   should be saved. Default is data/pat_list.rds
#'
#' @return A named list, the length of sensor_labels of Purple Air Time Series
#'   (pat) objects. If an error occurs during the curl request of sensor data, a
#'   string of the error message is saved in place of the pat object. For more
#'   information see the vignette \code{vignette("pat_introduction", package =
#'   "AirSensor")}.
#'
#' @examples
#' \code{
#' pas <- load_pas()
#' labels <- c("226RAR_kitchen, "Red Acre Stow")
#' ids <- c(29913, 29709)
#' start <- c(20200101, 20200102)
#' end <- c(NULL, 20200201)
#' # Returns list of pat object, length of and with names supplied by sensor label.
#' # pat for 226RAR_kitchen will have all measurement uploaded between January 1st, 2020 UTC 
#' # and the most recent measurement.
#' # pat for Red Acre Stow will have all uploaded between January 2nd, 2020 UTC
#' # and February 1st, 2020. 
#' fetch_pat_list(pas = pas, sensor_labels = labels, sensor_ids = ids,
#'                startdate = start, enddate = end)
#' }
#'
#'   

fetch_pat_list <- function(
  pas, 
  sensor_labels,
  sensor_ids,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  output_path = "data/pat_list.rds"
) {
  
  # Creates list of pat objects from a pas object and vector of sensor labels and sensor ID's.
  # TODO: Add option for vectorized start and end dates.
  # TODO: Fix growing list. 
  
  if (!purrr::is_bare_vector(sensor_labels)) {
    sensor_labels <- unlist(sensor_labels)
  }
  if (!purrr::is_bare_vector(sensor_ids)) {
    sensor_ids <- unlist(sensor_ids)
  }
  if (length(startdate) != length(enddate)) {
    stop("startdate and enddate arguments must be of equal length.")
  }
  if (length(startdate) > 1 & length(startdate) != length(sensor_labels)) {
    stop("if passing a vector of startdates and enddates, length must of vectors must be equal to length of sensor_labels.")
  }
  
  # If start and enddate are null or singular values.
  if (length(startdate) <= 1) {
      suppressWarnings(expr = {
      pat_list <- purrr::map2(.x = sensor_labels, .y = sensor_ids, .f = fetch_pat, pas = pas,
                              startdate = startdate, enddate = enddate, timezone = timezone) %>% 
        rlang::set_names(sensor_labels)
    })
  }
  # If start and enddate are neither atomic or singular, vectorize over them.
  if (length(startdate) > 1) {
    suppressWarnings(expr = {
      pat_list <- purrr::pmap(.l = list(label = sensor_labels,
                                        id = sensor_ids,
                                        startdate = startdate,
                                        enddate = enddate), .f = fetch_pat, pas = pas, timezone = timezone) %>% 
        rlang::set_names(sensor_labels)
    })
  }
  if (is.null(output_path)) {
    return(pat_list)
  } else {
    saveRDS(pat_list, output_path)
    invisible(pat_list)
  }
}
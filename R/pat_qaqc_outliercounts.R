#' @title
#'
#' @description Wrapper function around sequence of Mazama Science's AirSensor
#'   package's quality assurance and control functions (see
#'   \code{vignette("purpleair_qc", package = "AirSensor")} for details): 
#'   \enumerate{
#'     \item{1. pat_aggregateOutlierCounts}
#'     \item{2. PurpleAirQC_hourly_AB_01}
#'   }
#'   See \code{vignette("qaqc_pt1.rmd", package = "ophealthyair")} for details
#'   of Operation Healthy Air QAQC Process.
#'
#' @param pat Purple Air Time Series object.
#' @param period The period of time to be aggregated to. Should be a character
#'   string, with a number, and a common unit of time. Default is 1 hour.
#' @param windowSize Hampel filter rolling window size. Default is 23.
#' @param thresholdMin The number of standard deviations calculated from the
#'   rolling window, outside of which, an observation is marked as an outlier.
#' @param max_humidity Maximum humidity threshold above which pm25 measurements
#'   are invalidated. Disabled unless explicitly set.
#' @param min_count Aggregation bins with fewer than 'min_count' measurements
#'   will be marked as 'NA'. #TODO: This is not true.
#'
#' TODO: Update with all columns.
#' @return Dataframe with columns datetime and pm25. If
#'   returnAllColumns is TRUE, additional columns outputted by
#'   pat_aggregateOutlierCounts are retained.
#'
#' @examples
#' \dontrun{
#' pat <- load_pat_list()[[1]]
#' # QAQC to 3-hour average, using a large rolling window of 40, and conservative outlier threshold minimum of 3.
#' # Maximum allowable humidity is 80 percent and only datetime and pm25 columns will be returned.
#' pat_qaqc_outliercounts(pat, period = "3 hours", windowSize = 40, thresholdMin = 3,
#'                        max_humidity = 80, returnAllColumns = FALSE)
#' }

# QAQC Functions
pat_qaqc_outliercounts <- function(
  pat,
  period = "1 hour",
  windowSize = 23,
  thresholdMin = 8,
  max_humidity = NULL,
  min_count = 20
) {

  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)

  # QAQC Steps:
  # 2. Count and replace outliers in dataset.
  # 3. Aggregate data to hourly.
  pat_qcd_outliercounts <- pat_aggregateOutlierCounts(pat, replace = c("pm25_A", "pm25_B"), period = period,
                                                      windowSize = windowSize, thresholdMin = thresholdMin)
  # 4. Apply PurpleAirQC_hourly_AB_01.
  pat_qaqcd_outliercounts <- PurpleAirQC_hourly_AB_01_revised(pat_qcd_outliercounts,
                                                              returnAllColumns = TRUE,
                                                              min_count = min_count) 
  
  return(pat_qaqcd_outliercounts)
}
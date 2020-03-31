# QAQC Functions
pat_qaqc_outliercounts <- function(pat, period = "1 hour",
                         windowSize = 23, thresholdMin = 8,
                         max_humidity = NULL, min_count = 20) {

  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # QAQC Steps:
  
  # 1. Apply pat_qc to remove invalid readings.
  pat <- pat_qc(pat, max_humidity = max_humidity)
  
  # 2. Count and replace outliers in dataset.
  # 3. Aggregate data to hourly.
  pat_outliercounts <- pat_aggregateOutlierCounts(pat, replace = c("pm25_A", "pm25_B"), period = period,
                                                      windowSize = windowSize, thresholdMin = thresholdMin)
  # 4. Apply PurpleAirQC_hourly_AB_01.
  pat_qc_outliercounts <- PurpleAirQC_hourly_AB_01(pat_outliercounts, returnAllColumns = TRUE, min_count = min_count) 
  
  return(pat_qc_outliercounts)
}
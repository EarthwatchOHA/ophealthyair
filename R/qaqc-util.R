# QAQC Functions
pat_qaqc_agg <- function(pat, period = "1 hour",
                         windowSize = 23, thresholdMin = 8,
                         max_humidity = NULL, min_count = 20, showPlot = FALSE) {
  
  if (showPlot) {
    # Plotting outlier plot to see flagged outliers and imputed values in both channels. 
    outlier_plot <- pat %>% 
      pat_qc(max_humidity = max_humidity) %>%
      pat_outlierPlot(windowSize = windowSize,
                      thresholdMin = thresholdMin)
    
    print(outlier_plot)    
  }
  
  # QAQC Steps:
  agg_qaqc <- 
    # 1. Apply pat_qc to remove invalid readings.
    pat_qc(pat, max_humidity = max_humidity) %>% 
    # 2. Count and replace outliers in dataset.
    # 3. Aggregate data to hourly.
    pat_aggregateOutlierCounts(replace = c("pm25_A", "pm25_B"), period = period,
                               windowSize = windowSize, thresholdMin = 8) %>% 
    # 4. Apply PurpleAirQC_hourly_AB_01.
    PurpleAirQC_hourly_AB_01(returnAllColumns = TRUE, min_count = min_count) 
  
  return(agg_qaqc)
}


.qaqc_filtering <- function(df) {
  
  output <- df %>% 
               mutate(
                 temperature = ifelse(min_count < 20,
                                              NA,
                                              temperature_mean),
                 humidity = ifelse(min_count < 20,
                                           NA,
                                           humidity_mean),
                 pm25 = ifelse(min_count < 20 | (pm25_p < 0.0001 & mean_diff > 10) | (pm25 < 100 & mean_diff > 20),
                                       NA,
                                       pm25)
               )
  return(output)
} 

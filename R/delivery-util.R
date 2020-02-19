.prep_agg_wkbk_data <- function (agg, aqi_col = TRUE) {
  # Prepares aggregated sensor data (the result of passing pat_list through agg-qaqc.R) for data-viz package excel workbook.
  agg_prepped <- 
    agg %>% 
    # Add aqi column, and sum A and B outlier counts into outlierCount.
    mutate(
      aqi = revised_con2aqi(con = pm25, pollutant = "pm25", na.rm = TRUE),
      outlierCount = pm25_A_outlierCount + pm25_B_outlierCount,
      temperature = (temperature_mean - 32) * 5/9
    )
  
  include_cols <- c("datetime", "pm25", "aqi", "outlierCount",
                    "temperature", "humidity" = "humidity_mean")
  
  if (!aqi_col) {
    include_cols[-3]
  }  
  
  agg_prepped <- 
    agg_prepped %>%
    # Select only necessary columns.
    select(include_cols)
  
  return(agg_prepped)
}

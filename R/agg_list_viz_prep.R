agg_list_viz_prep <- function(agg_list) {
  # Preparing preliminary datasets.
  # TODO: Refactor
  # Days during the workweek.
  workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  # Days of the weekend.
  weekend <- c("Saturday", "Sunday")
  
  # Readying plotting set.
  agg_df_prepped <- agg_list %>% 
    # Convert to df.
    bind_rows(.id = "sensor") %>%
    # Filter by minimum observation count of 20.
    filter(min_count >= 20) %>% 
    mutate(
      # Mean, mean maximum, mean minimum of A and B channels
      pm_channels_max = (pm25_A_max+pm25_B_max)/2,
      pm_channels_min = (pm25_A_min+pm25_B_min)/2,
      # Calculating AQI for each of above channels.
      # If pm is above 500 (the upper limit of AQI scale), aqi = 500. 
      aqi = if_else(pm25 <= 500, con2aqi_single(con = pm25, pollutant = "pm25"), 500, missing = pm25),
      aqi_min = if_else(pm_channels_min <= 500,
                        con2aqi_single(con = pm_channels_min, pollutant = "pm25"),
                        500, missing = pm_channels_min),
      aqi_max = if_else(pm_channels_max <= 500,
                        con2aqi_single(con = pm_channels_max, pollutant = "pm25"),
                        500, missing = pm_channels_max),
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Extracting hour.
      hour = format(datetime, "%H"),
      # Workweek 1 or 0.
      workweek = factor(if_else(weekday %in% workweek, "workweek", "weekend"))) 
  
  return(agg_df_prepped)
}

site_hourlyavg_aqiPlot <- function(data, sensor_colors, ribbon_alpha = 0.3,
                               aqi_bar_alpha = 0.2, point_size = 1) {
  
  # TODO: Add docstring.
  # TODO: Add error control system. 
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info()
  
  # Getting the max and min observation time for plotting limits.
  xmax <- max(data[["datetime"]])
  xmin <- min(data[["datetime"]])
  
  if ( !(is.data.frame(data)) )
    stop("data must be a dataframe.")
  
  if ( is.null(data$datetime) | !lubridate::is.POSIXct(data$datetime))
    stop("data must have datetime column containing POSIXct values.")
  
  plot <-
    data %>% 
    group_by(datetime) %>% 
    ggplot(aes(x = datetime)) +
    geom_point(aes(y = aqi, color = factor(sensor)), size = point_size) +
    geom_ribbon(aes(ymin = aqi_min, ymax = aqi_max, fill = factor(sensor)), alpha = ribbon_alpha) +
    scale_x_datetime() +
    scale_color_manual(values = sensor_colors) +
    scale_fill_manual(values = sensor_colors, guide = FALSE) +
    annotate("rect", ymin = aqi_info$aqi_breaks$minimums, ymax = aqi_info$aqi_breaks$maximums,
             xmin=xmin, xmax=xmax, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
    coord_cartesian(ylim = c(0, max(data$aqi_max))) +
    labs(x = "Time",
         y = "AQI",
         title = "Channel Hourly Averages AQI",
         subtitle = "with Max and Min Ribbon",
         color = "Sensor")
  return(plot)
}
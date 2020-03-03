site_hourlyavg_pmPlot <- function(
  data,
  sensor_colors,
  aqi_country,
  ribbon_alpha = 0.3,
  aqi_bar_alpha = 0.2,
  point_size = 1
) {
  
  # TODO: Add docstring.
  # TODO: Add error control system. 
  
 
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  # Getting the max and min observation time for plotting limits.
  xmax <- max(data[["datetime"]])
  xmin <- min(data[["datetime"]])
  
  if ( !(is.data.frame(data)) ) {
    stop("data must be a dataframe.")
  }
  if ( is.null(data$datetime) | !lubridate::is.POSIXct(data$datetime)) {
    stop("data must have datetime column containing POSIXct values.")
  }
  plot <-
    data %>% 
    dplyr::group_by(datetime) %>% 
    ggplot2::ggplot(aes(x = datetime)) +
    ggplot2::geom_point(aes(y = pm25, color = factor(sensor)), size = point_size) +
    ggplot2::geom_ribbon(aes(ymin = pm_channels_min, ymax = pm_channels_max, fill = factor(sensor)), alpha = ribbon_alpha) +
    ggplot2::scale_x_datetime() +
    ggplot2::scale_color_manual(values = sensor_colors) +
    ggplot2::scale_fill_manual(values = sensor_colors, guide = FALSE) +
    # TODO: AQI PM Breaks need to be made to work. 
    ggplot2::annotate(geom = "rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
                      xmin=xmin, xmax=xmax, alpha = aqi_bar_alpha,
                      fill = aqi_info$colors) +
    ggplot2::coord_cartesian(ylim = c(0, max(data$pm_channels_max, na.rm = TRUE))) +
    ggplot2::labs(x = "Datetime (UTC)",
         y = "PM 2.5 μg/m3",
         title = "Hourly Average Particulate Matter 2.5 Concentration",
         subtitle = "with Hourly Max and Min Ribbon",
         color = "Sensor")
 
   return(plot)
}
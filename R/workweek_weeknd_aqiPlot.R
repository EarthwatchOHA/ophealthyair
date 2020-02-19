workweek_weeknd_aqiPlot <- function(data, sensor_colors, line_thickness = 1, aqi_bar_alpha = 0.2) {
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info()
  
  # Summarizing data over workweek or weekend.
  workweek_weeknd_data <- 
    data %>% 
    group_by(hour, workweek, sensor) %>% 
    summarize(aqi = mean(aqi))
  
  
  workweek_weeknd_plot <-
    workweek_weeknd_data %>%
    ggplot(aes(x = hour, y = aqi, group = interaction(sensor, workweek))) +
    geom_line(aes(color = sensor, linetype = workweek), size = line_thickness) + 
    annotate("rect", ymin = aqi_info$aqi_breaks$minimums, ymax = aqi_info$aqi_breaks$maximums,
             xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
    scale_linetype_manual(values=c("solid", "twodash")) +
    scale_color_manual(values = sensor_colors) +
    coord_cartesian(ylim = c(0, max(workweek_weeknd_data$aqi))) +
    labs(x = "Hour of Day",
         y = "AQI", 
         title = "Hour of Day AQI Average Weekday vs. Weekend",
         subtitle = "Max and Min Ribbon",
         color = "Sensor",
         linetype = "Day Type")
  
  return(workweek_weeknd_plot)  
}
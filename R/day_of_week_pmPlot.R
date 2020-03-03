day_of_week_pmPlot <- function(
  data,
  sensor_colors,
  aqi_country,
  ribbon_alpha = 0.2,
  aqi_bar_alpha = 0.2,
  line_thickness = 1,
  title_text = "Average Hourly Particulate Matter 2.5 Concentration",
  x_axis_label = "Hour of Day (UTC)",
  y_axis_label = "PM 2.5 μg/m3"
) {
  
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  # AQI Average Hourly Line plots faceted by day of the week.
  day_of_week_data <- 
    data %>% 
    group_by(hour, weekday, sensor) %>% 
    summarize(pm25 = mean(pm25, na.rm = TRUE),
              pm25_min = mean(pm_channels_min, na.rm = TRUE),
              pm25_max = mean(pm_channels_max, na.rm = TRUE))
  
  
  # Pasting the vectors together.
  plot_fill_colors <- append(sensor_colors, aqi_info$colors)
  
  full_week <- c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday")
  
  day_plots <- list()
  
  for (i in 1:length(full_week)) {
    day <- full_week[[i]]
    
    day_plots[[day]] <- 
      day_of_week_data %>% 
      filter(weekday == day) %>% 
      ggplot(aes(x = hour, y = pm25, group = sensor)) +
      geom_line(aes(color = sensor), size = line_thickness) +
      geom_point(aes(color = sensor)) +
      geom_ribbon(aes(ymin = pm25_min, ymax = pm25_max, fill = sensor), alpha = ribbon_alpha) +
      scale_color_manual(values = sensor_colors) +
      annotate("rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
               xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
      scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      coord_cartesian(ylim = c(0, max(day_of_week_data$pm25_max, na.rm = TRUE))) +
      labs(subtitle = day,
           color = "Sensor")
  }
  weekday_plots <- grob_arrange_shared(plots_list = day_plots, title_text = title_text, nrows = 2,
                                       y_axis_label = y_axis_label, x_axis_label = x_axis_label)
  invisible(weekday_plots)
}
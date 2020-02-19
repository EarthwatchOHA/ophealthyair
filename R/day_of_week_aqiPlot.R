day_of_week_aqiPlot <- function(data, sensor_colors,
                                ribbon_alpha = 0.2,
                                aqi_bar_alpha = 0.2, line_thickness = 1,
                                title_text = "Average Hourly AQI",
                                x_axis_label = "Hour of Day", y_axis_label = "AQI") {
  
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info()
  
  # AQI Average Hourly Line plots faceted by day of the week.
  day_of_week_data <- 
    data %>% 
    group_by(hour, weekday, sensor) %>% 
    summarize(aqi = mean(aqi),
              aqi_min = mean(aqi_min),
              aqi_max = mean(aqi_max))
  
  
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
      ggplot(aes(x = hour, y = aqi, group = sensor)) +
      geom_line(aes(color = sensor), size = line_thickness) +
      geom_point(aes(color = sensor)) +
      geom_ribbon(aes(ymin = aqi_min, ymax = aqi_max, fill = sensor), alpha = ribbon_alpha) +
      scale_color_manual(values = sensor_colors) +
      annotate("rect", ymin = aqi_info$aqi_breaks$minimums, ymax = aqi_info$aqi_breaks$maximums,
               xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
      scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      coord_cartesian(ylim = c(0, max(day_of_week_data$aqi_max))) +
      labs(subtitle = day,
           color = "Sensor")
  }
  weekday_plots <- grob_arrange_shared(plots_list = day_plots, title_text = title_text, nrows = 2,
                                       y_axis_label = y_axis_label, x_axis_label = x_axis_label)
  invisible(weekday_plots)
}
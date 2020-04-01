day_of_week_pmPlotlist <- function(
  outliercount_list,
  sensor_colors,
  aqi_country,
  ribbon_alpha = 0.2,
  aqi_bar_alpha = 0.2,
  line_thickness = 1
) {
  
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  # Readying plotting set.
  data <- outliercount_list %>% 
    # Convert to df.
    bind_rows(.id = "sensor") %>%
    mutate(
      # Mean, mean maximum, mean minimum of A and B channels
      pm_channels_max = (pm25_A_max+pm25_B_max)/2,
      pm_channels_min = (pm25_A_min+pm25_B_min)/2,
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Extracting hour.
      hour = format(datetime, "%H"),
      # Workweek 1 or 0.
      workweek = factor(if_else(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                "workweek", "weekend"))
    )
  
  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")
  
  # Creating x label with timezone.
  x_label <- paste("Hour of Day ", "(", timezone, ")", sep = "")
  
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
    
    day_data <- day_of_week_data %>% 
      filter(weekday == day)
    
    day_plots[[day]] <- day_data %>% 
      ggplot(aes(x = hour, y = pm25, group = sensor)) +
      geom_line(aes(color = sensor), size = line_thickness) +
      geom_point(aes(color = sensor)) +
      geom_ribbon(aes(ymin = pm25_min, ymax = pm25_max, fill = sensor), alpha = ribbon_alpha) +
      scale_color_manual(values = sensor_colors) +
      annotate("rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
               xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
      scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      coord_cartesian(ylim = c(0, max(day_of_week_data$pm25_max, na.rm = TRUE))) +
      labs(
        title = "Average Hourly Particulate Matter 2.5 Concentration",
        subtitle = day,
        caption = "with hourly max and min ribbons",
        color = "Sensor Label",
        x = x_label, 
        y = "PM 2.5 μg/m3")
  }
  invisible(day_plots)
}
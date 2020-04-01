workweek_weeknd_pmPlot <- function(
  outliercount_list,
  sensor_colors,
  aqi_country,
  line_thickness = 1,
  aqi_bar_alpha = 0.2
) {
  
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  # Readying plotting set.
  data <- outliercount_list %>% 
    # Convert to df.
    bind_rows(.id = "sensor") %>%
    mutate(
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
  
  # Summarizing data over workweek or weekend.
  workweek_weeknd_data <- 
    data %>% 
    group_by(hour, workweek, sensor) %>% 
    summarize(pm25 = mean(pm25, na.rm=TRUE))
  
  workweek_weeknd_plot <-
    workweek_weeknd_data %>%
    ggplot(aes(x = hour, y = pm25, group = interaction(sensor, workweek))) +
    geom_line(aes(color = sensor, linetype = workweek), size = line_thickness) + 
    annotate("rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
             xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
    scale_linetype_manual(values=c("solid", "twodash")) +
    scale_color_manual(values = sensor_colors) +
    coord_cartesian(ylim = c(0, max(workweek_weeknd_data$pm25, na.rm = TRUE))) +
    labs(x = x_label,
         y = "PM 2.5 μg/m3", 
         title = "Average Particulate Matter 2.5 Concentration by Hour of Day Weekday vs. Weekend",
         color = "Sensor Label",
         linetype = "Day Type")
  
  return(workweek_weeknd_plot)  
}
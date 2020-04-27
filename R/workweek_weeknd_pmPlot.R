#' @title Plots Particulate Matter 2.5 Hour of Day Averages with lines showing Workweek and Weekend Averages 
#' 
#' \code{workweek_weeknd_pmPlot} returns a ggplot object.
#' 
#' @description 
#' 
#' @param aggstats_list
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Current options are United States ("US") or India ("IN").
#' @param sensor_colors Character Vector the length of unique sensors in
#'   outliercount_list, of hexadigit colors to use for distinguishing sensor
#'   lines. If NULL, the default, a random palette will be produce. See details
#'   for more information.
#' @param aqi_bar_alpha Numeric between 0 and 1 indicating the opacity of the
#'   AQI background colors. Default is 0.2
#' 
#' @return 
 

workweek_weeknd_pmPlot <- function(
  aggstats_list,
  aqi_country,
  sensor_colors = NULL,
  aqi_bar_alpha = 0.2
) {
  
  # TODO: Add docstring.
  # TODO: Add error control system.
  
  if (is.null(sensor_colors)) {
    # Making palette of colors for sensors.
    sensor_colors <- RColorBrewer::brewer.pal(n = length(aggstats_list), name = "Dark2")
    # Naming the vector using sensor labels.
    names(sensor_colors) <- names(aggstats_list)
  }
  
  # Readying plotting set.
  data <- aggstats_list %>% 
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
    geom_line(aes(color = sensor, linetype = workweek), size = 1) + 
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
#' @title
#'
#' \code{day_of_week_pmPlotlist} returns a named list of length 7 containing
#' ggplot objects for each day of the week.
#'
#' @description Generates seven (one for each day of the week) hourly average
#'   point/line plot for sensors aggstats_list.
#'
#' @param aggstats_list
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Current options are United States ("US") or India ("IN").
#' @param sensor_colors Character Vector the length of unique sensors in
#'   outliercount_list, of hexadigit colors to use for distinguishing sensor
#'   lines. If NULL, the default, a random palette will be produce. See details
#'   for more information.
#' @param ribbon_alpha Numeric between 0 and 1 indicating the opacity of the
#'   maximum/minimum ribbon. Default is 0.3.
#' @param aqi_bar_alpha Numeric between 0 and 1 indicating the opacity of the
#'   AQI background colors. Default is 0.2
#'
#' @return A length seven named list of ggplot objects.

day_of_week_pmPlotlist <- function(
  aggstats_list,
  aqi_country,
  sensor_colors = NULL,
  ribbon_alpha = 0.3,
  aqi_bar_alpha = 0.2
) {
  
  # TODO: Add error control system.
  
  if (is.null(sensor_colors)) {
    # Making palette of colors for sensors.
    sensor_colors <- RColorBrewer::brewer.pal(n = length(aggstats_list), name = "Dark2")
    # Naming the vector with using sensor labels.
    names(sensor_colors) <- names(aggstats_list)
  }
  
  # Readying plotting set.
  data <- aggstats_list %>% 
    # Convert to df.
    dplyr::bind_rows(.id = "sensor") %>%
    dplyr::mutate(
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Extracting hour.
      hour = format(datetime, "%H")
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
    dplyr::group_by(sensor, weekday, hour) %>% 
    dplyr::summarize(
      pm25_min = min(pm25, na.rm = TRUE),
      pm25_max = max(pm25, na.rm = TRUE),
      pm25 = mean(pm25, na.rm = TRUE),
      N = n()
    )
  
  # Pasting the vectors together.
  plot_fill_colors <- append(sensor_colors, aqi_info$colors)
  
  full_week <- c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday")
  
  day_plots <- list()
  
  for (i in 1:length(full_week)) {
    day <- full_week[[i]]
    
    day_data <- day_of_week_data %>% 
      dplyr::filter(weekday == day)
    
    day_plots[[day]] <- day_data %>% 
      ggplot2::ggplot(aes(x = hour, y = pm25, group = sensor)) +
      ggplot2::geom_line(aes(color = sensor), size = 1) +
      ggplot2::geom_point(aes(color = sensor)) +
      ggplot2::geom_ribbon(aes(ymin = pm25_min, ymax = pm25_max, fill = sensor), alpha = ribbon_alpha) +
      ggplot2::scale_color_manual(values = sensor_colors) +
      ggplot2::annotate("rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
               xmin = -Inf, xmax = Inf, alpha = aqi_bar_alpha, fill = aqi_info$colors) +
      ggplot2::scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      ggplot2::coord_cartesian(ylim = c(0, max(day_of_week_data$pm25_max, na.rm = TRUE))) +
      ggplot2::labs(
        title = "Average Hourly Particulate Matter 2.5 Concentration",
        subtitle = day,
        caption = "with hourly max and min ribbons",
        color = "Sensor Label",
        x = x_label, 
        y = "PM 2.5 μg/m3")
  }
  invisible(day_plots)
}
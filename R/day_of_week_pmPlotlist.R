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
  sensor_list,
  aqi_country,
  sensor_colors = NULL,
  sd_ribbon = NULL,
  ribbon_alpha = 0.3,
  aqi_bar_alpha = 0.2
) {
  

  if ( !is.null(sd_ribbon) ) {
    
    if ( !is.numeric(sd_ribbon) | length(sd_ribbon) != 1 ) {
      stop("sd_ribbon must be NULL or numeric value.")
    }
    
  } else {
    
    sd_ribbon <- 0
  }
  
  if (is.null(sensor_colors)) {
    # Making palette of colors for sensors.
    sensor_colors <- RColorBrewer::brewer.pal(n = length(sensor_list), name = "Dark2")
    # Naming the vector with using sensor labels.
    names(sensor_colors) <- names(sensor_list)
  }
  
  # Readying plotting set.
  data <- 
    purrr::map(.x = sensor_list,
               .f = function(x) sensor_extractData(x) %>%
                 rename("pm25" = 2)) %>%
    # Convert to df.
    dplyr::bind_rows(.id = "label") %>% 
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
  week_data <- 
    data %>% 
    dplyr::group_by(label, weekday, hour) %>% 
    dplyr::summarize(
      sd = sd(pm25, na.rm = TRUE) * sd_ribbon,
      pm25 = mean(pm25, na.rm = TRUE),
      ribbon_low = pm25 - sd,
      ribbon_high = pm25 + sd,
      N = n()
    )
  
  # Pasting the vectors together.
  plot_fill_colors <- append(sensor_colors, aqi_info$colors)
  
  full_week <- c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday")
  
  day_plots <- list()
  
  for (i in 1:length(full_week)) {
    day <- full_week[[i]]
    
    day_data <- week_data %>% 
      dplyr::filter(weekday == day)
    
    plot <- day_data %>% 
      ggplot2::ggplot(aes(x = hour, y = pm25, group = label)) +
      ggplot2::geom_line(aes(color = label), size = 1) +
      ggplot2::geom_point(aes(color = label)) +
      ggplot2::scale_color_manual(values = sensor_colors) +
      ggplot2::annotate(
        geom = "rect",
        ymin = aqi_info$aqi_pm_mins,
        ymax = aqi_info$aqi_pm_maxs,
        xmin = -Inf,
        xmax = Inf,
        alpha = aqi_bar_alpha,
        fill = aqi_info$colors) +
      ggplot2::scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      ggplot2::labs(
        title = "Average Hourly Particulate Matter 2.5 Concentration",
        subtitle = day,
        color = "Sensor Label",
        x = x_label, 
        y = "PM 2.5 μg/m3")
      
      if ( !is.null(sd_ribbon) ){
        
        y_max <- max(day_data$ribbon_high, na.rm = TRUE)
        
        plot <- plot +
          ggplot2::geom_ribbon(aes(ymax = ribbon_high, ymin = ribbon_low,
                                   fill = label), alpha = ribbon_alpha) +
          ggplot2::coord_cartesian(
            ylim = c(0, y_max) 
          ) +
          ggplot2::labs(
            caption = paste("Ribbons show", sd_ribbon, "standard deviation above and below mean.")
          )
      } else {
      
        y_max <- max(day_data$pm25, na.rm = TRUE) * 1.1
        
        plot <- plot + 
          ggplot2::coord_cartesian(
            ylim = c(0, y_max)
          )
    }
    
    day_plots[[day]] <- plot
    
  }
  
  invisible(day_plots)
}
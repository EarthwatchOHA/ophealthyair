#' @title Site Sensor Hourly Averages Plot with Max and Min Ribbons and AQI
#'   Background
#'
#'   \code{site_hourlyavg_pmPlot} returns a ggplot object.
#'
#' @description Input dataframe of PM hourly averages with columns datetime,
#'   pm25, sensor, pm_channels_min, pm_channels_max, and a color palette with
#'   length equal to the number of sensors in data. Generates a timeline plot of
#'   hourly averages per sensor with maximum and minimum ribbons.
#'
#' @param sensor_list 
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Current options are United States ("US") or India ("IN").
#' @param sensor_colors Character Vector the length of unique sensors in
#'   outliercount_list, of hexadigit colors to use for distinguishing sensor
#'   lines. If NULL, the default, a random palette will be produce. See details
#'   for more information.
#' @param aqi_bar_alpha Numeric between 0 and 1 indicating the opacity of the
#'   AQI background colors. Default is 0.2.
#'
#' @return A ggplot object.
#'
#' @references 
#' Cite ggplot2


site_hourlyavg_pmPlot <- function(
  sensor_list,
  aqi_country,
  sensor_colors = NULL,
  aqi_bar_alpha = 0.2
) {

  # TODO: Add error control system. 
  # TODO: Add averaging controls?
  # TODO: Fix monthly faceting with annotation bars.
  
  if (is.null(sensor_colors)) {
    # Making palette of colors for sensors.
    sensor_colors <- RColorBrewer::brewer.pal(n = length(sensor_list), name = "Dark2")
    # Naming the vector with using sensor labels.
    names(sensor_colors) <- names(sensor_list)
  }
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  if ( !(is.data.frame(data)) ) {
    stop("data must be a dataframe.")
  }
  if ( is.null(data$datetime) | !lubridate::is.POSIXct(data$datetime)) {
    stop("data must have datetime column containing POSIXct values.")
  }
  
  for (i in 1:length(sensor_list)) {
    
    sensor <- sensor_list[[i]]
    
    sensor_label <- sensor$meta$monitorID
    
    data <- sensor %>% 
      sensor_extractData()
    
    # Getting Timezone from data.
    timezone <- attr(data$datetime,"tzone")
    
    # Creating x label with timezone.
    x_label <- paste("Hour of Day ", "(", timezone, ")", sep = "")
    
    month = "October"
    year = 2019
    
    
    data2 <- data %>%
      rename(pm25 = sensor_label) %>%
      # Adding month variable to dataset for faceting.
      dplyr::mutate(
        day = lubridate::day(datetime),
        month = lubridate::month(datetime,
                                 label = TRUE,
                                 abbr = FALSE),
        year = lubridate::year(datetime)
        ) 
    
    xmin <- min(data2$datetime)
    xmax <- max(data2$datetime)
    
    
    
    data2 %>%
      dplyr::filter(.data$month == !!month, .data$year == !!year) %>% 
      mutate(
        aqi_category = cut(pm25, breaks = aqi_info$breaks_24,
                           labels = aqi_info$names)
      ) %>% 
      ggplot2::ggplot(aes(x = datetime)) +
      ggplot2::geom_point(aes(y = pm25, color = aqi_category), size = 2, alpha = 0.4) +
      ggplot2::scale_color_manual(
        aesthetics = "color",
        values = aqi_info$colors
        ) +
      ggplot2::geom_smooth(aes(y=pm25)) +
      ggplot2::labs(
           x = x_label,
           y = "PM 2.5 μg/m3",
           title = "Hourly Average Particulate Matter 2.5 Concentration",
           subtitle = paste(sensor_label, month, year, sep = ", ")
      ) +
      ggplot2::facet_grid(year~month, scales = "free_y") +
      ggplot2::theme_dark()
    
    

   return(plot)
  }
}
  
  #######
  # AQI Bars Not Working with Faceting
  #######
  
 
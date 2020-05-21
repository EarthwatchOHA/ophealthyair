#' @title Sensor Hourly Averages Plot with points colored by Air Quality Index
#'   Category
#'
#' @description Input dataframe of PM hourly averages with columns datetime,
#'   pm25, sensor, pm_channels_min, pm_channels_max, and a color palette with
#'   length equal to the number of sensors in data. Generates a timeline plot of
#'   hourly averages per sensor with maximum and minimum ribbons.
#'
#' @param sensor AirSensor object.
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Options are United States ("US") or India ("IN").
#' @param point_size The size of every point on the plot.
#'
#' @return A ggplot object.
#'
#' @references \code{citation("ggplot2")}


sensor_hourlyavg_aqicolorsPlot <- function(
  sensor,
  aqi_country,
  time_facet = "none",
  point_size = 1,
  point_alpha = 0.5
) {
  
  # TODO: Add error control system. 
  
  time_facet_opts <- c(
    "none",
    "year", 
    "month"
  )
  
  # Check if is Airsensor object.
  if ( !(sensor_isSensor(sensor)) ) {
    stop("sensor_list must be an AirSensor object.")
  }
  
  # Getting sensor label.
  label <- sensor$meta$label
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  data <- sensor %>%
    sensor_extractData() %>%
    dplyr::rename(pm25 = 2) %>%
    # Adding month variable to dataset for faceting.
    dplyr::mutate(
      month = lubridate::month(datetime,
                               label = TRUE,
                               abbr = FALSE),
      year = lubridate::year(datetime),
      aqi_category = cut(pm25, breaks = aqi_info$breaks_24,
                         labels = aqi_info$names)
    )
  
  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")
  
  # Creating x label with timezone.
  x_label <- paste("Hour of Day ", "(", timezone, ")", sep = "")
  
  plot <- data %>% 
    ggplot2::ggplot(aes(x = datetime)) +
    ggplot2::geom_point(aes(y = pm25, color = aqi_category),
                        size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(
      aesthetics = "color",
      values = aqi_info$colors,
      na.translate = FALSE,
      guide = guide_legend(title.position = "top", 
                           label.position = "bottom",
                           direction = "horizontal",
                           nrow = 1,
                           keywidth = 3,
                           override.aes = list(size=10)) 
    ) +
    ggplot2::labs(
      x = x_label,
      y = "PM 2.5 μg/m3",
      title = "Hourly Average Particulate Matter 2.5 Concentration",
      color = "AQI Category",
      subtitle = label
  )
  
  if ( time_facet != "none" ) {
    # Check that a valid time_facet input given.
    if( !(time_facet %in% time_facet_opts) ) {
      
      stop("time_facet must be one of 'month', 'year', or 'none'.")
    
    } else if ( time_facet == "year" ) {
      plot <- plot +
        ggplot2::facet_wrap(year~., scales = "free_x") +
        ggplot2::scale_x_datetime()
        
    } else if ( time_facet == "month" ) {
      n_years <- length(unique(data$year))
      plot <- plot +
        ggplot2::facet_wrap(month~year, scales = "free_x",
                            ncol = n_years) +
        ggplot2::scale_x_datetime(date_breaks = "1 day",
                                  date_labels = "%d")
    
    }
  }
  
  # Making Theme Dark
  plot <- plot +
    theme_dark() +
    # Move legend
    theme(legend.position = "top")
    
  
  return(plot)      
}
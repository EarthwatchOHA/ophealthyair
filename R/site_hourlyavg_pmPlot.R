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
#' @param outliercount_list 
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Current options are United States ("US") or India ("IN").
#' @param sensor_colors Character Vector the length of unique sensors in
#'   outliercount_list, of hexadigit colors to use for distinguishing sensor
#'   lines. If NULL, the default, a random palette will be produce. See details
#'   for more information.
#' @param ribbon_alpha Numeric between 0 and 1 indicating the opacity of the
#'   maximum/minimum ribbon. Default is 0.2.
#' @param aqi_bar_alpha Numeric between 0 and 1 indicating the opacity of the
#'   AQI background colors. Default is 0.2.
#'
#' @return A ggplot object.
#'
#' @references 
#' Cite ggplot2


site_hourlyavg_pmPlot <- function(
  outliercount_list,
  aqi_country,
  sensor_colors = NULL,
  ribbon_alpha = 0.3,
  aqi_bar_alpha = 0.2
) {

  # TODO: Add error control system. 
  # TODO: Add averaging controls?
  # TODO: Fix monthly faceting with annotation bars.
  
  if (is.null(sensor_colors)) {
    # Making palette of colors for sensors.
    sensor_colors <- RColorBrewer::brewer.pal(n = length(outliercount_list), name = "Dark2")
    # Naming the vector with using sensor labels.
    names(sensor_colors) <- names(outliercount_list)
  }
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  # Getting the max and min observation time for plotting limits.
  xmax <- max(data[["datetime"]])
  xmin <- min(data[["datetime"]])
  
  if ( !(is.data.frame(data)) ) {
    stop("data must be a dataframe.")
  }
  if ( is.null(data$datetime) | !lubridate::is.POSIXct(data$datetime)) {
    stop("data must have datetime column containing POSIXct values.")
  }
  
  # Adding month variable to dataset for faceting.
  data <- outliercount_list %>%
    dplyr::bind_rows(.id = "sensor") %>% 
    dplyr::mutate(
        # Mean, mean maximum, mean minimum of A and B channels
        pm_channels_max = (pm25_A_max+pm25_B_max)/2,
        pm_channels_min = (pm25_A_min+pm25_B_min)/2,
        month = lubridate::month(datetime, label = TRUE, abbr = FALSE) %>% factor(),
        sensor = factor(sensor)
    )
  
  plot <-
    data %>% 
    dplyr::group_by(datetime) %>% 
    ggplot2::ggplot(aes(x = datetime)) +
    ggplot2::geom_line(aes(y = pm25, color = sensor), size = 1) +
    ggplot2::geom_ribbon(aes(ymin = pm_channels_min, ymax = pm_channels_max, fill = sensor), alpha = ribbon_alpha) +
    ggplot2::scale_x_datetime() +
    ggplot2::scale_color_manual(values = sensor_colors) +
    ggplot2::scale_fill_manual(values = sensor_colors, guide = FALSE) +
    # TODO: This not working with monthly faceting.  
    ggplot2::annotate(geom = "rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
                      xmin=xmin, xmax=xmax, alpha = aqi_bar_alpha,
                      fill = aqi_info$colors) +
    ggplot2::coord_cartesian(ylim = c(0, max(data$pm_channels_max, na.rm = TRUE))) +
    ggplot2::labs(x = "Datetime (UTC)",
         y = "PM 2.5 μg/m3",
         title = "Hourly Average Particulate Matter 2.5 Concentration",
         subtitle = "with Hourly Max and Min Ribbon",
         color = "Sensor") +
    ggplot2::facet_grid(.~month)
 
   return(plot)
}
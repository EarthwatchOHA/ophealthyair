#' @title 
#' 
#' 
#' @description 
#' 
#' @param sensor
#' @param aqi_country
#' @param position
#'
#' @return a ggplot object.
#'
#' @example
#'


day_of_week_aqiBar <- function(
  sensor,
  aqi_country,
  position = "stack"
) {
  
  position_opts <- c(
    "fill",
    "stack"
  )
  
  aqi_country_opts <- c(
    "US", 
    "IN"
  )
  
  
  if( !sensor_isSensor(sensor) ) {
    stop("sensor must be of class 'airsensor'.")
  }
  if ( !(position %in% position_opts) ) {
    stop("position must be one of 'fill' or 'stack'.")
  }
  
  if ( !(aqi_country %in% aqi_country_opts) ) {
    stop("aqi_country must be one of 'US' or 'IN'")
  }
  
  
  full_week <- c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday")
  
  
  
  meta <- sensor %>% 
    sensor_extractMeta()
  
  # Readying plotting set.
  data <- sensor %>% 
    sensor_extractData() %>% 
    dplyr::rename(pm25 = 2) %>%
    dplyr::mutate(
      # Getting AQI category of pm25
      aqi_category = cut(pm25,
                         breaks = aqi_info$breaks_24,
                         labels = aqi_info$names),
      # Getting day of week for each hour.
      weekday = factor(weekdays(datetime),
                       levels = full_week, 
                       ordered = TRUE),
      # Extracting hour.
      hour = lubridate::hour(datetime)
    )
  
  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")
  
  # Creating x label with timezone.
  x_label <- paste("Hour of Day ", "(", timezone, ")", sep = "")
  if (position == "fill") {
    y_label <- "Proportion of Hours"
  }
  
  if (position == "stack") {
    y_label <- "Number of Hours"
  }
  
  # Loading AQI Categorical Index info for plotting.
  aqi_info <- load_aqi_info(country = aqi_country)
  
  plot <- data %>% 
      ggplot2::ggplot(aes(x = factor(hour), fill = aqi_category)) +
      ggplot2::geom_bar(position = position) +
      ggplot2::scale_fill_manual(aesthetics = "fill",
                                 values = aqi_info$colors, 
                                 na.translate = TRUE,
                                 na.value = "black", 
                                 labels = append(aqi_info$names, "Missing") 
                                 ) +
      ggplot2::scale_x_discrete() +
      ggplot2::labs(
        title = "Average Hourly PM25 AQI Category by Hour and Day",
        subtitle = meta$label,      
        x = x_label, 
        y = y_label,
        fill = "AQI Category") +
      ggplot2::facet_wrap(~weekday, nrow = 2)

  return(plot)
}
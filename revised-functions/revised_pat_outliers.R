revised_pat_outliers <- function (pat = NULL, windowSize = 23, thresholdMin = 8, replace = FALSE, 
                                   showPlot = TRUE, returnPlot = FALSE, data_shape = 18, data_size = 1,
                                   data_color = "black", data_alpha = 0.5, outlier_shape = 8, outlier_size = 1,
                                   outlier_color = "red", outlier_alpha = 1) 
# Revisions:
  # Added returnPlot argument. If TRUE and showPlot is TRUE, function returns only the plot, not the dataset.
  #TODO: Add additional column to returned pat marking imputed values ( or if replace = FALSE values that would be imputed).
  
  {
  MazamaCoreUtils::stopIfNull(pat)
  if (!AirSensor::pat_isPat(pat)) 
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  if (AirSensor::pat_isEmpty(pat)) 
    stop("Parameter 'pat' has no data.")
  pat <- AirSensor::pat_distinct(pat)
  A_data <- dplyr::filter(pat$data, !is.na(.data$pm25_A)) %>% 
    dplyr::select(-.data$pm25_B, -.data$datetime_B)
  B_data <- dplyr::filter(pat$data, !is.na(.data$pm25_B)) %>% 
    dplyr::select(.data$datetime, .data$pm25_B, .data$datetime_B)
  A_missing <- dplyr::filter(pat$data, is.na(.data$pm25_A)) %>% 
    dplyr::select(-.data$pm25_B, -.data$datetime_B)
  B_missing <- dplyr::filter(pat$data, is.na(.data$pm25_B)) %>% 
    dplyr::select(.data$datetime, .data$pm25_B, .data$datetime_B)
  A_flagged <- AirSensor::.flagOutliers(A_data, parameter = "pm25_A", 
                             windowSize = windowSize, thresholdMin = thresholdMin)
  B_flagged <- AirSensor::.flagOutliers(B_data, parameter = "pm25_B", 
                             windowSize = windowSize, thresholdMin = thresholdMin)
  A_outlierIndices <- which(A_flagged[, ncol(A_flagged)])
  B_outlierIndices <- which(B_flagged[, ncol(B_flagged)])
  if (replace) {
    A_fixed <- AirSensor::.replaceOutliers(A_data, parameter = "pm25_A")[["pm25_A"]]
    B_fixed <- AirSensor::.replaceOutliers(B_data, parameter = "pm25_B")[["pm25_B"]]
  }
  else {
    A_fixed <- A_data$pm25_A
    B_fixed <- B_data$pm25_B
    A_fixed[A_outlierIndices] <- NA
    B_fixed[B_outlierIndices] <- NA
  }
  if (showPlot) {
    timezone <- pat$meta$timezone
    A_flagged$datetime <- lubridate::with_tz(A_flagged$datetime, 
                                             tzone = timezone)
    B_flagged$datetime <- lubridate::with_tz(B_flagged$datetime, 
                                             tzone = timezone)
    ylim <- range(c(A_data$pm25_A, B_data$pm25_B), na.rm = TRUE)
    ylim[1] <- min(0, ylim[1])
    chA <- AirSensor::.plotOutliers(df = A_flagged, ylim = ylim, subtitle = pat$meta$label, 
                         data_shape = data_shape, data_size = data_size, 
                         data_color = data_color, data_alpha = data_alpha, 
                         outlier_shape = outlier_shape, outlier_size = outlier_size, 
                         outlier_color = outlier_color, outlier_alpha = outlier_alpha)
    chB <- AirSensor::.plotOutliers(df = B_flagged, ylim = ylim, subtitle = pat$meta$label, 
                         data_shape = data_shape, data_size = data_size, 
                         data_color = data_color, data_alpha = data_alpha, 
                         outlier_shape = outlier_shape, outlier_size = outlier_size, 
                         outlier_color = outlier_color, outlier_alpha = outlier_alpha)
    plot <- multi_ggplot(plotList = list(chA, chB))
    if(returnPlot) {
      return(plot)
  }
  A_data$pm25_A <- A_fixed
  B_data$pm25_B <- B_fixed
  A_full <- dplyr::bind_rows(A_data, A_missing)
  B_full <- dplyr::bind_rows(B_data, B_missing)
  data <- dplyr::full_join(A_full, B_full, by = "datetime") %>% 
    dplyr::arrange(.data$datetime)
  data <- data[, c("datetime", "pm25_A", "pm25_B", "temperature", 
                   "humidity", "uptime", "adc0", "rssi", "datetime_A", 
                   "datetime_B")]
  pat <- list(meta = pat$meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  pat <- AirSensor::pat_distinct(pat)
  return(invisible(pat))
  }
}

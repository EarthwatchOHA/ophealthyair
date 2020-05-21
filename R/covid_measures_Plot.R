#' @title
#' 
#' @description
#' 
#' @param sensor
#' @param covid_measures
#' @param facet_workweek
#' 
#' @returns
#' 
#' @example
#' 

covid_measures_Plot <- function(
  sensor,
  covid_measures,
  facet_workweek = FALSE
) {
  
  meta <- sensor %>% sensor_extractMeta()
  
  data <- sensor %>%
    sensor_extractData() %>% 
    rename(pm25 = 2)
  
  label <- meta[["label"]]
  
  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")
  
  # Converting Timezones if necessary.
  if (timezone != "UTC") {
    covid_measures <- covid_measures %>%
      mutate(
        start_date = lubridate::with_tz(start_date, tzone = timezone),
        end_date = lubridate::with_tz(end_date, tzone = timezone)
      )
  }
  
  for (i in 1:nrow(covid_measures)) {
    # Creates a binary logical column for each response measure, whether it was in-place or not for an hourly measurement.
    measure <- covid_measures[["response_measure"]][[i]]
    data[[measure]] <- data$datetime %within% covid_measures[["interval"]][[i]]
  }
  
  data <- data %>%
    dplyr::mutate(
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Extracting hour.
      hour = format(datetime, "%H"),
      # Workweek 1 or 0.
      workweek = factor(if_else(weekday %in% c("Monday", "Tuesday",
                                               "Wednesday", "Thursday",
                                               "Friday"),
                                "workweek",
                                "weekend")),
      # Sums across the COVID measures columns and creates a new column, covid_measures with the sum total.
      covid_measures = dplyr::select(., any_of(covid_measures[["response_measure"]])) %>%
        rowSums(),
      # Using covid_measures column, if 0 (no measures in place), then false, else TRUE (measure/s in place).
      in_place = factor(
        dplyr::if_else(covid_measures == 0, TRUE, FALSE),
        levels = c(TRUE, FALSE),
        labels  = c("No Response Measures", "Response Measures in Place")
        )
    )
  
  # Groups data depending on facet_workweek value.
  if(facet_workweek) {
    data <- data %>%
      dplyr::group_by(hour, workweek, in_place)
  } else {
    data <- data %>%
      dplyr::group_by(hour, in_place)
  }
  
  data <- data %>%
    dplyr::summarize(
      pm_median = median(pm25, na.rm=TRUE),
      pm_mean = mean(pm25, na.rm=TRUE),
      pm_min = min(pm25, na.rm = TRUE),
      pm_max = max(pm25, na.rm = TRUE),
      pm_25q = quantile(pm25, probs = .25, na.rm = TRUE),
      pm_75q = quantile(pm25, probs = .75, na.rm = TRUE),
      N = n()
    )
  
  title <- paste(
    "Purple Air Sensor Readings Hour of Day",
    "Averages Pre and During COVID-19 Measures",
    sep = " ")
  
  caption <- paste("Bars represent 25th and 75th Percentiles",
                   "Point represents Hourly Median",
                   "Number of Hourly Measurements are above each Bar",
                   sep = "\n")
  
  plot <- data %>%
    ggplot2::ggplot(aes(x = hour)) +
    ggplot2::geom_crossbar(aes(y = pm_median, ymin = pm_25q, ymax = pm_75q,
                               color = in_place, fill = in_place),
                           position = "dodge", alpha = 0.6) +
    ggplot2::geom_text(aes(label = N, y = pm_75q, colour = in_place),
                       position = ggplot2::position_dodge(width=0.9),
                       vjust=-0.25) +
    ggplot2::labs(
      title = title,
      subtitle = label,
      caption = caption,
      x = paste("Hour of Day ", "(", timezone, ")", sep = ""),
      y = "PM25 µg/m3",
      color = "COVID-19 Response Measures in Place",
      fill = "COVID-19 Response Measures in Place"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11),
                   plot.caption = ggplot2::element_text(size = 15, vjust = 4, hjust = 0.5), legend.position = "bottom")

  if (facet_workweek) {
    plot <- plot + ggplot2::facet_grid(workweek~.)
  }
  
  
  return(plot)
}
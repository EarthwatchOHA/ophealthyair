covid_measures_hourlyavg_Plotlist <- function(
  outliercount_list,
  sensor_catalog,
  partner_site,
  covid_path = NULL,
  facet_workweek = FALSE 
) {
# Outputs a named list of plots for each element of outliercount_list.
# Plots are error bar plots showing median, first, and third quantiles of hourly averages for pre and post COVID-19 measures applicable to the site and program region.
# Plots also include a table of the COVID measures and dates that measures are in place.  
  
  sensors <- sensor_catalog %>%
    dplyr::filter(site == partner_site,
                  label %in% names(outliercount_list))
  
  data <- outliercount_list %>%
    dplyr::bind_rows(.id = "label") %>% 
    dplyr::mutate(
      # Extracting hour.
      hour = format(datetime, "%H"),
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Workweek 1 or 0.
      workweek = factor(if_else(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "workweek", "weekend"))
    )
  
  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")
  
  # Getting Covid Measures
  covid_measures <- load_covid_measures() %>%
    # Filters covid_measures so either site or program matches sensor catalog.
    dplyr::filter(site == partner_site | site %in% sensors$Program) %>% 
    # Creates interval out of start and end dates for measures.
    dplyr::mutate(interval = lubridate::interval(start = start_date, end = end_date))
  
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
      # Sums across the COVID measures columns and creates a new column, covid_measures with the sum total.
      covid_measures = dplyr::select(., any_of(covid_measures[["response_measure"]])) %>% rowSums(),
      # Using covid_measures column, if 0 (no measures in place), then false, else TRUE (measure/s in place).
      in_place = factor(ifelse(covid_measures == 0, FALSE, TRUE), labels = c("No Response Measures", "Response Measures in Place"))
      )
  
  # Groups data depending on facet_workweek value.
  if(facet_workweek) {
    data <- data %>%
      dplyr::group_by(hour, workweek, in_place, label)
  } else {
    data <- data %>% 
      dplyr::group_by(hour, in_place, label)
  }
  
  data <- data %>% 
    dplyr::summarize(
      pm_median = median(pm25, na.rm=TRUE),
      pm_mean = mean(pm25, na.rm=TRUE),
      pm_min = min(pm25),
      pm_max = max(pm25),
      pm_25q = quantile(pm25, probs = .25),
      pm_75q = quantile(pm25, probs = .75),
      N = n()
    )
  
  plots_list <- list()
  
  for (i in 1:nrow(sensors)) {
    sensor <- sensors[["label"]][[i]]
    plots_list[[sensor]] <- data %>%
      dplyr::filter(label == sensor) %>% 
      ggplot2::ggplot(aes(x = hour)) +
      ggplot2::geom_point(aes(y = pm_median, color = in_place, fill = in_place),
                 size = 3, shape = 23, position = "dodge") +
      ggplot2::geom_crossbar(aes(y = pm_median, ymin = pm_25q, ymax = pm_75q,
                        color = in_place, fill = in_place), 
                    position = "dodge", alpha = 0.6) +
      ggplot2::geom_text(aes(label = N, y = pm_75q, colour = in_place), position = ggplot2::position_dodge(width=0.9), vjust=-0.25) +
      ggplot2::labs(
        title = "Purple Air Sensor Readings Hour of Day Averages Pre and Post COVID-19 Measures",
        subtitle = sensor,
        caption = paste("Bars represent 1st and 3rd Quartile",
                        "Point represents Hourly Median",
                        "Number of Hourly Measurements are above each Bar",
                        sep = "\n"),
        x = paste("Hour of Day ", "(", timezone, ")", sep = ""),
        y = "PM25 µg/m3",
        color = "COVID-19 Response Measures in Place",
        fill = "COVID-19 Response Measures in Place"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11),
                     plot.caption = ggplot2::element_text(size = 15, vjust = 4, hjust = 0.5), legend.position = "bottom")
  }
  
  if (facet_workweek) {
   plots_list <- purrr::map(plots_list, .f = function(plt) plt + ggplot2::facet_grid(workweek~.))
  }
  
  covid_table <- gridExtra::tableGrob(dplyr::select(covid_measures, 
                                                    "Response Measures" = response_measure,
                                                    "Start" = start_date, "End" = end_date),
                                      rows=NULL)

  grobs_list <- purrr::map(.x = plots_list,
                     .f = function(plt) gridExtra::arrangeGrob(plt, covid_table,
                                                               layout_matrix = rbind(c(1, 1, 2),
                                                                                     c(1, 1, NA))))
  
  return(grobs_list)
}

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
#' @param sensor An AirSensor object.
#' @param aqi_country Character vector ISO 3166-1 alpha-2 country code of
#'   country who's Air Quality Index (AQI) should be used for plotting
#'   background. Current options are United States ("US") or India ("IN").

#' @param aqi_bar_alpha Numeric between 0 and 1 indicating the opacity of the
#'   AQI background colors. Default is 0.2.
#'
#' @return A ggplot object.
#'
#' @references
#' Cite ggplot2


hourlyavg_pmPlot <- function(
  sensor,
  aqi_country,
  point_color = "black",
  point_size = 2,
  point_alpha = 0.4,
  aqi_bar_alpha = 0.2,
  time_facet = "month",
  free_y = FALSE
) {

  time_facet_opts <- c(
    "month",
    "year"
  )

  if ( !is.null(time_facet) ) {
    if ( !(time_facet %in% time_facet_opts) ) {
      stop("time_facet must be 'month', 'year', or NULL.")
    }

  }

  # Check if is Airsensor object.
  if ( !(sensor_isSensor(sensor)) ) {
    stop("sensor_list must be an AirSensor object.")
  }

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

  plots_list <- list()

  if ( is.null(time_facet) ) {

    xmin <- min(data$datetime)
    xmax <- max(data$datetime)


    plots_list[[1]] <-
      data %>%
      ggplot2::ggplot(aes(x = datetime)) +
      ggplot2::geom_point(aes(y = pm25),
                          size = point_size,
                          alpha = point_alpha,
                          color = point_color) +
      ggplot2::scale_x_datetime() +
      ggplot2::annotate(
        geom = "rect",
        ymin = aqi_info$aqi_pm_mins,
        ymax = aqi_info$aqi_pm_maxs,
        xmin = xmin,
        xmax = xmax,
        alpha = aqi_bar_alpha,
        fill = aqi_info$colors) +
      ggplot2::labs(
        x = x_label,
        y = "PM 2.5 μg/m3",
        title = "Hourly Average Particulate Matter 2.5 Concentration",
        subtitle = label
      )



  } else {

    years_w_data <- unique(data$year)

    for (i in 1:length(years_w_data) ) {

      year <- as.character(years_w_data[i])

      year_data <- dplyr::filter(data, year == !!year)

      if ( time_facet == "month" ) {

        months_w_data <- unique(year_data$month)

        for (j in 1:length(months_w_data)) {

          month <- as.character(months_w_data[j])

          month_data <- dplyr::filter(year_data, month == !!month)

          xmin <- min(month_data$datetime)
          xmax <- max(month_data$datetime)

          plots_list[[paste(month, year, sep = "_")]] <-
            month_data %>%
            ggplot2::ggplot(aes(x = datetime)) +
              ggplot2::geom_point(aes(y = pm25),
                                  size = point_size,
                                  alpha = point_alpha,
                                  color = point_color) +
              ggplot2::scale_x_datetime() +
              ggplot2::annotate(
                geom = "rect",
                ymin = aqi_info$aqi_pm_mins,
                ymax = aqi_info$aqi_pm_maxs,
                xmin = xmin,
                xmax = xmax,
                alpha = aqi_bar_alpha,
                fill = aqi_info$colors) +
              ggplot2::labs(
                x = x_label,
                y = "PM 2.5 μg/m3",
                title = "Hourly Average Particulate Matter 2.5 Concentration",
                subtitle = paste(label, month, year, sep = ", ")
              )
        }

      }

      if ( time_facet == "year" ) {

        xmin <- min(year_data$datetime)
        xmax <- max(year_data$datetime)

        plots_list[[year]] <-
          year_data %>%
          ggplot2::ggplot(aes(x = datetime)) +
          ggplot2::geom_point(aes(y = pm25),
                              size = point_size,
                              alpha = point_alpha,
                              color = point_color) +
          ggplot2::scale_x_datetime() +
          ggplot2::annotate(
            geom = "rect",
            ymin = aqi_info$aqi_pm_mins,
            ymax = aqi_info$aqi_pm_maxs,
            xmin = xmin,
            xmax = xmax,
            alpha = aqi_bar_alpha,
            fill = aqi_info$colors) +
          ggplot2::labs(
            x = x_label,
            y = "PM 2.5 μg/m3",
            title = "Hourly Average Particulate Matter 2.5 Concentration",
            subtitle = paste(label, year, sep = ", ")
          )

      }
    }
  }
  if ( free_y ) {

    plot_get <- "data"

    if( !is.null(time_facet) ) {
      paste(time_facet, plot_get, sep = "_")
    }


    ymin <- min(get(plot_get)[["pm25"]], na.rm = TRUE)
    ymax <- max(get(plot_get)[["pm25"]], na.rm = TRUE)

    plots_list <- purrr::map(plots_list,
                             .f = function(plot) plot +
                               ggplot2::coord_cartesian(ylim = c(ymin, ymax))
                             )

  }

  return(plots_list)
}


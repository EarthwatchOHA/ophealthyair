#' @title 
#' 
#' \code{make_site_dataviz_pkg} returns character string of absolute path of the datavisualization package directory.
#' 
#' @description 
#' 
#' @param site
#' @param aggstats_list
#' @param sensor_aqi_list
#' @param sensor_catalog
#' @param aqi_country
#' @param output_directory
#' @param use_aqi
#' @param timezone
#' @param facet_covid_workweek
#' 
#' @return
#' 
#' @examples 


make_site_dataviz_pkg <- function(
    site,
    aggstats_list,
    sensor_list,
    sensor_catalog,
    aqi_country,
    output_directory = "outputs/data-viz-pkgs/",
    use_aqi = FALSE,
    timezone = NULL,
    facet_covid_workweek = FALSE
) {
  # Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
  # TODO: Error control system for site in Sensor Catalog.
  # TODO: Refactor.
  # TODO: Add additional function arguments as ... argument.
  
  # Making folder for site_deliverable
  output_dir <- output_directory
  site4file <- site %>% tolower() %>% stringr::str_replace_all("[',]*", "") %>% stringr::str_replace_all(" ", "-")
  dirname <- paste(lubridate::today(), site4file, sep = "-")
  dir_path <- paste(output_dir, dirname, sep = "")
  # Creating directory directories.
  dir.create(dir_path)
  
  # Prepping AQI Data for Workbook.
  # Converts sensor_list to aqi_list.
  if(use_aqi) {
    sensor_list <- purrr::map(.x = sensor_list, .f = function(x) PWFSLSmoke::monitor_aqi(x) %>% PWFSLSmoke::monitor_extractData())
  } else {
    sensor_list <- purrr::map(.x = sensor_list, .f = function(x) PWFSLSmoke::monitor_extractData(x))
  }
  
  # Converting Timezone if timezone given, and valid.
  if(!is.null(timezone) && timezone %in% OlsonNames()) {
    aggstats_list <- aggstats_list %>% 
      purrr::map(.f = function(x) mutate(x, 
                                         datetime = lubridate::with_tz(time = datetime,
                                                                       tzone = timezone)))
    sensor_list <- sensor_list %>% 
      purrr::map(.f = function(x) mutate(x,
                                         datetime = lubridate::with_tz(time = datetime,
                                                                       tzone = timezone)))
  }
  #--------------------------------------------------------------------------------------------------------
  # Creating workbook:
  
  # Instantiates and fills workbook object.
  wb <- make_PA_wkbk(aggstats_list = aggstats_list,
                     sensor_catalog = sensor_catalog, 
                     aqi_data = sensor_list,
                     use_aqi = use_aqi)
  
  #--------------------------------------------------------------------------------------------------------
    # Making palette of colors to use across for sensors
  sensor_colors <- RColorBrewer::brewer.pal(n = length(names(aggstats_list)),
                                            name = "Dark2")
  # Naming the vector with sensor labels.
  names(sensor_colors) <- names(aggstats_list)
  
  # Generating Plots:
  #--------------------------------------------------------------------------------------------------------
  # Calendar Plots:
  calendar_plots_list <- purrr::map2(.x = aggstats_list,
                                     .y = names(aggstats_list),
                                     .f = function(x,y, aqi_country) select(x, date = datetime, pm25) %>%
                                       .calendar_pmPlot(sensor_name = y, aqi_country = aqi_country),
                                     aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 2
  workweek_plot <- workweek_weeknd_pmPlot(aggstats_list = aggstats_list,
                                          sensor_colors = sensor_colors,
                                          aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 3
  dayplots <- day_of_week_pmPlotlist(aggstats_list = aggstats_list,
                                     sensor_colors = sensor_colors,
                                     aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Preparing COVID Measures Dataframe.
  sensors <- dplyr::filter(sensor_catalog, 
                           label %in% names(aggstats_list))
  
  # Cannot use site == site in filtering, so:
  partner_site <- site
  
  # Getting Covid Measures
  covid_measures <- load_covid_measures() %>%
    # Creates interval out of start and end dates for measures.
    dplyr::mutate(interval = lubridate::interval(start = start_date,
                                                 end = end_date)) %>% 
    # Filters covid_measures so either site or program matches sensor catalog.
    dplyr::filter(site == partner_site | site %in% sensors$Program)
  
  # COVID Plots
  covid_hourlyavg_plots_list <- covid_measures_hourlyavg_Plotlist(aggstats_list = aggstats_list, 
                                                                  covid_measures = covid_measures,
                                                                  facet_workweek = facet_covid_workweek)
  
  #--------------------------------------------------------------------------------------------------------
  # Preparing ppt object.
  ppt <- make_viz_ppt(sensors = names(aggstats_list),
                      sensor_colors = sensor_colors,
                      sensor_catalog = sensor_catalog,
                      workweek_plot = workweek_plot,
                      dayplots_list = dayplots,
                      calendar_plots_list = calendar_plots_list)
  
  # Adding COVID Measures Slide
  # Creating flextable.
  covid_measures_flex <- covid_measures %>% 
    select(-interval) %>% 
    flextable::qflextable()
  # Adding slide. 
  ppt <- ppt %>% 
    officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(value = covid_measures_flex, location = officer::ph_location_fullsize())
  
  # Saving COVID Plots to ppt
  # COVID Measures Hourly Averages
  for (i in 1:length(covid_hourlyavg_plots_list)) {
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = covid_hourlyavg_plots_list[[i]]),
                                location = officer::ph_location_fullsize())
  }
  #--------------------------------------------------------------------------------------------------------
  # Saving objects to folder. 
  
  # Workbook
  filename_wkbk <- paste(dir_path, "aqworkbook.xlsx", sep = "/")
  openxlsx::saveWorkbook(wb, file = filename_wkbk)
  
  print(ppt, target = paste(dir_path, "visualizations.pptx", sep = "/"))
  
  files2zip <- dir(dir_path, full.names = TRUE)
  
  # Zipping File
  # TODO: Not zipping file. 
  zip::zipr(zipfile = paste(dir_path, ".zip", sep = ""), files = files2zip)
  
  return(dir_path)
  
}
#--------------------------------------------------------------------------------------------------------
.calendar_pmPlot <- function(data, sensor_name, aqi_country, data_thresh = 75) {
  # Loading AQI Info
  aqi_info <- load_aqi_info(country = aqi_country)
  
  # Generating plot.
  openair::calendarPlot(data,
                        pollutant = "pm25",
                        main = "Daily Average Particulate Matter 2.5",
                        xlab = sensor_name,
                        ylab = "Particulate Matter 2.5",
                        cols = aqi_info$colors, labels = aqi_info$names,
                        breaks = aqi_info$breaks_24, data.thresh = data_thresh)
}
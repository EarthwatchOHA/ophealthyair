#' @title 
#' 
#' \code{make_site_dataviz_pkg} returns character string of absolute path of the datavisualization package directory.
#' 
#' @description 
#' 
#' @param site
#' @param aggstats_list
#' @param sensor_list
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
    aqi_country,
    output_directory = "outputs/data-viz-pkgs/",
    include_aqi_data = FALSE,
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
  
  sensor_catalog <- load_SensorCatalog()
  
  sensors <- dplyr::filter(sensor_catalog, 
                           label %in% names(aggstats_list))
  
  # Prepping AQI Data for Workbook.
  # Converts sensor_list to aqi_list.
  if( include_aqi_data ) {
    aqi_data <- purrr::map(.x = sensor_list,
                           .f = function(x) PWFSLSmoke::monitor_aqi(x) %>%
                             sensor_extractData())
  } else {
    aqi_data <- NULL
  }
  
  # Converting Timezone if timezone given, and valid.
  if(!is.null(timezone) && timezone %in% OlsonNames()) {
    aggstats_list <- aggstats_list %>% 
      purrr::map(.f = function(x) mutate(x, 
                                         datetime = lubridate::with_tz(time = datetime,
                                                                       tzone = timezone)))
    sensor_list <- purrr::map(.x = sensor_list,
                              .f = function(x) sensor_convertTZ(x, to = timezone))
  }
  #--------------------------------------------------------------------------------------------------------
  # Creating workbook:
  
  # Instantiates and fills workbook object.
  wb <- make_PA_wkbk(aggstats_list = aggstats_list,
                     sensor_catalog = sensor_catalog, 
                     aqi_data = aqi_data)
  
  #--------------------------------------------------------------------------------------------------------
  # Making palette of colors to use across for sensors
  sensor_colors <- RColorBrewer::brewer.pal(n = length(names(aggstats_list)),
                                            name = "Dark2")
  # Naming the vector with sensor labels.
  names(sensor_colors) <- names(aggstats_list)
  
  # Generating Plots:
  visualizations <- vector("character")
  #------------------------------------------------------------------------------
  # Sensor Metadata FlexTable
  meta_colnames <- c(
    "label",
    "Indoor/Outdoor",
    "First Measurement",
    "Most Recent Measurement",
    "Location Description",
    "Latitude (decimal degrees)",
    "Longitude (decimal degrees)",
    "Elevation (m)", 
    "Height from ground (m)",
    "Sensor Orientation (degrees and Cardinal Orientation)"
  )
  
  obs_range <- aggstats_list %>% 
    purrr::map(.f = function(x) dplyr::summarize(x, 
                                                 `First Measurement` = min(datetime),
                                                 `Most Recent Measurement` = max(datetime))) %>% 
    dplyr::bind_rows(.id = "label")
                
  
  sensor_meta_flex <- sensors %>% 
    dplyr::left_join(obs_range, by = "label") %>%  
    dplyr::select(one_of(meta_colnames)) %>% 
    flextable::flextable()
  
  for (i in 1:nrow(sensors)) {
    sensor <- sensors[[i]]
    sensor_meta_flex <- flextable::bg(x = sensor_meta_flex, i = i,
                                      bg = sensor_colors[[i]])
  }
  
  # Adding to viz
  visualizations <- append(visualizations, "sensor_meta_flex")
  #--------------------------------------------------------------------------------------------------------
  # Calendar Plots:
  #--------------------------------------------------------------------------------------------------------
  .calendar_pmPlot <- function(data, sensor_name, aqi_country, data_thresh = 75) {
    # Loading AQI Info
    aqi_info <- load_aqi_info(country = aqi_country)
    
    # Generating plot.
    plot <- openair::calendarPlot(data,
                          pollutant = "pm25",
                          main = "Daily Average Particulate Matter 2.5",
                          xlab = sensor_name,
                          ylab = "Particulate Matter 2.5",
                          cols = aqi_info$colors, labels = aqi_info$names,
                          breaks = aqi_info$breaks_24, data.thresh = data_thresh)
    invisible(plot)
  }
  
  calendarPlots_list <- purrr::map2(.x = sensor_list,
                                    .y = names(sensor_list),
                                    .f = function(x,y, aqi_country) sensor_extractData(x) %>% 
                                      select(date = datetime, pm25 = 2) %>%
                                       .calendar_pmPlot(sensor_name = y,
                                                        aqi_country = aqi_country),
                                    aqi_country = aqi_country)
  
  if( length(calendarPlots_list) > 0 ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "calendarPlots_list")
  
  }
  #--------------------------------------------------------------------------------------------------------
  # Workweek/Weekend Plot
  workweek_plot <- workweek_weeknd_pmPlot(aggstats_list = aggstats_list,
                                          sensor_colors = sensor_colors,
                                          aqi_country = aqi_country)
  
  if( is.ggplot(workweek_plot) ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "workweek_plot")
    
  }
  
  #--------------------------------------------------------------------------------------------------------
  # Day of Week List of Hour of Day Averages
  day_plots <- day_of_week_pmPlotlist(sensor_list = sensor_list,
                                      aqi_country = aqi_country,
                                      sensor_colors = sensor_colors,
                                      sd_ribbon = 2)
    
  if( length(day_plots) > 0 ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "day_plots")
    
  }
  #--------------------------------------------------------------------------------------------------------  
    
  safe_pollutionRose_dash <- 
    purrr::safely(.f = function(x) sensor_pollutionRose_dash(x,
                                                             aqi_country = "US", 
                                                             include_map = FALSE, 
                                                             max_noaa_dist = 3), 
                  otherwise = NA,
                  quiet = FALSE)  
  
  rose_list <- purrr::map(.x = sensor_list,
                          .f = safe_pollutionRose_dash)
  
  
  rose_list <- rose_list[purrr::map_lgl(rose_list,
                                        .f = function(x) !is.na(x$result))]
  
  if( length(rose_list) > 0 ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "rose_list")
    
  }
  
  if ( length(rose_list) < length(sensor_list) ) {
    names(sensor_list)[!names(sensor_list) %in% names(rose_list)]
    print(paste("No rose plots generated for:",
                names(sensor_list)[!names(sensor_list) %in% names(rose_list)])
          )
  }
  #--------------------------------------------------------------------------------------------------------  
  # Stacked Day of Week AQI Bar Charts
  day_bar_plots <- purrr::map(.x = sensor_list, .f = day_of_week_aqiBar,
                                    aqi_country = aqi_country, position = "stack")
  
  if( length(day_bar_plots ) > 0 ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "day_bar_plots")
  
  }  
  #--------------------------------------------------------------------------------------------------------
  # Preparing COVID Measures Dataframe.
  
  # Getting Covid Measures
  covid_measures <- load_covid_measures() %>%
    # Creates interval out of start and end dates for measures.
    dplyr::mutate(interval = lubridate::interval(start = start_date,
                                                 end = end_date)) %>% 
    # Filters covid_measures so either site or program matches sensor catalog.
    dplyr::filter(site == !!site | site %in% sensors$Program)
 
  # Creating flextable.
  covid_measures_flex <- covid_measures %>% 
    select(-interval) %>% 
    flextable::qflextable()
  
  # Adding to viz
  visualizations <- append(visualizations, "covid_measures_flex")
  
  # COVID Plots
  covid_plots <- covid_measures_hourlyavg_Plotlist(aggstats_list = aggstats_list, 
                                                   covid_measures = covid_measures,
                                                   facet_workweek = facet_covid_workweek)
  
  if( length(covid_plots) > 0 ) {
    # Adding to viz
    visualizations <- append(x = visualizations, "covid_plots")
    
  }
  
  #--------------------------------------------------------------------------------------------------------
  # Preparing ppt object.
  ppt <- officer::read_pptx()
  
  viz_ppt <- make_viz_ppt(mget(visualizations), ppt = ppt)

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


make_site_dataviz_pkg <- function(
    site,
    aqi_country,
    outliercount_list,
    sensor_aqi_list,
    sensor_catalog,
    output_directory = "outputs/data-viz-pkgs/",
    use_aqi = FALSE,
    timezone = NULL
) {
  # Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
  # TODO: Error control system for site in Sensor Catalog.
  
  # Making folder for site_deliverable
  output_dir <- output_directory
  site4file <- site %>% tolower() %>% stringr::str_replace_all("[',]*", "") %>% stringr::str_replace_all(" ", "-")
  dirname <- paste(lubridate::today(), site4file, sep = "-")
  dir_path <- paste(output_dir, dirname, sep = "")
  # Creating directory directories.
  dir.create(dir_path)
  
  # Prepping AQI Data for Workbook.
  # Extracts data from sensor_aqi_list.
  aqi_data <- purrr::map(.x = sensor_aqi_list, .f = function(x) x$data)
  
  # Converting Timezone if timezone given, and valid.
  if(!is.null(timezone) && timezone %in% OlsonNames()) {
    outliercount_list <- outliercount_list %>% 
      purrr::map(.f = function(x) mutate(x, 
                                         datetime = lubridate::with_tz(time = datetime,
                                                                       tzone = timezone)))
    aqi_data <- aqi_data %>% 
      purrr::map(.f = function(x) mutate(x,
                                         datetime = lubridate::with_tz(time = datetime,
                                                                       tzone = timezone)))
  }
  #--------------------------------------------------------------------------------------------------------
  # Creating workbook:
  
  # Instantiates and fills workbook object.
  wb <- make_PA_wkbk(outliercount_list = outliercount_list, sensor_catalog = sensor_catalog, 
                       aqi_data = aqi_data, use_aqi = use_aqi)
  
  #--------------------------------------------------------------------------------------------------------
  # Getting unique sensors.
  sensors <- names(outliercount_list)
  # Making palette of colors to use across for sensors
  sensor_colors <- RColorBrewer::brewer.pal(n = length(sensors), name = "Dark2")
  # Naming the vector with sensors.
  names(sensor_colors) <- sensors
  
  # Generating Plots:
  #--------------------------------------------------------------------------------------------------------
  # Calendar Plots:
  calendar_plots_list <- purrr::map2(.x = outliercount_list, .y = names(outliercount_list),
                                     .f = function(x,y, aqi_country) select(x, date = datetime, pm25) %>%
                                       .calendar_pmPlot(sensor_name = y, aqi_country = aqi_country),
                                     aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 2
  plot2 <- workweek_weeknd_pmPlot(outliercount_list = outliercount_list, sensor_colors = sensor_colors,
                                  aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 3
  dayplots <- day_of_week_pmPlotlist(outliercount_list = outliercount_list, sensor_colors = sensor_colors,
                                     aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # COVID Plots
  covid_hourlyavg_plots_list <- covid_measures_hourlyavg_Plotlist(outliercount_list = outliercount_list,
                                                                   sensor_catalog = sensor_catalog,
                                                                   partner_site = site, facet_workweek = FALSE)
  
  #--------------------------------------------------------------------------------------------------------
  # Preparing ppt object.
  
  # First creating a table for the first slide to show sensor metadata.
  ppt_sensor_meta <- sensor_catalog %>% 
    dplyr::filter(label %in% sensors) %>% 
    dplyr::select("label", "Indoor/Outdoor", "Deploy Date", "Deploy Time", `Deploy Location Description`,
                  "Latitude (decimal degrees)", "Longitude (decimal degrees)", "Elevation (m)",
                  `Height from ground (m)`, "Sensor Orientation (degrees and Cardinal Orientation)") %>% 
    flextable::flextable()
  
  for (i in 1:length(sensors)) {
     sensor <- sensors[[i]]
     ppt_sensor_meta <- flextable::bg(x = ppt_sensor_meta, i = i, bg = sensor_colors[[i]])
  }
  
  ppt <- officer::read_pptx() %>%
    officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(value = ppt_sensor_meta, location = officer::ph_location_fullsize()) %>% 
    officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(rvg::dml(ggobj = plot2), location = officer::ph_location_fullsize())
  
  for (i in 1:length(dayplots)) {
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = dayplots[[i]]), location = officer::ph_location_fullsize())
  }
  
  for (i in 1:length(calendar_plots_list)) {
    filename <- paste(names(calendar_plots_list)[[1]], "calendar_plot.png", sep = "-")
    path <- paste("outputs", "graphics", filename, sep = "/")
    lattice::trellis.device(device="png", filename=path)
    print(calendar_plots_list[[i]])
    dev.off()
    
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>%
      officer::ph_with(value = officer::external_img(src = path), location = officer::ph_location_fullsize())
  }
  
  # Saving COVID Plots to ppt
  # COVID Measures Hourly Averages
  for (i in 1:length(covid_hourlyavg_plots_list)) {
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = ggpubr::as_ggplot(covid_hourlyavg_plots_list[[i]])),
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
make_site_dataviz_pkg <- function(
    site,
    aqi_country,
    outliercount_list,
    sensor_aqi_list,
    sensor_catalog,
    output_directory = "outputs/data-viz-pkgs/",
    use_aqi = FALSE
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
  
  
  #--------------------------------------------------------------------------------------------------------
  # Creating workbook:
  
  # Instantiates and fills workbook object.
  wb <- make_PA_wkbk(outliercount_list = outliercount_list, sensor_catalog = sensor_catalog, 
                       sensor_aqi_list = sensor_aqi_list, use_aqi = use_aqi)
  
  #--------------------------------------------------------------------------------------------------------
  # Generating Plots:
  # Prepping agg_list for visualizations.
  df_viz_prepped <- .outliercount_list_viz_prep(outliercount_list = outliercount_list)
  
  # Getting unique sensors.
  sensors <- unique(df_viz_prepped$sensor)
  # Making vector of colors to use for sensors
  sensor_colors <- RColorBrewer::brewer.pal(n = length(sensors), name = "Dark2")
  # Naming the vector with sensors.
  names(sensor_colors) <- sensors
  
  #--------------------------------------------------------------------------------------------------------
  # Calendar Plots:
  calendar_plots_list <- purrr::map2(.x = outliercount_list, .y = names(outliercount_list),
                              .f = function(x,y, aqi_country) select(x, date = datetime, pm25) %>%
                                .calendar_pmPlot(sensor_name = y, aqi_country = aqi_country),
                              aqi_country = aqi_country)
  
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 2
  plot2 <- workweek_weeknd_pmPlot(data = df_viz_prepped, sensor_colors = sensor_colors,
                                  aqi_country = aqi_country)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 3
  dayplots <- day_of_week_pmPlotlist(data = df_viz_prepped, sensor_colors = sensor_colors,
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
# Util Data Preparation Function.
.outliercount_list_viz_prep <- function(outliercount_list) {
  # Preparing preliminary datasets.
  # TODO: Refactor
  # Days during the workweek.
  workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  # Days of the weekend.
  weekend <- c("Saturday", "Sunday")
  
  # Readying plotting set.
  viz_prepped <- outliercount_list %>% 
    # Convert to df.
    bind_rows(.id = "sensor") %>%
    # Filter by minimum observation count of 20.
    filter(min_count >= 20,
           !is.na(pm25)) %>% 
    mutate(
      # Mean, mean maximum, mean minimum of A and B channels
      pm_channels_max = (pm25_A_max+pm25_B_max)/2,
      pm_channels_min = (pm25_A_min+pm25_B_min)/2,
      # Getting day of week for each hour.
      weekday = weekdays(datetime),
      # Extracting hour.
      hour = format(datetime, "%H"),
      # Workweek 1 or 0.
      workweek = factor(if_else(weekday %in% workweek, "workweek", "weekend"))) 
  
  return(viz_prepped)
}

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

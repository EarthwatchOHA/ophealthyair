make_site_dataviz_pkg <- function(
    site,
    agg_list,
    sensor_catalog,
    wkbk_aqi_col = TRUE
) {
  # Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
  # TODO: Error control system for site in Sensor Catalog.
  
  # Making folder for site_deliverable
  site4file <- site %>% tolower() %>% stringr::str_replace_all("[',]*", "") %>% stringr::str_replace_all(" ", "-")
  dirname <- paste(lubridate::today(), site4file, sep = "-")
  dir_path <- paste("outputs/data-viz-pkgs/", dirname, sep = "")
  # Creating directory directories.
  dir.create(dir_path)
  
  
  #--------------------------------------------------------------------------------------------------------
  # Creating workbook:
  
  # Instantiates and fills workbook object.
  wb <- create_PA_wkbk(agg_list = agg_list, sensor_catalog = sensor_catalog, aqi_col = wkbk_aqi_col)
  
  #--------------------------------------------------------------------------------------------------------
  # Generating Plots:
  
  # Prepping agg_list for visualizations.
  agg_df_viz_prepped <- agg_list_viz_prep(agg_list = agg_list)
  
  # Making vector of colors to use for sensors
  sensor_colors <- RColorBrewer::brewer.pal(n = unique(agg_df_viz_prepped$sensor), name = "Dark2")
  # Naming the vector with sensors.
  names(sensor_colors) <- unique(agg_df_viz_prepped$sensor)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 1
  plot1 <- site_hourlyavg_aqiPlot(data = agg_df_viz_prepped, sensor_colors = sensor_colors)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 2
  plot2 <- workweek_weeknd_aqiPlot(data = agg_df_viz_prepped, sensor_colors = sensor_colors)
  
  #--------------------------------------------------------------------------------------------------------
  # Plot 3
  plot3 <- day_of_week_aqiPlot(data = agg_df_viz_prepped, sensor_colors = sensor_colors)
  
  #--------------------------------------------------------------------------------------------------------
  # Saving objects to folder. 
  
  # Workbook
  filename_wkbk <- paste(dir_path, "aqworkbook.xlsx", sep = "/")
  openxlsx::saveWorkbook(wb, file = filename_wkbk)
  
  # Plot 1.
  filename_plt1 = paste(dir_path, "site_hourlyavg_aqiPlot.png", sep = "/")
  ggsave(filename = filename_plt1, plot = plot1, width = 8)
  # Plot 2. 
  filename_plt2 = paste(dir_path, "workweek-weekend-aqi-plot.png", sep = "/")
  ggsave(filename = filename_plt2, plot = plot2, width = 8)
  # Plot 3. 
  filename_plt3 = paste(dir_path, "day-plots.png", sep = "/")
  ggsave(filename = filename_plt3, plot = plot3, height = 15, width = 20)
  
  files2zip <- dir(dir_path, full.names = TRUE)
  
  # Zipping File
  # TODO: Not zipping file. 
  zip::zipr(zipfile = paste(dir_path, ".7z", sep = ""), files = files2zip)
  
  return(dir_path)
  
}
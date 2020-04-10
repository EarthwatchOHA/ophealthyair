#' @title 
#' 
#' @description 
#' 
#' @param sensors
#' @param sensor_catalog
#' @param workweek_plot
#' @param dayplots_list
#' @param calendar_plots_list
#'
#' @return a Powerpoint Object.
#' 
#' @example
#'
#'

make_viz_ppt <- function(
  sensors,
  sensor_colors,
  sensor_catalog,
  workweek_plot,
  dayplots_list,
  calendar_plots_list
) {
  
  # TODO: Add ... argument for plotting. Can use if list then for-loop if ggplot then add_slide?
  
  # Creating  ppt object.
  ppt <- officer::read_pptx()
  # Preparing slides.
  #------------------------------------------------------------------------------
  # Sensor Metadata FlexTable
  sensor_meta_flex <- sensor_catalog %>% 
    dplyr::filter(label %in% sensors) %>% 
    dplyr::select("label", "Indoor/Outdoor", "Deploy Date", "Deploy Time",
                  `Deploy Location Description`, "Latitude (decimal degrees)",
                  "Longitude (decimal degrees)", "Elevation (m)", 
                  `Height from ground (m)`,
                  "Sensor Orientation (degrees and Cardinal Orientation)") %>% 
    flextable::flextable()
  
  for (i in 1:length(sensors)) {
    sensor <- sensors[[i]]
    sensor_meta_flex <- flextable::bg(x = sensor_meta_flex, i = i,
                                     bg = sensor_colors[[i]])
  }
  # Adding to powerpoint
  ppt <- ppt %>%
    officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(value = sensor_meta_flex, location = officer::ph_location_fullsize())
  #------------------------------------------------------------------------------
  # Adding workweek/weekend plot
  ppt <- ppt %>% 
    officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    officer::ph_with(rvg::dml(ggobj = workweek_plot), location = officer::ph_location_fullsize())
  #------------------------------------------------------------------------------
  # Adding Day Plot Slides
  for (i in 1:length(dayplots_list)) {
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = dayplots_list[[i]]), location = officer::ph_location_fullsize())
  }
  
  #------------------------------------------------------------------------------
  # Adding Calendar Plot Slides
  for (i in 1:length(calendar_plots_list)) {
    # Saving to cache file as png files.
    path <- paste("outputs", "graphics", "cache", "calendar_plot.png", sep = "/")
    lattice::trellis.device(device="png", filename=path)
    print(calendar_plots_list[[i]])
    dev.off()
    # Loading png files into ppt slides.
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide", master = "Office Theme") %>%
      officer::ph_with(value = officer::external_img(src = path), location = officer::ph_location_fullsize())
  }
  return(ppt)
}
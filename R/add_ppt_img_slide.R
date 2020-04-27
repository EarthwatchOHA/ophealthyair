#' @title
#'
#' @description Create a powerpoint with slides for every graphical object
#'   inputted, in the order that they are given as arguments.
#'
#' @param graphic Objects of class "ggplot", "openair", "flextable", or a list
#'   containing only objects of these sort.
#' @param ppt an Officer Powerpoint  object.
#' 
#' @return an Officer Powerpoint object.
#'
#' @example
#'
#' 

add_ppt_img_slide <- function(
  ppt,
  graphic
) {
  
  # If ggplot:
  if ( class(graphic) == c("gg", "ggplot") ) {
    
    # Adding ggplot to powerpoint
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide",
                         master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = graphic),
                       location = officer::ph_location_fullsize())
  # If openair:
  } else if ( class(graphic) == "openair" ) {
      
      plot <- graphic$plot
      
      # Saving to cache file as png files.
      path <- paste("outputs", "graphics", "cache", "viz_ppt.png", sep = "/")
      
      lattice::trellis.device(device="png", filename=path)
      
      print(graphic)
      dev.off()
      # Loading png files into ppt slides.
      ppt <- ppt %>% 
        officer::add_slide(layout = "Title Slide",
                           master = "Office Theme") %>%
        officer::ph_with(value = officer::external_img(src = path),
                         location = officer::ph_location_fullsize())
    # If flextable:
  } else if ( class(graphic) == "flextable" ) {
      
      ppt <- ppt %>%
        officer::add_slide(layout = "Title Slide",
                           master = "Office Theme") %>% 
        officer::ph_with(value = graphic,
                         location = officer::ph_location_fullsize())
    
  } else {
      
      stop("graphic class not recognized. Only objects of class ggplot, openair,
           flextable accepted.")
    }
  
  return(ppt)
}

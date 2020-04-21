#' @title
#'
#' @description Create a powerpoint with slides for every graphical object
#'   inputted.
#'
#' @param ... Objects of class "ggplot", "openair", "flextable", or a list
#'   containing only objects of these sort.
#'
#' @return an Officer Powerpoint Object.
#'
#' @example
#'
#' 

make_viz_ppt <- function(
  ...
) {
  
  # Creating  ppt object.
  ppt <- officer::read_pptx()
  
#------------------------------------------------------------------------------
  
  dots <- list(...)
  
  for (i in 1:length(dots)) {
    if(class(dots[[i]]) == "list") {
      for(j in 1:length(dots[[i]])) {
        if(any(class(dots[[i]][[j]]) == c("gg", "ggplot"))) {
          plot <- dots[[i]][[j]]  
          ppt <- ppt %>% 
            officer::add_slide(layout = "Title Slide",
                               master = "Office Theme") %>%
            officer::ph_with(rvg::dml(ggobj = plot),
                             location = officer::ph_location_fullsize())
        
        } else if (class(dots[[i]][[j]]) == "openair") {
          plot <- dots[[i]][[j]][["plot"]]
          # Saving to cache file as png files.
          path <- paste("outputs", "graphics", "cache", "viz_ppt.png", sep = "/")
          
          lattice::trellis.device(device="png", filename=path)
          
          print(plot)
          dev.off()
          # Loading png files into ppt slides.
          ppt <- ppt %>% 
            officer::add_slide(layout = "Title Slide",
                               master = "Office Theme") %>%
            officer::ph_with(value = officer::external_img(src = path),
                             location = officer::ph_location_fullsize())
          
        } else if (class(dots[[i]][[j]] == "flextable")) {
          
          table <- dots[[i]][[j]]
          
          ppt <- ppt %>%
            officer::add_slide(layout = "Title Slide",
                               master = "Office Theme") %>% 
            officer::ph_with(value = table,
                             location = officer::ph_location_fullsize())  
        } else {
          stop("object class not recognized. Only objects of class ggplot, openair,
           flextable, or a list of those objects.")
        }
      }
    } else if(any(class(dots[[i]]) == c("gg", "ggplot"))) {
    
    plot <- dots[[i]]
      
    # Adding ggplot to powerpoint
    ppt <- ppt %>% 
      officer::add_slide(layout = "Title Slide",
                         master = "Office Theme") %>% 
      officer::ph_with(rvg::dml(ggobj = plot),
                       location = officer::ph_location_fullsize())
    
    } else if(class(dots[[i]]) == "openair") {
      plot <- dots[[i]][["plot"]]
      # Saving to cache file as png files.
      path <- paste("outputs", "graphics", "cache", "viz_ppt.png", sep = "/")
      
      lattice::trellis.device(device="png", filename=path)
      
      print(plot)
      dev.off()
      # Loading png files into ppt slides.
      ppt <- ppt %>% 
        officer::add_slide(layout = "Title Slide",
                           master = "Office Theme") %>%
        officer::ph_with(value = officer::external_img(src = path),
                         location = officer::ph_location_fullsize())
    
    } else if (class(dots[[i]]) == "flextable") {
      table <- dots[[i]]  
      ppt <- ppt %>%
        officer::add_slide(layout = "Title Slide",
                           master = "Office Theme") %>% 
        officer::ph_with(value = table,
                         location = officer::ph_location_fullsize())  
    
    } else {
      stop("object class not recognized. Only objects of class ggplot, openair,
           flextable, or a list of those objects.")
    }
  }
  return(ppt)
}
#' @title
#'
#' @description Create a powerpoint with slides for every graphical object
#'   inputted, in the order that they are given as arguments.
#'
#' @param x a named list of ggplot, flextable, or openair objects, or lists of
#'   those objects.
#' @param ppt an Officer Powerpoint  object.
#'
#' @return an Officer Powerpoint object.
#'
#' @example
#'
#' 

make_viz_ppt <- function(
  x,
  ppt
) {
  
#------------------------------------------------------------------------------
  
  for (i in 1:length(x)) {
    
    # If x is a list
    if ( is(x[[i]], "list") ) {
      
      # Add every element as a slide.
      for (j in 1:length(x[[i]])) {
        
        ppt <- add_ppt_img_slide(
          graphic = x[[i]][[j]],
          ppt = ppt
        )
      }
      
    } else {
    
      ppt <- add_ppt_img_slide(
        graphic = x[[i]],
        ppt = ppt
      )
    }
  }
  return(ppt)
}

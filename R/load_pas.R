#' @title
#'
#' \code{load_pas} returns the most recently saved Purple Air Synoptic (pas)
#' dataframe fetch_pas(). For more information on Purple Air Synoptic dataframe
#' see \code{vignette("pas_introduction", package = "AirSensor")}.
#' 
#' For internal use to load saved pas in data/.


load_pas <- function(path = "data/pas.rds") {
  # This function reads in the saved pas RDS object.
  pas <- readRDS(path)
  return(pas)
}
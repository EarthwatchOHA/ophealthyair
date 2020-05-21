#' @title Load most recent pat_list.
#'
#' \code{load_pat_list} returns a named list of Purple Air Time Series (pat)
#' objects, or character string of error message returned by curl request.
#'
#' @description Internal function used to quickly load data saved by
#'   fetch_pat_list. fetch_pat_list is run daily using Earthwatch Sensor catalog
#'   (see "scripts/ingest_all.R"). load_pat_list allows for data persistence and
#'   time-efficient loading of large object.
#'
#' @param path Location of .rds file of pat_list to be loaded. Default is
#'   "data/pat_list.rds"
#' @param partner_site
#'
#' @return

load_pat_list <- function(path = "data/pat_list.rds",
                          site = NULL) {
  pat_list <- readRDS(path)


  if ( !is.null(site) ) {

    sensor_catalog <- load_SensorCatalog()

    if (!(site %in% unique(sensor_catalog$site))) {
      stop("site must be a valid deploy site in the Sensor Catalog.")
    }

    site_sensors <- sensor_catalog %>%
      dplyr::filter(site == !!site)

    pat_list <- pat_list[site_sensors$label]

  }

  return(pat_list)

}
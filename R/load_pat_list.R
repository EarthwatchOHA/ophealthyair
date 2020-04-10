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

load_pat_list <- function(path = "data/pat_list.rds") {
  pat_list <- readRDS(path)
  return(pat_list)
}
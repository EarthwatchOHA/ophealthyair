fetch_pat_list <- function(
  pas, 
  sensor_labels,
  sensor_ids,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  output_path = "data/pat_list.rds") {
  
  # Creates list of pat objects from a pas object and vector of sensor labels and sensor ID's.
  # TODO: Add option for vectorized start and end dates.
  # TODO: Fix growing list. 
  
  if (!purrr::is_bare_vector(sensor_labels)) {
    sensor_labels <- unlist(sensor_labels)
  }
  if (!purrr::is_bare_vector(sensor_ids)) {
    sensor_ids <- unlist(sensor_ids)
  }
  if (length(startdate) != length(enddate)) {
    stop("startdate and enddate arguments must be of equal length.")
  }
  if (length(startdate) > 1 & length(startdate) != length(sensor_labels)) {
    stop("if passing a vector of startdates and enddates, length must of vectors must be equal to length of sensor_labels.")
  }
  
  # If start and enddate are null or singular values.
  if (length(startdate) <= 1) {
      suppressWarnings(expr = {
      pat_list <- purrr::map2(.x = sensor_labels, .y = sensor_ids, .f = get_pat, pas = pas,
                              startdate = startdate, enddate = enddate, timezone = timezone) %>% 
        rlang::set_names(sensor_labels)
    })
  }
  # If start and enddate are neither atomic or singular, vectorize over them.
  if (length(startdate) > 1) {
    suppressWarnings(expr = {
      pat_list <- purrr::pmap(.l = list(label = sensor_labels,
                                        id = sensor_ids,
                                        startdate = startdate,
                                        enddate = enddate), .f = get_pat, pas = pas, timezone = timezone) %>% 
        rlang::set_names(sensor_labels)
    })
  }
  if (is.null(output_path)) {
    return(pat_list)
  } else {
    saveRDS(pat_list, output_path)
    invisible(pat_list)
  }
}
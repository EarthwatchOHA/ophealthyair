fetch_pat_list <- function(
  pas, 
  sensor_labels,
  sensor_ids,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  output_path = "data/pat_list.rds") {
  
  # Creates list of pat objects from a pas object and vector of sensor labels and sensor ID's.
  # TODO: Add logging (package loggr).
  # TODO: Best filing and loading system (data persistence). 
  # TODO: Fix growing list. 
  
  if (!purrr::is_bare_vector(sensor_labels)) {
    sensor_labels <- unlist(sensor_labels)
  }
  if (!purrr::is_bare_vector(sensor_ids)) {
    sensor_ids <- unlist(sensor_ids)
  }
  
  suppressWarnings(expr = {
    pat_list <- purrr::map2(.x = sensor_labels, .y = sensor_ids, .f = get_pat, pas = pas,
                            startdate = startdate, enddate = enddate, timezone = timezone) %>% 
      rlang::set_names(sensor_labels)
    
    if (is.null(output_path)) {
      return(pat_list)
    } else {
      saveRDS(pat_list, output_path)
      invisible(pat_list)
    }
  })
}
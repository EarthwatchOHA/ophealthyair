get_pat <- function(label, id, pas,
                    startdate = NULL, enddate = NULL, timezone = NULL) {
  # Creates a new pat object, but intended to be used iteratively, with error and warning handlers.
  # TODO: Add loggr:: for error detection.
  
  tryCatch( 
    expr = {
      pat <- AirSensor::pat_createNew(pas = pas, id = id, label = label,
                                      startdate = startdate, enddate = enddate,
                                      timezone = timezone)
      print(paste(label, ": ingestion successful."))
      return(pat)
    },
    error = function(e) {
      message(paste(label, ":", e))
      return(paste(e))
    })
}

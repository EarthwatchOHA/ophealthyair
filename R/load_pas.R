
load_pas <- function(path = "data/pas.rds") {
  # This function reads in the saved pas RDS object.
  pas <- readRDS(path)
  return(pas)
}
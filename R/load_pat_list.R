

load_pat_list <- function(path = "data/pat_list.rds") {
  pat_list <- readRDS(path)
  return(pat_list)
}
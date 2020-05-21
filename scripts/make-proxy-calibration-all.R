# Make new proxy calibrations for all of our deploy sites.
devtools::load_all()

sensorCatalog <- fetch_SensorCatalog()

colloc_sites <- sensorCatalog %>% 
  dplyr::filter(Collocated) %>% 
  dplyr::pull(site) %>% 
  unique()

for (i in 1:length(colloc_sites)) {
  
  args <- list(
    site = colloc_sites[[i]],
    indicator = "R2",
    adjusted = TRUE
  )
  
  
  print(colloc_sites[[i]])
  source("scripts/make-proxy-calibration.R")

}

# Makes data visualization packages for all sites in Sensor Catalog.
devtools::load_all()

args <- list()

args$delete_uncompress <- "TRUE" 
args$output_dir <- "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/Citizen Science deployment/Data-Visualization-Packages/"
args$include_wind_map <- "FALSE"
args$max_wind_distance <- "5"
args$facet_covid_workweek <- "TRUE"

# Sites to exclude:
exclude <- c(
  "Undeployed",
  # TODO: Find way to solve this duplicate label issue.
  # For now simply filtering out US Embassy Delhi site.
  "US Embassy Delhi"
)

# Load sensor_catalog.
sensor_catalog <- load_SensorCatalog() %>% 
  dplyr::filter(
    # Removing exclude sites.
    !(site %in% exclude),
    # Removing NA values (When cells are blank, they sneak in).
    !is.na(site))

# Get site names.
sites <- unique(sensor_catalog$site)

for (i in 1:length(sites)) {
  
  args$site <- sites[i]
  print(args$site)
  
  try({
    suppressWarnings({
      source("scripts/make_site_data_viz_pkg-test-script.R")
    })
    print("Successful")
  })
}
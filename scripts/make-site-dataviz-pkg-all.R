# Makes data visualization packages for all sites in Sensor Catalog.
suppressMessages({
  devtools::load_all()
})

args <- list()

args$delete_uncompress <- "TRUE" 
args$output_dir <- "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/7.Data and Field Reports/2020/Data-Visualization-Packages/"
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
sensor_catalog <- fetch_SensorCatalog() %>% 
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
      source("scripts/make-site-dataviz-pkg.R")
    })
    print("Successful")
  })
}

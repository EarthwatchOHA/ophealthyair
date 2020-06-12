# Makes data visualization packages for all sites in Sensor Catalog.
suppressMessages({
  library(dplyr)
  library(pavisualizR)
  devtools::load_all()
})

# Rules for which Program uses which AQI scale
program_aqi <- c("Boston" = "US",
                 "Southern California" = "US",
                 "India" = "IN",
                 "Sri Lanka" = "US")

# Sites to exclude:
exclude <- c(
  "Undeployed",
  "US Embassy Delhi"
)

# Load sensor_catalog.
sites <- readRDS("data/sensor_catalog.rds") %>%
  filter(
    # Removing exclude sites.
    !(site %in% exclude),
    # Removing NA values (When cells are blank, they sneak in).
    !is.na(site)) %>%
  pull(site) %>%
  unique()

#------------------------------------------------------------------------------
# Create Argument Parser
if ( !exists("args", mode = "list") ) {

  suppressPackageStartupMessages(library("argparse"))
  # Instantiating parser.
  parser <- ArgumentParser(
    description = paste("Make data vizualization packages for sites:",
                        paste(sites, collapse = ";  "), sep = " ")
  )

  # Arguments
  parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-o", "--output_dir", type = "character",
                      default = "outputs/data-viz-pkgs",
                      help = "Directory for file output [default %(default)s]")

  parser$add_argument("-d", "--delete_uncompress", type = "logical",
                      default = FALSE,
                      help = paste(
                        "TRUE/FALSE; delete the unzipped version of",
                        "the directory [default %(default)s].", sep = " "
                      )
  )

  parser$add_argument("-f", "--facet_covid_workweek", type = "logical",
                      default = TRUE,
                      help = paste(
                        "TRUE/FALSE; facet COVID plots by workweek/weekend",
                        "[default %(default)s].", sep = " "
                      )
  )

  parser$add_argument("-c", "--calibrate", type = "logical",
                      default = FALSE,
                      help = paste(
                        "If TRUE, apply calibration from the nearest",
                        "model. Currently, sites in the Boston program",
                        "use the Von Hillern calibration. Sites in the",
                        "India and Sri Lanka programs use the Lodhi Road",
                        "calibration [default %(default)s].", sep = " "))

  args <- parser$parse_args()
}

#------------------------------------------------------------------------------
# Iterating through datavis pkgs for all sites.
for (i in 1:length(sites)) {

  args[["site"]] <- sites[i]
  print(args$site)

  program <- load_SensorCatalog(site = args[["site"]]) %>%
    pull(Program) %>% unique()

  args[["aqi_country"]] <- program_aqi[program] %>%
    countrycode::countrycode(origin = "iso2c",
                             destination = "country.name")
  print(paste("Using",
              args$aqi_country,
              "AQI scale.", sep = " "))

  try({
    suppressWarnings({
      source("scripts/make-site-dataviz-pkg.R")
    })
    print("Successful")
  })
}

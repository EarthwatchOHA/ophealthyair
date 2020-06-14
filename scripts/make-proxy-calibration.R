# Make Proxy Calibration Equations for Programs:
#------------------------------------------------------------------------------
# Env Setup
suppressMessages({
  library(dplyr)
  library(ropenaq)
  library(AirSensor)
  library(modelr)
  library(purrr)
  library(broom)
  devtools::load_all()
})
#------------------------------------------------------------------------------
# Command Line Interface
# TODO: Need to access AWS data further back than 90 days.
# TODO: Seasonal models
# TODO: Rename indicator.

# Define Options for Choice Arguments
mod_select_stat_opts <- c(
  "R2",
  "RMSE",
  "AIC"
)

# Load site sensor_catalog metadata.
site_opts <- load_SensorCatalog() %>%
  dplyr::filter(Collocated) %>%
  dplyr::pull(site) %>%
  unique()

if ( !exists("args", mode = "list") ) {
  suppressPackageStartupMessages(library("argparse"))

  # Instantiating parser.
  parser <- ArgumentParser(
    description = paste(
      "Generate proxy-calibration model from collocated sensors (according to",
      "Sensor Catalog) at a single Collocation Site. Select best model of",
      "collocated sensors using --indicator. Currently,  only 90 days of",
      "Reference Monitor data is available from the OpenAQ API, so models are",
      "based on the most recent 90 days of data.", sep = " ")
    )

  # Required Arguments
  parser$add_argument("site", action="store", choices = site_opts,
                      help = paste("Name of site to make calibration from",
                                   "Argument type: %(type)s",
                                   "[default %(default)s]", sep = " "))

  # Optional Arguments
  parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-i", "--indicator", action = "store",
                      choices = mod_select_stat_opts,
                      default = "R2",
                      help = paste("Statistic to select best model",
                                   "Argument type: %(type)s",
                                   "[default %(default)s]", sep = " "))

  parser$add_argument("-a", "--adjusted", action = "store_true",
                      help = paste("TRUE/FALSE; If indicator R2,",
                                   "adjust R2 [default %(default)s]",
                                   sep = " ")
                      )

  args <- parser$parse_args()
}

#------------------------------------------------------------------------------
# Input Control
if ( args$adjusted & !args$indicator == "R2" ) {
  stop("Adjusted can only be used if indicator is R2.")
}
#------------------------------------------------------------------------------
mod_select_stat <- args$indicator

# Load site sensor_catalog metadata.
sensor_catalog <- load_SensorCatalog(site = args$site)
# Load site pats.
pat_list <- load_pat_list(site = args$site)

# Get Collocation Site Name from Sensor Catalog
ref_site <- sensor_catalog %>%
  distinct(`Collocation Site`) %>%
  pull()

# Confirm ref_site is valid.
all_ref_sites <- aq_locations()

if ( !(ref_site %in% all_ref_sites$location) ) {
  stop("Reference Monitor site could not be found.")
}

# Site metadata.
ref_meta <- all_ref_sites %>%
  filter(location == ref_site)

print("Loading Reference Monitor data.")

# Get Collocation Site Reference Monitor Data
ref_data <- aq_measurements(location = ref_meta$locationURL, parameter = "pm25") %>%
  select(datetime = "dateUTC", pm25 = value) %>%
  filter(pm25 >= 0)

print("Applying QAQC to Purple Air data.")
source("scripts/qaqc.R")

# Loading AirSensor objects.
sensor_list <- readRDS("data/sensor_list.rds")
# Subset AirSensors with PM data.
pm_data <- sensor_list %>%
  map("pm25") %>%
  map(.f = function(x) dplyr::rename(x$data, pm25 = 2))
# Subset AirSensors with Temp data.
temp_data <- sensor_list %>%
  map("temperature") %>%
  map(.f = function(x) rename(x$data, temperature = 2))
# Subset AirSensors with Humidity data.
humidity_data <- sensor_list %>%
  map("humidity") %>%
  map(.f = function(x) rename(x$data, humidity = 2))

# Join PM Data to Reference Data
pm_data <- pm_data %>%
  map(.f = function(x) dplyr::inner_join(x = x,
                                         y = ref_data,
                                         by = "datetime",
                                         suffix = c("_pa", "_ref"))
  )

print("Generating calibration models.")
# Combining all sensor variables into single dataframes for modelling.
data <-
  map2(.x = pm_data, .y = temp_data,
                    .f = left_join, by = "datetime") %>%
  map2(.y = humidity_data,
              .f = left_join, by ="datetime") %>%
  # Filter complete cases.
  map(.f = function(x) x[complete.cases(x), ])

# Make Models
models <- data %>%
  map(
    .f = function(x) lm(pm25_ref ~ pm25_pa + temperature + humidity, data = x)
    )

mod_summary <-
  data.frame(
    label = names(models),
    p_value = map_dbl(.x = models, .f = extract_p),
    AIC = map_dbl(.x = models, .f = AIC),
    R2 = map_dbl(.x = models, .f = extract_R2, adjusted = args$adjusted),
    RMSE = map_dbl(.x = models,
                          ~ sqrt(mean((.x$residuals) ^ 2))
                          ),
    missing = map_dbl(.x = pm_data, .f = function(x) sum(is.na(x$pm25_pa))),
    row.names = NULL,
    stringsAsFactors = FALSE
    )

print(mod_summary)

if ( mod_select_stat == "R2") {
  best_mod <- mod_summary %>%
    top_n(n = 1, wt = !!as.symbol(mod_select_stat)) %>%
    pull(label)
} else if ( mod_select_stat == "AIC" | mod_select_stat == "RMSE") {
  best_mod <- mod_summary %>%
    top_n(n = -1, wt = !!as.symbol(mod_select_stat)) %>%
    pull(label)
}

calibration <- list()
calibration[["meta"]] <- ref_meta
calibration[["model"]] <- models[[best_mod]]

print(summary(calibration$model))
#-------------------------------------------------------------------------------
# Save to file:
deploy_site4file <- args$site %>%
  tolower() %>%
  # Remove all commas or apostophres.
  stringr::str_remove_all("[',]*") %>%
  # Replace all spaces with dashes.
  stringr::str_replace_all(" ", "-")

out_file <- paste(deploy_site4file, ".rds", sep = "")
out_path <- paste("data", "calibration-models", out_file, sep = "/")

saveRDS(calibration, out_path)

print(paste("Model saved to ", out_path, '.', sep = " "))

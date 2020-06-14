# Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
#------------------------------------------------------------------------------
# Env setup
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(AirSensor)
  library(pavisualizR)
  devtools::load_all()
})
#------------------------------------------------------------------------------
# Command Line Interface

# Define Options for Choice Arguments
calibration_opts <- list.files(path = "data/calibration-models") %>%
  stringr::str_remove(
    pattern = stringr::fixed(".rds", ignore_case = TRUE)
  ) %>%
  # Replace all dashes w spaces.
  stringr::str_replace_all("-", " ") %>%
  stringr::str_to_title()

aqi_country_opts <- pavisualizR::aqi_info %>%
  names() %>%
  countrycode::countrycode(origin = "iso2c",
                           destination = "country.name")

site_opts <- load_SensorCatalog() %>%
  filter(site != "Undeployed") %>%
  pull(site) %>% unique()

# Create Argument Parser
if ( !exists("args", mode = "list") ) {

  suppressPackageStartupMessages(library("argparse"))
  # Instantiating parser.
  parser <- ArgumentParser(
    description = "Make a site data vizualization package.",
    add_help = TRUE
    )

  # Positional Arguments
  parser$add_argument("site", type = "character",
                      choices = site_opts,
                      metavar = "",
                      help = paste(
                        "Deployment site (based on Sensor Catalog)",
                        "to make visualization package for.",
                        "Argument type: %(type)s",
                        "Options are:",
                        paste(site_opts, collapse = "; "), sep = " "))



  # Optional Arguments
  parser$add_argument("-v", "--verbose", action="store_true",
                      default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-o", "--output_dir", type = "character",
                      default = "outputs/data-viz-pkgs",
                      help = paste("Directory for file output",
                                   "Argument type: %(type)s.",
                                   "[default %(default)s]", sep = " "))

  parser$add_argument("-a", "--aqi_country", type = "character",
                      choices = aqi_country_opts,
                      default = "United States",
                      help = paste("What countries AQI should be used in",
                                   "visualizations.",
                                   "Argument type: %(type)s.",
                                   "Allowed options are:",
                                   paste(aqi_country_opts, collapse = ", "),
                                   "[default %(default)s].", sep = " ")
                      )

  parser$add_argument("-c", "--calibrate", type = "logical",
                      default = FALSE,
                      help = paste("TRUE/FALSE; if TRUE, must also supply",
                                   "--calibration_model",
                                   "Argument type: %(type)s.",
                                   "[default %(default)s].", sep = " ")
                      )

  parser$add_argument("-m", "--calibration_model",
                      type = "character",
                      metavar = "",
                      choices = calibration_opts,
                      help = paste(
                        "If --calibrate is TRUE, apply inputted calibration",
                        "to sensor data.",
                        "Argument type: %(type)s.",
                        "Allowable options are:",
                        paste(calibration_opts, collapse = " "), sep = " ")
                      )

  parser$add_argument("-d", "--delete_uncompress", type = "logical",
                      default = FALSE, metavar = "",
                      help = paste(
                        "TRUE/FALSE; delete the uncompressed (.zip)",
                        "version of the directory.",
                        "Argument type: %(type)s.",
                        "[default %(default)s].",
                        sep = " ")
                      )

  parser$add_argument("-f", "--facet_covid_workweek",
                      type = "logical",
                      default = TRUE,
                      metavar = "",
                      help = paste(
                        "TRUE/FALSE; facet COVID plots by workweek/weekend.",
                        "Argument type: %(type)s.",
                        "[default %(default)s].", sep = " "
                        )
                      )

  args <- parser$parse_args()
}

#------------------------------------------------------------------------------
# Input Control:
if ( args$calibrate & is.null(args$calibration_model) ) {
  stop("If --calibrate is TRUE, calibration model must be specified.")
}

#------------------------------------------------------------------------------
# Load Data
programs <- c("India" = "IN", "Sri Lanka" = "IN", "Southern California" = "US",
              "Boston" = "US")

sensor_catalog <- load_SensorCatalog(site = args$site)

pat_list <- load_pat_list(site = args$site)

#------------------------------------------------------------------------------
# Prepare Data
print("Applying QAQC to data.")
# QAQC data. See vignettes/qaqc_pt1.rmd for details.
suppressMessages({
  source("scripts/qaqc.R")
})
print("QAQC applied successfully.")

# Load cleaned data.
pat_list <- readRDS("data/pat_list_qcd.rds")
sensor_list <- readRDS("data/sensor_list.rds")

if ( args$calibrate ) {
  print("Calibrating data.")
  proxy_site <- args$calibration_model
  suppressMessages({
    source("scripts/calibrate.R")
  })
  # Load calibrated data.
  pat_list <- readRDS("data/pat_list_calibrated.rds")
  sensor_list <- readRDS("data/sensor_list_calibrated.rds")
  print("Data calibrated successfully.")
}

# Separate sensor_list into component measurements.
pm_list <- purrr::map(.x = sensor_list, "pm25")
temp_list <- purrr::map(.x = sensor_list, "temperature")
humidity_list <- purrr::map(.x = sensor_list, "humidity")

# Get program from site.
program <- sensor_catalog$Program[1]

# Get country code for visualization AQI parameters.
aqi_country <- args$aqi_country %>%
  countrycode::countrycode(origin = "country.name",
                           destination = "iso2c")

# Attempt to convert data timezone to local time.
print("Converting to local timezone.")
# Timezone set to NULL.
timezone <- NULL
# If possible extract timezone from pat_list meta.
for (i in 1:length(pat_list)) {
  try({
    timezone <- pat_extract_TZ(pat_list[[i]])
  })
}

# Convert Timezone if timezone given and valid.
if( !is.null(timezone) && timezone %in% OlsonNames() ) {
  pat_list <- purrr::map(.x = pat_list,
                         .f = function(x) pat_convertTZ(x,
                                                        to = timezone)
                         )

  pm_list <- purrr::map(.x = pm_list,
                        .f = function(x) sensor_convertTZ(x,
                                                          to = timezone)
                        )
  temp_list <- purrr::map(.x = temp_list,
                          .f = function(x) sensor_convertTZ(x,
                                                            to = timezone)
  )

  humidity_list <-purrr::map(.x = humidity_list,
                             .f = function(x) sensor_convertTZ(x,
                                                               to = timezone)
  )

}
print(paste0("Timezone converted to ", timezone, "."))

#------------------------------------------------------------------------------
# Create workbook:
print("Making PA data workbook.")
# Use aqi in make_site_dataviz_pkg adds a US AQI column to workbook.
if (aqi_country == "IN") {
  include_aqi_data <- FALSE
} else if( aqi_country == "US") {
  include_aqi_data <- TRUE
}

# Converts sensor_list to aqi_list.
if( include_aqi_data ) {
  aqi_list <- purrr::map(.x = pm_list,
                         .f = function(x) PWFSLSmoke::monitor_aqi(x) %>%
                           sensor_extractData())
} else {
  aqi_list <- NULL
}

# Instantiate and fill workbook object.
# sensor object, or sensor object and pat objects.
wb <- make_PA_wkbk(sensor_list = sensor_list,
                   sensor_catalog = sensor_catalog,
                   aqi_data = aqi_list)

print("Workbook successfully made.")
#------------------------------------------------------------------------------
# Vizualization Preparation:
print("Preparing for visualizations.")
# Make palette of colors to use across visualizations for sensors
sensor_colors <- RColorBrewer::brewer.pal(n = length(names(sensor_list)),
                                          name = "Dark2")

# Name the palette vector with sensor labels.
names(sensor_colors) <- names(sensor_list)

# Initialize character vector that will contain names of plots to be included in
# viz powerpoint.

visualizations <- vector("character")

#------------------------------------------------------------------------------
# Sensor Metadata FlexTable:
print("Making Sensor Metadata Table.")

meta_colnames <- c(
  "label",
  "Indoor/Outdoor",
  "First Measurement",
  "Most Recent Measurement",
  "Location Description",
  "Latitude (decimal degrees)",
  "Longitude (decimal degrees)",
  "Elevation (m)",
  "Height from ground (m)",
  "Sensor Orientation (degrees and Cardinal Orientation)"
)

obs_range <- pat_list %>%
  purrr::map(
    .f = function(x) x$data %>%
      dplyr::summarize(first = min(datetime),
                       last = max(datetime))
    ) %>%
  dplyr::bind_rows(.id = "label") %>%
  mutate(
    first = format(x = first, format = "%d-%b-%Y"),
    last = format(x = last, format = "%d-%b-%Y")
    ) %>%
  select(
    label,
    "First Measurement" = first,
    "Most Recent Measurement" = last
  )


sensor_meta_flex <- sensor_catalog %>%
  dplyr::left_join(obs_range, by = "label") %>%
  dplyr::select(one_of(meta_colnames)) %>%
  flextable::flextable()

for (i in 1:nrow(sensor_catalog)) {
  sensor <- sensor_catalog[["label"]][[i]]
  sensor_meta_flex <- flextable::bg(x = sensor_meta_flex, i = i,
                                    bg = sensor_colors[[i]])
}

# Adding to ppt.
visualizations <- append(visualizations, "sensor_meta_flex")

print("Sensor Metadata Table made")
#------------------------------------------------------------------------------
# Calendar Plots:
print("Making Calendar plots.")

# Loading AQI Info


# Generating plot.
calendarPlots_list <- purrr::imap(
  .x = pm_list,
  .f = function(x,y) sensor_extractData(x) %>%
    select(date = datetime, pm25 = 2) %>%
    openair::calendarPlot(
      pollutant = "pm25",
      main = "Daily Average Particulate Matter 2.5",
      xlab = y,
      ylab = "Particulate Matter 2.5",
      cols = pavisualizR::aqi_info[[aqi_country]][["colors"]],
      labels = pavisualizR::aqi_info[[aqi_country]][["names"]],
      breaks = pavisualizR::aqi_info[[aqi_country]][["breaks_24"]],
      data.thresh = 75
      )
  )

if( length(calendarPlots_list) > 0 ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "calendarPlots_list")
  print(
    paste0("Calendar plots added for ",
          paste(names(calendarPlots_list), collapse = ", "),
          ".")
    )
}
#------------------------------------------------------------------------------
# Workweek/Weekend Plot:
print("Making Workweek/Weekend plots.")

workweek_plot <- workweek_weeknd_pmPlot(sensor_list = pm_list,
                                        sensor_colors = sensor_colors,
                                        aqi_country = aqi_country)

if( is.ggplot(workweek_plot) ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "workweek_plot")

  print("Workweek/Weekend plot added.")
}


#------------------------------------------------------------------------------
# Day of Week AQI Bar Charts
print("Making Day of Week AQI Bar Charts.")

day_bar_plots <- purrr::map(.x = pm_list, .f = day_of_week_aqiBar,
                            aqi_country = aqi_country, position = "fill")

if( length(day_bar_plots ) > 0 ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "day_bar_plots")

  print(
    paste("Day of Week AQI Bar Charts added for",
          paste(names(day_bar_plots), collapse = ", "),
          ".", sep = " ")
  )
}

#------------------------------------------------------------------------------
# Monthly Faceted AQI Colored Points
print("Making Hourly Average plots.")

aqicolor_points_plots <- purrr::map(
  .x = pm_list,
  .f = function(x) sensor_hourlyavg_aqicolorsPlot(sensor = x,
                                                  aqi_country = aqi_country,
                                                  time_facet = "month",
                                                  point_size = 1.5,
                                                  point_alpha = 0.9)
)

if( length(aqicolor_points_plots ) > 0 ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "aqicolor_points_plots")

  print(
    paste0("Hourly Average plots added for ",
           paste(names(aqicolor_points_plots), collapse = ", "),
           ".")
  )
}

#------------------------------------------------------------------------------
# COVID Measures Pre and During Plot:
# TODO: Affix to covid-measures database.
# TODO: Update plot for before, during, after.

print("Making COVID-19 plots.")
# Prepare COVID Measures Dataframe.

# Load Covid Measures df.
covid_measures <- pavisualizR::covid_measures %>%
  # Create interval out of start and end dates for measures.
  dplyr::mutate(interval = lubridate::interval(start = start_date,
                                               end = end_date)) %>%
  # TODO: Add below code as arg to load_covid_measures.
  # Filter covid_measures so either site or program matches sensor catalog.
  dplyr::filter(site == args$site | site %in% sensor_catalog$Program)

# Create flextable.
covid_measures_flex <- covid_measures %>%
  select(-interval) %>%
  flextable::qflextable()

# Add to ppt.
visualizations <- append(visualizations, "covid_measures_flex")

# Make COVID Plots
covid_plots <-
  purrr::map(.x = pm_list,
             .f = covid_measuresPlot,
             facet_workweek = args$facet_covid_workweek)

if( length(covid_plots) > 0 ) {
  # Add to ppt.
  visualizations <- append(x = visualizations, "covid_plots")

  print(
    paste0("COVID-19 plots added for ",
           paste(names(covid_plots), collapse = ", "),
           ".")
  )
}

#------------------------------------------------------------------------------
# Create ppt object.
print("Making powerpoint.")

ppt <- officer::read_pptx()

viz_ppt <- make_viz_ppt(mget(visualizations), ppt = ppt)

print("Powerpoint made.")
#------------------------------------------------------------------------------
# Save objects to dir.
# Make folder for site_deliverable:
# Adjust site name to be in dir name.
site4file <- args$site %>%
  tolower() %>%
  # Remove all commas or apostophres.
  stringr::str_remove_all("[',]*") %>%
  # Replace all spaces with dashes.
  stringr::str_replace_all(" ", "-")

# Date directory name.
dirname <- paste(lubridate::today(), site4file, sep = "-")
dir_path <- paste(args$output_dir, dirname, sep = "/")

# Create directory.
dir.create(dir_path)

# Workbook
filename_wkbk <- paste(dir_path, "aqworkbook.xlsx", sep = "/")

openxlsx::saveWorkbook(wb, file = filename_wkbk)

# Powerpoint
print(ppt, target = paste(dir_path, "visualizations.pptx", sep = "/"))

files2zip <- dir(dir_path, full.names = TRUE)

# Zip File
zip::zipr(zipfile = paste(dir_path, ".zip", sep = ""), files = files2zip)

if ( args$delete_uncompress ) {
  # Delete uncompressed dir.
  unlink(dir_path, recursive = TRUE)
}

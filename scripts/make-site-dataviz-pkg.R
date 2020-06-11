# Outputs data delivery package for given site using  sensor_catalog, and previously generated pat_list.
# TODO: Suppress much of the output.
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
    description = "Make a site data vizualization package."
    )

  # Arguments
  parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-s", "--site", type = "character",
                      required = TRUE, choices = site_opts,
                      help = paste("Deployment site (based on Sensor Catalog)",
                                   "to make visualization package for.",
                                   sep = " "))

  parser$add_argument("-o", "--output_dir", type = "character",
                      default = "outputs/data-viz-pkgs",
                      help = "Directory for file output [default %(default)s]")

  parser$add_argument("-q", "--aqi_country", type = "character",
                      required = TRUE,
                      choices = aqi_country_opts,
                      default = "United States",
                      help = paste(
                        "What countries AQI should be used in visualizations ",
                        "[default %(default)s].", sep = " "
                        )
                      )

  parser$add_argument("-d", "--delete_uncompress", type = "logical",
                      default = FALSE,
                      help = paste(
                        "TRUE/FALSE; delete the uncompressed version of",
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

  parser$add_argument("-c", "--calibrate", type = "character",
                      choices = calibration_opts,
                      help = paste("If valid option given, apply calibration",
                                   "to sensor data.", sep = " "))

  args <- parser$parse_args()
}

#------------------------------------------------------------------------------
# Load Data
programs <- c("India" = "IN", "Sri Lanka" = "IN", "Southern California" = "US",
              "Boston" = "US")

sensor_catalog <- load_SensorCatalog(site = args$site)

pat_list <- load_pat_list(site = args$site)

#------------------------------------------------------------------------------
# Prepare Data

# QAQC data. See vignettes/qaqc_pt1.rmd for details.
suppressMessages({
  source("scripts/qaqc.R")
})

# Load cleaned data.
pat_list <- readRDS("data/pat_list_qcd.rds")
sensor_list <- readRDS("data/sensor_list.rds")

if ( !is.null(args$calibrate) ) {
  proxy_site <- args$calibrate
  suppressMessages({
    source("scripts/calibrate.R")
  })
  # Load calibrated data.
  pat_list <- readRDS("data/pat_list_calibrated.rds")
  sensor_list <- readRDS("data/sensor_list_calibrated.rds")
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

#------------------------------------------------------------------------------
# Create workbook:

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
# TODO: Have PA_wkbk accept either just
# sensor object, or sensor object and pat objects.
wb <- make_PA_wkbk(sensor_list = sensor_list,
                   sensor_catalog = sensor_catalog,
                   aqi_data = aqi_list)

#------------------------------------------------------------------------------
# Vizualization Preparation:

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

# Calendar Plots:
#------------------------------------------------------------------------------
.calendar_pmPlot <- function(
  data,
  sensor_name,
  aqi_country,
  data_thresh = 75
  ) {
  # Loading AQI Info
  aqi_info <- pavisualizR::aqi_info$aqi_country

  # Generating plot.
  plot <- openair::calendarPlot(data,
                                pollutant = "pm25",
                                main = "Daily Average Particulate Matter 2.5",
                                xlab = sensor_name,
                                ylab = "Particulate Matter 2.5",
                                cols = aqi_info$colors,
                                labels = aqi_info$names,
                                breaks = aqi_info$breaks_24,
                                data.thresh = data_thresh)
  invisible(plot)
}

calendarPlots_list <- purrr::map2(
  .x = pm_list,
  .y = names(pm_list),
  .f = function(x,y, aqi_country) sensor_extractData(x) %>%
    select(date = datetime, pm25 = 2) %>%
    .calendar_pmPlot(sensor_name = y,
                     aqi_country = aqi_country),
  aqi_country = aqi_country)

if( length(calendarPlots_list) > 0 ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "calendarPlots_list")
}
#------------------------------------------------------------------------------
# Workweek/Weekend Plot:

workweek_plot <- workweek_weeknd_pmPlot(sensor_list = pm_list,
                                        sensor_colors = sensor_colors,
                                        aqi_country = aqi_country)

if( is.ggplot(workweek_plot) ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "workweek_plot")

}

#------------------------------------------------------------------------------
# Stacked Day of Week AQI Bar Charts
day_bar_plots <- purrr::map(.x = pm_list, .f = day_of_week_aqiBar,
                            aqi_country = aqi_country, position = "fill")

if( length(day_bar_plots ) > 0 ) {
  # Adding to viz
  visualizations <- append(x = visualizations, "day_bar_plots")

}

#------------------------------------------------------------------------------
# Monthly Faceted AQI Colored Points
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

}

#------------------------------------------------------------------------------
# COVID Measures Pre and During Plot:
# TODO: Affix to covid-measures database.
# TODO: Update plot for before, during, after.

# Prepare COVID Measures Dataframe.

# Load Covid Measures df.
covid_measures <- .load_covid_measures() %>%
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
             .f = covid_measures_Plot,
             covid_measures = covid_measures,
             facet_workweek = args$facet_covid_workweek)

if( length(covid_plots) > 0 ) {
  # Add to ppt.
  visualizations <- append(x = visualizations, "covid_plots")
}

#------------------------------------------------------------------------------
# Create ppt object.
ppt <- officer::read_pptx()

viz_ppt <- make_viz_ppt(mget(visualizations), ppt = ppt)

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

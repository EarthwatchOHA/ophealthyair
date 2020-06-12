# This script generates the Weekly Sensor Health Reports for all Programs.
# Saves reports to specified folder.

# This script generates the Weekly Sensor Health Report for given Program
# Saves reports in Specified folder.
suppressMessages({
  devtools::load_all()
})

# TODO: Add start and end date params.
setwd(here::here())
input_path <- "reporting/weekly-sensor-health-report.rmd"
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

#------------------------------------------------------------------------------
# Command Line Interface
program_opts <- load_SensorCatalog() %>%
  dplyr::filter(site != "Undeployed") %>%
  dplyr::pull(Program) %>%
  unique()

if ( !exists("args", mode = "list") ) {

  suppressPackageStartupMessages(library("argparse"))
  # Instantiating parser.
  parser <- ArgumentParser(
    description = paste(
      "Generate the Weekly Sensor Health Report for all Programs",
      "(as specified by the Sensor Catalog):",
      paste(program_opts, collapse = ", "), ".",
      "Saves reports in --output_dir.", sep = ""
    ),
    add_help = TRUE
  )
  # Optional Arguments
  parser$add_argument("-o", "--output_dir",
                      type = "character",
                      default = "outputs/sensor-health-reports/",
                      help = paste("Folder to save health report to",
                                   "[default %(default)s].", sep = " "))

  args <- parser$parse_args()
}
#------------------------------------------------------------------------------
# Input Control

#------------------------------------------------------------------------------
for (i in 1:length(program_opts)) {
  program <- program_opts[i]
  filename <- paste(program, ".html", sep = "")
  rmarkdown::render(input = input_path,
                    output_dir = args$output_dir,
                    output_file = filename,
                    params = list(
                      program = program
                    ))

  print(paste(program, "report complete."))
}

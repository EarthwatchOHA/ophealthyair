# This script generates the Weekly Sensor Health Report for given Program
# Saves reports in Specified folder.
suppressMessages({
  devtools::load_all()
})

# TODO: Add start and end date params.

setwd(here::here())
input_path <- "reporting/weekly-sensor-health-report.rmd"
# TODO: May need to make an input.
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
      "Generate the Weekly Sensor Health Report for given Program.",
      "Saves reports in --output_dir.", sep = ""
    ),
    add_help = TRUE
  )
  # Positional Arguments
  parser$add_argument("program",
                      type = "character",
                      choices = program_opts,
                      metavar = "",
                      help = paste("Statistic to select best model",                        "Argument type: %(type)s",
                                   "Allowable options are:",
                                   paste(program_opts, collapse = ", "),
                                   "[default %(default)s]", sep = " ")
                      )
  # Optional Arguments
  parser$add_argument("-o", "--output_dir",
                      type = "character",
                      default = "outputs/weekly-sensor-health-reports/",
                      help = paste("Folder to save health report to",
                                   "[default %(default)s].", sep = " "))

  args <- parser$parse_args()
}
#------------------------------------------------------------------------------
# Input Control

#------------------------------------------------------------------------------
filename <- paste(args$program, ".html", sep = "")
rmarkdown::render(input = input_path,
                  output_dir = args$output_dir,
                  output_file = filename,
                  params = list(
                    program = args$program
                  ))

print(paste(args$program, "report complete."))

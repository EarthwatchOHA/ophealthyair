# Make new proxy calibrations for all of our deploy sites.
suppressMessages({
  library(dplyr)
  devtools::load_all()
})

#------------------------------------------------------------------------------
# Command Line Interface
sensorCatalog <- load_SensorCatalog()

colloc_sites <- sensorCatalog %>%
  dplyr::filter(Collocated) %>%
  dplyr::pull(site) %>%
  unique()

if ( !exists("args", mode = "list") ) {

  suppressPackageStartupMessages(library("argparse"))
  # Instantiating parser.
  parser <- ArgumentParser(
    description = paste(
      "Generate calibration models for collocated sensors (according to",
      "Sensor Catalog) at all Collocation Sites (as specified by Sensor",
      "Catalog). Collocation Sites are:",
      paste(colloc_sites, collapse = ";  "),
      "Best model among collocated sensors is selected using --indicator.",
      sep = " "
      ),
    add_help = TRUE
  )
  # Optional Arguments
  parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                      help="Print extra output [default]")

  parser$add_argument("-q", "--quietly", action="store_false",
                      dest="verbose", help="Print little output")

  parser$add_argument("-i", "--indicator", action = "store",
                      choices = mod_select_stat_opts,
                      default = "R2",
                      help = paste(
                        "Statistic to select best model",
                        "Argument type: %(type)s",
                        "[default %(default)s]", sep = " "
                        )
                      )

  parser$add_argument("-a", "--adjusted", action = "store",
                      default = TRUE,
                      help = paste("If indicator R2, boolean adjust R2",
                                   "[default %(default)s]", sep = " "))

  args <- parser$parse_args()
}
#------------------------------------------------------------------------------
# Input Control

#------------------------------------------------------------------------------

for (i in 1:length(colloc_sites)) {

  args[["site"]] <- site = colloc_sites[[i]]

  print(colloc_sites[[i]])

  source("scripts/make-proxy-calibration.R")

}

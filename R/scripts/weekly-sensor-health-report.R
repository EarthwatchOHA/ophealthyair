# This script generates the weekly Sensor Health Report.
# TODO: Add program as an option for the report generation (cmd line option), add program to filename as well.
setwd(here::here())
filename <- paste("weekly-sensor-health-report-", lubridate::today(), ".html", sep = "")
input_path <- "R/reporting/weekly-sensor-health-report.rmd"
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
rmarkdown::render(input = input_path,
                  output_dir = "outputs/reports/weekly-sensor-health-reports/",
                  output_file = filename)

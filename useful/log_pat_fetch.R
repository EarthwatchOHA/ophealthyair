library(loggr)
library(here)

# TODO: Fix SimpleWarnings Log Output.
filename <- paste("outputs/logs/", today(), ".log", sep = "")
file.create(filename)
log_file(filename)
pat_list <- fetch_pat_list(pas = pas, sensor_labels = sensor_catalog[['Sensor Label']],
                           startdate = start, enddate = end)

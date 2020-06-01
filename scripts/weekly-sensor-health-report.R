# This script generates the weekly Sensor Health Report and saves into our OneDrive folder.
# TODO: Add start and end date params.
# TODO: Add file zipping.
setwd(here::here())
output_path <- "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/7.Data and Field Reports/2020/Weekly-Sensor-Health-Reports"
input_path <- "reporting/weekly-sensor-health-report.rmd"
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
programs <- c("All", "India", "Southern California", "Boston", "Sri Lanka")
  
for (i in 1:length(programs)) {
  program <- programs[i]
  filename <- paste(program, ".html", sep = "")
  rmarkdown::render(input = input_path,
                    output_dir = output_path,
                    output_file = filename,
                    params = list(
                      program = programs[i]
                    ))
  
  print(paste(program, "report complete."))
}

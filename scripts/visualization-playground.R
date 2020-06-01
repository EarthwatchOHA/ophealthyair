# This script is for testing visualizations.
library(sf)
devtools::load_all()
library(leaflet)
library(ggplot2)
library(dplyr)
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
library(openair)
source("revised-functions/revised_pas_leaflet.R")


# Preparing data.
#----------------------------------------------------------------
partner_site = "Waldorf School, Belmont MA"

programs <- c("India" = "IN", "Sri Lanka" = "IN", "Southern California" = "US", "Boston" = "US")

# Get program from site.
program <- site_sensors$Program[1]
# Get country code for visualization AQI parameters.
aqi_country <- programs[program]
# Use aqi in make_site_dataviz_pkg adds a US AQI column to workbook. Only use if program is in US.
if (aqi_country == "IN") {
  use_aqi <- FALSE
} else if( aqi_country == "US") {
  use_aqi <- TRUE
}
# Loading AQI Categorical Index info for plotting.
aqi_info <- load_aqi_info(country = aqi_country)

sensor_catalog <- load_SensorCatalog() 
sensors <- sensor_catalog %>% 
  filter(site == partner_site)

pat_list <- load_pat_list()[sensors$label]

source("scripts/qaqc.R")

#----------------------------------------------------------------
agg_list %>%
  bind_rows(.id = "sensor") %>%  
  left_join(select(sensor_catalog, label, Latitude, Longitude), by = c("sensor"= "label")) %>% 
  leaflet() %>% 
  addMarkers(layerId = "Purple Air Sensors")
  addProviderTiles(provider = "Stamen.Toner")



#----------------------------------------------------------------
# Visualizes EW sensors and nearby Reference monitors.

pas %>%
  pas_filter(label %in% sensor_catalog$`Sensor Label`) %>% 
  pas_leaflet(outsideOnly = FALSE) 
  
  
#----------------------------------------------------------------
# COVID-19 Before After Boxplots
# Data Prep.
# Getting Covid Measures
covid_measures <- load_covid_measures() %>%
  # Filters covid_measures so either site or program matches sensor catalog.
  dplyr::filter(site == partner_site | site %in% sensors$Program) %>% 
  # Creates interval out of start and end dates for measures.
  dplyr::mutate(interval = lubridate::interval(start = start_date, end = end_date))



data <- outliercount_list %>% 
  bind_rows(.id = "label") %>% 
  mutate(
    # Extracting hour.
    hour = format(datetime, "%I"),
    # Extracting AM/PM
    am_pm = format(datetime, "%p"),
    # Getting day of week for each hour.
    weekday = weekdays(datetime),
    # Workweek 1 or 0.
    workweek = factor(if_else(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "workweek", "weekend"))
  )

# Getting Timezone from data.
timezone <- attr(data$datetime,"tzone")

# Converting Timezones if necessary.
if (timezone != "UTC") {
  covid_measures <- covid_measures %>% 
    mutate(
      start_date = lubridate::with_tz(start_date, tzone = timezone),
      end_date = lubridate::with_tz(end_date, tzone = timezone)
    )
}

for (i in 1:nrow(covid_measures)) {
  # Creates a binary logical column for each response measure, whether it was in-place or not for an hourly measurement.
  measure <- covid_measures[["response_measure"]][[i]]
  data[[measure]] <- data$datetime %within% covid_measures[["interval"]][[i]]
}  

data <- data %>%
  dplyr::mutate(
    # Sums across the COVID measures columns and creates a new column, covid_measures with the sum total.
    covid_measures = dplyr::select(., any_of(covid_measures[["response_measure"]])) %>% rowSums(),
    # Using covid_measures column, if 0 (no measures in place), then false, else TRUE (measure/s in place).
    in_place = factor(ifelse(covid_measures == 0, FALSE, TRUE), labels = c("No Response Measures", "Response Measures in Place"))
  )

# Groups data depending on facet_workweek value.
test <- data %>%
  group_by(hour, am_pm, in_place, label) %>% 
  summarize(
    pm_median = median(pm25, na.rm=TRUE),
    pm_mean = mean(pm25, na.rm=TRUE),
    pm_min = min(pm25),
    pm_max = max(pm25),
    pm_25q = quantile(pm25, probs = .25),
    pm_75q = quantile(pm25, probs = .75),
    N = n()
  )

test %>%
  filter(label == "EW_Waldorf_4a8") %>% 
  ggplot2::ggplot(aes(x = hour)) +
  ggplot2::geom_crossbar(aes(y = pm_median, ymin = pm_25q, ymax = pm_75q,
                             color = in_place, fill = in_place), 
                         position = "dodge", alpha = 0.6) +
  ggplot2::labs(
    title = "Purple Air Sensor Readings Hour of Day Averages Pre and Post COVID-19 Measures",
    caption = paste("Bars represent 1st and 3rd Quartile",
                    "Point represents Hourly Median",
                    "Number of Hourly Measurements are above each Bar",
                    sep = "\n"),
    x = paste("Hour of Day ", "(", timezone, ")", sep = ""),
    y = "PM25 µg/m3",
    color = "COVID-19 Response Measures in Place",
    fill = "COVID-19 Response Measures in Place"
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11),
                 plot.caption = ggplot2::element_text(size = 15, vjust = 4, hjust = 0.5), legend.position = "bottom") +
  coord_polar(start = .25, direction = 1) +
  facet_grid(.~am_pm)

if (facet_workweek) {
  plots_list <- purrr::map(plots_list, .f = function(plt) plt + ggplot2::facet_grid(workweek~.))
}

covid_table <- gridExtra::tableGrob(dplyr::select(covid_measures, 
                                                  "Response Measures" = response_measure,
                                                  "Start" = start_date, "End" = end_date),
                                    rows=NULL)

gridExtra::grid.arrange(plt, covid_table, layout_matrix = rbind(c(1, 1, 2),
                                                               c(1, 1, NA)))


aggstats_list %>% 
  bind_rows(.id = "label") %>%
  mutate(
    # Getting day of week for each hour.
    weekday = factor(weekdays(datetime)),
    # Extracting hour.
    hour = factor(format(datetime, "%H")),
    ) %>% 
  ggplot(aes(x = hour, color = label)) +
  geom_point(aes(y = pm25)) +
  geom_errorbar()



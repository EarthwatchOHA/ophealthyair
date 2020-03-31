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
partner_site = "Waldorf School, Belmont MA"

sensor_catalog <- load_SensorCatalog() 
site_sensors <- sensor_catalog %>% 
  filter(site == partner_site)

pat_list <- load_pat_list()[site_sensors$label]

source("scripts/agg-qaqc.R")

MazamaSpatialUtils::setSpatialDataDir("data/spatial")
MazamaSpatialUtils::loadSpatialData()

#-----------------------------------------------------------------
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
  
  
#---------------------------------------------------------------
# COVID-19 Before After Boxplots
# Data Prep.
partner_site <- "Waldorf School, Belmont MA"  
  
# Reading COVID-19 Data
covid_path <- "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/Delivery/OHA-COVID-policy-responses.csv"
covid_measures <- readr::read_csv(file = covid_path) %>% 
  mutate(
    start_date = lubridate::parse_date_time(x = start_date, 
                                            orders = "d!-m!-y! H!:M!",
                                            tz = "UTC"),
    end_date = if_else(end_date == "Ongoing",
                            lubridate::now(tzone="UTC"),
                            lubridate::parse_date_time(x = end_date, orders = "d!-m!-y! H!:M!", tz = "UTC"))
    ) 

sensors <- sensor_catalog %>%
  filter(site == partner_site)
if (nrow(filter(covid_measures, site == partner_site)) >= 1) {
  covid_measures <- covid_measures %>% 
    filter(site == partner_site)
} else {
  covid_measures <- covid_measures %>% 
    filter(site %in% sensors$Program)
}


# PAT List
pat_list <- load_pat_list()[sensors$label]
source("scripts/qaqc.R")

data <- outliercount_list %>%
  bind_rows(.id = "label") %>% 
  mutate(
    # Pre/Post-COVID measures
    covid_measures = between(datetime, covid_measures$start_date, covid_measures$end_date),
    # Extracting hour.
    hour = format(datetime, "%H")
  ) %>%  
  group_by(hour, covid_measures, label) %>% 
  summarize(
    pm_median = median(pm25, na.rm=TRUE),
    pm_mean = mean(pm25, na.rm=TRUE),
    pm_min = min(pm25),
    pm_max = max(pm25),
    pm_25q = quantile(pm25, probs = .25),
    pm_75q = quantile(pm25, probs = .75)
  )

data %>% 
  ggplot(aes(x = hour)) +
  geom_point(aes(y = pm_median, color = covid_measures, fill = covid_measures)) +
  geom_crossbar(aes(y = pm_median, ymin = pm_25q, ymax = pm_75q,
                    color = covid_measures, fill = covid_measures),
                position = "dodge", alpha = 0.6) +
  facet_wrap(~label, shrink = TRUE, scales = "free_y") +
  labs(
    title = "Purple Air Sensor Readings Hour of Day Averages Pre and Post COVID-19 Measures",
    subtitle = "Bars represent 1st and 3rd Quartile, Point represents Hourly Median",
    caption = "Note: Y Axes are not aligned,",
    x = "Hour of the Day (UTC)",
    y = "PM25 µg/m3",
    color = paste(covid_measures$response_measure, "in place", sep = " "),
    fill = paste(covid_measures$response_measure, "in place", sep = " ")
  )
#----------------------------------------------------------------------------------------------
# TODO: Add docstring.
# TODO: Add error control system. 


covid_measures <- covid_measures %>% 
  filter(site == partner_site | site %in% sensors$Program)

# Loading AQI Categorical Index info for plotting.
aqi_info <- load_aqi_info(country = aqi_country)

data <- outliercount_list %>% 
  bind_rows(.id = "label")

# Getting the max and min observation time for plotting limits.
xmax <- max(data[["datetime"]])
xmin <- min(data[["datetime"]])


plot <- data %>% 
  ggplot2::ggplot(aes(x = datetime)) +
  ggplot2::geom_point(aes(y = pm25, color = label), alpha = 0.5) +
  ggplot2::scale_x_datetime() +
  ggplot2::coord_cartesian(xlim=c(min(data$pm25), max(data$pm25))) +
  # TODO: AQI PM Breaks need to be made to work. 
  ggplot2::annotate(geom = "rect", ymin = aqi_info$aqi_pm_mins, ymax = aqi_info$aqi_pm_maxs,
                    xmin=xmin, xmax=xmax, alpha = 0.3,
                    fill = aqi_info$colors) +
  ggplot2::labs(x = "Datetime (UTC)",
                y = "PM 2.5 μg/m3",
                title = "Hourly Average Particulate Matter 2.5 Concentration",
                subtitle = "with Hourly Max and Min Ribbon",
                color = "Sensor")

for (i in 1:nrow(covid_measures)) {
  plot + 
    geom_vline(aes(x = ))
}
# Data Visualization Package Visualization First Drafts
library(ggplot2)
library(lubridate)
library(dplyr)
library(AirSensor)
library(con2aqi)

source("R/ingestion-util.R")
source("R/delivery-util.R")

#-----------------------------------------------------------------------------------------------------
# Viz Prep

# Preparing preliminary datasets.
# Tabular representation of EPA AQI categorical index.
aqi_categories <- data.frame(minimums = c(0, 51, 101, 151, 201, 301),
                             maximums = c(50, 100, 150, 200, 300, 500),
                             colors = c("green", "yellow", "orange", "red", "purple", "maroon"),
                             labels = factor(c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous"), 
                                             levels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous")),
                             stringsAsFactors = FALSE)

aqi_info <- PWFSLSmoke::AQI
# Days during the workweek.
workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
# Days of the weekend.
weekend <- c("Saturday", "Sunday")

# Making vector of colors to use for sensors
sensor_colors <- RColorBrewer::brewer.pal(n = length(agg_list_test), name = "Dark2")
# Naming the vector with sensors.
names(sensor_colors) <- names(agg_list_test)
# Making vector of aqi category colors.
aqi_colors <- aqi_info$colors
# Naming the vector with the aqi category labels. 
names(aqi_colors) <- aqi_info$names

# Defining what aqi should be for PM's over 500.
above_500_repl_val <- 500
# Readying plotting set.
data <- agg_list_test %>% 
  # Convert to df.
  bind_rows(.id = "sensor") %>%
  # Filter by minimum observation count of 20.
  filter(min_count >= 20,
         !is.na(pm25)) %>% 
  mutate(
    # Mean, mean maximum, mean minimum of A and B channels
    pm_channels_max = (pm25_A_max+pm25_B_max)/2,
    pm_channels_min = (pm25_A_min+pm25_B_min)/2,
    # Calculating AQI for each of above channels.
    # If pm is above 500 (the upper limit of AQI scale), aqi = 500. 
    aqi = ifelse(pm25 <= 500, con2aqi_single(con = pm25, pollutant = "pm25"), 500),
    aqi_min = ifelse(pm_channels_min <= 500,
                     con2aqi_single(con = pm_channels_min, pollutant = "pm25"),
                     500),
    aqi_max = ifelse(pm_channels_max <= 500,
                     con2aqi_single(con = pm_channels_max, pollutant = "pm25"),
                     500),
    # Getting day of week for each hour.
    weekday = weekdays(datetime),
    # Extracting hour.
    hour = format(datetime, "%H"),
    # Workweek 1 or 0.
    workweek = factor(ifelse(weekday %in% workweek, 1, 0), labels = c("workweek", "weekend"))) 

#-----------------------------------------------------------------------------------------------

# Visualization 1:

# Getting the max and min observation time for plotting limits.
xmax <- max(data[["datetime"]])
xmin <- min(data[["datetime"]])

site_hourlyavg_aqi <- function(data, ribbon_alpha = 0.3, aqi_bar_alpha = 0.2, point_size = 1) {
  
  if ( !(is.data.frame(data)) )
    stop("data must be a dataframe.")
  
  if ( is.null(data$datetime) | !lubridate::is.POSIXct(data$datetime))
    stop("data must have datetime column containing POSIXct values.")
  
  plot <-
    data %>% 
    group_by(datetime) %>% 
    ggplot(aes(x = datetime)) +
    geom_point(aes(y = aqi, color = factor(sensor)), size = point_size) +
    geom_ribbon(aes(ymin = aqi_min, ymax = aqi_max, fill = factor(sensor)), alpha = ribbon_alpha) +
    scale_x_datetime() +
    scale_color_manual(values = sensor_colors) +
    scale_fill_manual(values = sensor_colors, guide = FALSE) +
    annotate("rect", ymin = aqi_categories$minimums, ymax = aqi_categories$maximums,
             xmin=xmin, xmax=xmax, alpha = aqi_bar_alpha, fill = aqi_colors) +
    coord_cartesian(ylim = c(0, max(data$aqi_max))) +
    labs(x = "Time",
         y = "AQI", 
         caption = site,
         title = "Channel Hourly Averages AQI",
         subtitle = "with Max and Min Ribbon",
         color = "Sensor")
  return(plot)
}

plot1 <- site_hourlyavg_aqi(data = data)
print(plot1)


#--------------------------------------------------------------------------------------------------
# Viz 2
# Line plot of hourly average aqi with lines for weekdays and for weekends.

workweek_weeknd_aqiPlot <- function(data) {
  
  # Summarizing data over workweek or weekend.
  workweek_weeknd_data <- 
    data %>% 
    group_by(hour, workweek, sensor) %>% 
    summarize(aqi = mean(aqi))
  
  
  workweek_weeknd_plot <-
    workweek_weeknd_data %>%
    ggplot(aes(x = hour, y = aqi, group = interaction(sensor, workweek))) +
    geom_line(aes(color = sensor, linetype = workweek), size = 1) + 
    annotate("rect", ymin = aqi_categories$minimums, ymax = aqi_categories$maximums,
             xmin = -Inf, xmax = Inf, alpha = 0.2, fill = aqi_categories$colors) +
    scale_linetype_manual(values=c("solid", "twodash")) +
    scale_color_manual(values = sensor_colors) +
    coord_cartesian(ylim = c(0, max(workweek_weeknd_data$aqi))) +
    labs(x = "Hour of Day",
         y = "AQI", 
         caption = site,
         title = "Hour of Day AQI Average Weekday vs. Weekend",
         subtitle = "Max and Min Ribbon",
         color = "Sensor",
         linetype = "Day Type")

  return(workweek_weeknd_plot)  
}

plot2 <- workweek_weeknd_aqiPlot(data = data)
print(plot2)

#-----------------------------------------------------------------------------------------------------
# Viz 3

day_of_week_aqiPlot <- function(data, title_text = "Average Hourly AQI",
                                x_axis_label = "Hour of Day", y_axis_label = "AQI") {
  
  # AQI Average Hourly Line plots faceted by day of the week.
  day_of_week_data <- 
    data %>% 
    group_by(hour, weekday, sensor) %>% 
    summarize(aqi = mean(aqi),
              aqi_min = mean(aqi_min),
              aqi_max = mean(aqi_max))
  
  
  # Pasting the vectors together.
  plot_fill_colors <- append(sensor_colors, aqi_colors)
  
  full_week <- append(workweek, weekend)
  
  day_plots <- list()
  
  for (i in 1:length(full_week)) {
    day <- full_week[[i]]
    
    day_plots[[day]] <- 
      day_of_week_data %>% 
      filter(weekday == day) %>% 
      ggplot(aes(x = hour, y = aqi, group = sensor)) +
      geom_line(aes(color = sensor), size = 1) +
      geom_point(aes(color = sensor)) +
      geom_ribbon(aes(ymin = aqi_min, ymax = aqi_max, fill = sensor), alpha = 0.3) +
      scale_color_manual(values = sensor_colors) +
      annotate("rect", ymin = aqi_categories$minimums, ymax = aqi_categories$maximums,
               xmin = -Inf, xmax = Inf, alpha = 0.2, fill = aqi_categories$colors) +
      scale_fill_manual(values = plot_fill_colors, guide = FALSE) +
      coord_cartesian(ylim = c(0, max(day_of_week_data$aqi_max))) +
      labs(subtitle = day,
           x = "Hour",
           y = "AQI",
           color = "Sensor")
  }
  weekday_plots <- grob_arrange_shared(plots_list = day_plots, title_text = title_text, nrows = 2,
                                       y_axis_label = y_axis_label, x_axis_label = x_axis_label)
  invisible(weekday_plots)
}

plot3 <- day_of_week_aqiPlot(data = data)
gridExtra::grid.arrange(plot3)


#-------------------------------------------------------------------------------
# Zipping and saving files. 
dir_path <- "outputs/graphics"


# 1.
filename1 = paste(dir_path, "/", "site_hourlyavg_aqiPlot.png", sep = "")
ggsave(filename = filename1, plot = plot1, width = 8)
# 2. 
filename2 = paste(dir_path, "/", "workweek-weekend-aqi-plot.png", sep = "")
ggsave(filename = filename2, plot = plot2, width = 8)
# 3. 
filename3 = paste(dir_path, "/", "day-plots.png", sep = "")
ggsave(filename = filename3, plot = plot3, height = 15, width = 20)

#-------------------------------------------------------------------------------

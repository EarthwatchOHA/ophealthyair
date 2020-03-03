# Functions for visualizations.
# TODO: In scope make these methods with . prefixes.
grob_arrange_shared <- function(plots_list, title_text = NULL, y_axis_label = NULL, x_axis_label = NULL,
                                nrows = 2) {
  
  # Converts first plot into grob and extracts.
  g <- ggplot2::ggplotGrob(plots_list[[1]] + theme(legend.position="right"))$grobs
  # Extracts legend from first plot.
  leg <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  # Removes legends from all plots in plot_list.
  plots_list <- lapply(plots_list,
                       function(x)
                         x + theme(legend.position = "none", axis.title = element_blank()))
  # Add legend grob to plot_list.
  plots_list[["leg"]] <- leg
  # Arranges grobs.
  arranged_plots <- 
    gridExtra::arrangeGrob(
      grobs = plots_list,
      nrow = nrows,
      top =  grid::textGrob(title_text, vjust = 0.7, gp = grid::gpar(fontface = "bold", cex = 1.5)),
      left = y_axis_label, bottom = x_axis_label)
  
  return(arranged_plots)
}


save_aqi_info <- function() {
  # TODO: Add docstring.
  # TODO: Credit PWFSLSmoke.
  
  # US
  # Loading EPA AQI categorical index info from PWFSL Smoke.
  # The breaks in this are by PM.
  us_aqi_info <- PWFSLSmoke::AQI
  # Adding minimums and maximums of AQI breaks.
  # TODO: Can I condense this into one series of breaks.
  us_aqi_info[["aqi_breaks"]] <- data.frame(minimums = c(0, 51, 101, 151, 201, 301),
                                            maximums = c(50, 100, 150, 200, 300, 500))
  
  us_aqi_info[["aqi_pm_mins"]] <- c(-Inf, 12.1, 35.5, 55.5, 150.5, 250.5)
  us_aqi_info[["aqi_pm_maxs"]] <- c(12, 35.4, 55.4, 150.4, 250.4, Inf)
  # India
  ind_aqi_info <- list(
    index_category = c("Good", "Satisfactory", "Moderate",
                       "Poor", "Very Poor", "Severe"),
    aqi_pm_mins = c(-Inf, 31, 61, 91, 121, 250),
    aqi_pm_maxs = c(30, 60, 90, 120, 250, Inf),
    aqi_breaks = c(0, 50, 100, 200, 300, 400, 500),
    index_category_descriptions = c("Good" = "Minimal Impact",
                                    "Satisfactory" = "May cause minor breathing discomfort to sensitive people.",
                                    "Moderate" = "May cause breathing discomfort to people with lung disease such as asthma and discomfort to people with heart disease, children and older adults.",
                                    "Poor" = "May cause breathing discomfort to people on prolonged exposure and discomfort to people with heart disease with short exposure.",
                                    "Very Poor" = "May cause respiratory illness to the people on prolonged exposution. Effect may be more pronounced in people with lung and heart diseases.",
                                    "Severe" = "May cause respiratory effects on even healthy people and serious health impacts on people with lung/heart diseases. The health impacts may be experienced even during light physical activity."),
    colors = c("#006600", "#009900", "#FFFF00", "#FF6600", "#CC0000", "#990000")
  )
  
  aqi_info <- list(
    US = us_aqi_info,
    IN = ind_aqi_info
  )
  
  
  saveRDS(aqi_info, file = "data/aqi_info.rds")
}

load_aqi_info <- function(country, path = "data/aqi_info.rds") {
  possible_countries <- c("US", "IN")
  if (!(country %in% possible_countries)) {
    stop(paste("country must be one of",
               paste(possible_countries, collapse = ", ")))
  }
  aqi_info <- readRDS(file = path)
  return(aqi_info[[country]])
}
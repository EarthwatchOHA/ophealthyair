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
  
  #Loading EPA AQI categorical index info from PWFSL Smoke.
  # The breaks in this are by PM.
  aqi_info <- PWFSLSmoke::AQI
  # Adding minimums and maximums of AQI breaks.
  # TODO: Can I condense this into one series of breaks.
  aqi_info[["aqi_breaks"]] <- data.frame(minimums = c(0, 51, 101, 151, 201, 301),
                                         maximums = c(50, 100, 150, 200, 300, 500)) 
  saveRDS(aqi_info, file = "data/aqi_info.rds")
}

load_aqi_info <- function(path = "data/aqi_info.rds") {
  aqi_info <- readRDS(file = path)
  return(aqi_info)
}

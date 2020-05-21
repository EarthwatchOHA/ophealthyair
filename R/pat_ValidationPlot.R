#' @title Do sensor readings fall within spec?
#'
#' @description Generates a scatter plot of pat sensor readings for a specified
#'   parameter with horizontal lines for valid reading limits.
#' 
#' @param pat A Purple Air Time Series object.
#' @param param A Purple Air sensor parameter, one of "pm25", "temperature",
#'   "humidity".
#' @param param_lims Numeric vector of length 2 specifying range of valid
#'   measurements fpr plotting horizontal lines. If default parameters are based
#'   off device specifications at
#'   https://www2.purpleair.com/products/purpleair-pa-ii.
#'   
#' @return ggplot object.
#' 
#' @example 
#' \dontrun{
#'   
#'   example_pat <- pat_load(label = "example")
#'   
#'   # Plotting with default specs.
#'   plot <- pat_ValidationPlot(pat = example_pat, param = 'pm25')
#'   
#'   # Plotting with custom specs.
#'   custom_lims <- c(5, 150)
#'   plot<- pat_ValidationPlot(pat = example_pat,
#'                             param = 'pm25',
#'                             param_lims = custom_lims)
#'   
#' }

pat_ValidationPlot <- function(
  pat,
  param,
  param_lims = NULL
) {
  #TODO: Reduce plotting.
  
  param_opts <- c(
    "pm25_A",
    "pm25_B",
    "pm25_both",
    "temperature",
    "humidity"
  )
  
  humidity_lims <- c(0, 100)
  temperature_lims <- c(-40, 185)
  pm25_lims <- c(0, 1000)
  
  
  if ( !(param %in% param_opts) ) {
    stop(paste("param must be one of:",
               paste(param_opts, collapse = ", ")))
  }
  if ( !pat_isPat(pat) ) {
    stop("pat must be a valid PA Timeseries object.")
  }
  
  # Split channel select.
  param_arg <- stringr::str_split(param, "_")
  
  param <- param_arg[[1]][[1]]
  
  if ( stringr::str_detect(param, "pm25") ) {
    
    channel <- param_arg[[1]][[2]]
    # If no parameter limits supplied.
  }
  
  # Retrieve appropriate value limits.
  param_lims <- get(paste(param[[1]][[1]], "lims", sep = "_"))
  
  # Check that param_lims is valid.
  if ( !is.numeric(param_lims) | !length(param_lims) == 2 ) {
    stop("param_lims must be a numeric vector of length 2.")
  }
  
  meta <- pat %>% pat_extractMeta()
  
  if ( param == "pm25" ) {
    
    if( channel == "both" ) {
      
      data <- pat %>%
        pat_extractData() %>%
        mutate(
          flag_A = between(pm25_A, param_lims[1], param_lims[2]),
          flag_B = between(pm25_B, param_lims[1], param_lims[2])
        )
      
      plot <- data %>% 
        ggplot(aes(x = datetime)) +
        geom_jitter(aes(y = pm25_A, color = factor(flag_A)), alpha = 0.6) +
        geom_jitter(aes(y = pm25_B, color = factor(flag_B)), alpha = 0.6) +
        scale_x_datetime() +
        scale_color_manual(values = c("green", "red"), guide = FALSE) +
        geom_hline(yintercept = param_lims[1], color = "red") +
        geom_hline(yintercept = param_lims[2], color = "red") +
        labs(
          x = "DateTime",
          y = param
        )
        
    } else if( channel == "A" | channel == "B" ) {
      
      param <- paste(param, channel, sep = "_")
      
      data <- pat %>% 
        pat_extractData() %>% 
        mutate(
          # Tags values out of range for coloring.
          flag = between(!!sym(param), param_lims[1], param_lims[2])
        )
      
      plot <- data %>% 
        ggplot(aes(x = datetime, y = !!sym(param), color = factor(flag))) +
        geom_jitter(alpha = 0.6) +
        scale_x_datetime() +
        scale_color_manual(values = c("green", "red"), guide = FALSE) +
        geom_hline(yintercept = param_lims[1], color = "red") +
        geom_hline(yintercept = param_lims[2], color = "red") +
        labs(
          x = "DateTime",
          y = paste("PM 2.5", channel, sep = " ")
        )
      
    }
  } else {
    data <- pat %>% 
      pat_extractData() %>% 
      mutate(
        # Tags values out of range for coloring.
        flag = between(!!sym(param), param_lims[1], param_lims[2])
      )
    
    plot <- data %>% 
      ggplot(aes(x = datetime, y = !!sym(param), color = factor(flag))) +
      geom_jitter(alpha = 0.6) +
      scale_x_datetime() +
      scale_color_manual(values = c("green", "red"), guide = FALSE) +
      geom_hline(yintercept = param_lims[1], color = "red") +
      geom_hline(yintercept = param_lims[2], color = "red") + 
      labs(
        x = "DateTime",
        y = param
      )
      
  }

  return(plot)
}
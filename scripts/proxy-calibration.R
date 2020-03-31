# Drafting proxy calibration system using Lodhi
library(dplyr)
devtools::load_all()
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
library(ropenaq)


# Trying to get data via openaq package and API.
#----------------------------------------------------------------------------------------------------------------------
# Openaq package is failing to read API.
# Trying for Lodhi Road data. Not currently working.
cities_tableIndia <- aq_locations(country="IN", city = "Delhi", parameter = "pm25")

lodhi_data <- aq_measurements(country = "IN", city = "Delhi", location = "Lodhi+Road%2C+Delhi+-+IMD")

# Trying Boston
cities_tableUS <- aq_locations(country="US", city = "Boston-Cambridge-Quincy", parameter = "pm25")

roxbury_data <- aq_measurements(location = "Boston+-+Roxbury", parameter = "pm25")

test <- aq_latest(city = "Boston-Cambridge-Quincy", parameter = "pm25")
#----------------------------------------------------------------------------------------------------------------------

# Going to test proxy-calibration on two different collocated sensor networks, Lodhi Road in India and Roxbury Mass.
# Starting with Roxbury as I believe there's more data here.
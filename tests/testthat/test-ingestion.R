# Testing Ingestion System
library(testthat)

skip_on_cran()
# Dates to use for testing.
start <- "01-10-2019"
end <- "02-10-2019" 

# Details for a sensor that should work.
label_good <- "Red Acre Stowe"
id_good <- 29709
# Details for a sensor that should fail.
label_bad <- "Reed Ackra Stove"
id_bad <- 29708

labels <- c(label_good, label_bad)
ids <- c(id_good, id_bad)

pas <- AirSensor::pas_load()

test_that("Testing fetch_pas Purple Air Series loading.", {
  expect_equal(pas, fetch_pas(countryCodes = "US"))
  expect_error(fetch_pas())
})


test_that("Testing get_pat.", {
  # Testing against AirSensor package.
  expect_equal(pat_createNew(pas = pas,
                             label = label_good, id = id_good,
                             startdate = start, enddate = end),
               get_pat(pas = pas,
                       label = label_good, id = id_good,
                       startdate = start, enddate = end))
  # Testing with bad label, bad id.
  expect_error(get_pat(pas = pas, label = label_bad, id = id_bad, startdate = start, enddate = end))
  # Testing with bad label, good id.
  expect_error(get_pat(pas = pas, label = label_bad, id = id_good, startdate = start, enddate = end))
  # Testing with good label, bad id.
  expect_error(get_pat(pas = pas, label = label_good, id = id_bad, startdate = start, enddate = end))
  # Testing without label.
  expect_error(get_pat(pas = pas, id = id_good, startdate = start, enddate = end))
  # Testing without id.
  expect_error(get_pat(pas = pas, label = label_good, startdate = start, enddate = end))
  # Testing with timezone.
  expect_equal(pat_createNew(pas = pas, label = label_good, id = id_good,
                             startdate = start, enddate = end, timezone = "GMT"),
               get_pat(pas = pas, label = label_good, id = id_good, 
                       startdate = start, enddate = end, timezone = "GMT"))
  })


test_that("Testing fetch_pats.", {
  expect_known_value(fetch_pat_list(pas = pas, sensor_labels = labels, sensor_ids = ids,
                                    startdate = start, enddate = end,
                                    output_path = NULL), file = "tests/cache")
})
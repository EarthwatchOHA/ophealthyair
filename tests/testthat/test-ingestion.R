# Testing Ingestion System
library(testthat)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
devtools::load_all("C://Users/iozeroff/Data-Science/R-Projects/AirSensor")
library(ophealthyair)

setArchiveBaseUrl(archiveBaseUrl = "http://smoke.mazamascience.com/data/PurpleAir")
setSpatialDataDir("data/spatial")
loadSpatialData("NaturalEarthAdm1")

# Our ingestion system is composed of wrapper functions around the Mazama 
# Science AirSensor.
# 
# These warppers provide trycatch and data persistence systems.
# The system is based around 3 functions.
# 1. fetch_pas
# 2. fetch_pat
# 3. fetch_pat_list
#
# fetch_pat_list is vectorized version of fetch_pat that iterates return
# values into a list.
# So we're testing fetch_pas and fetch_pat, to confirm the wrapper functions,
# and that data persistence is working correctly.

label <- "EW_IOApt_FF47"
id <- 40347

test_pas <- fetch_pas(countryCodes = "US", 
                      lookbackDays = 7,
                      output_path = "tests/cache/pas.rds")

pas <- pas_createNew(countryCodes = "US", lookbackDays = 7)

test_that("fetch_pas successfuly fetches and saves as .rds a pas object.", {
  expect_equal(test_pas, pas)
  expect_true(AirSensor::pas_isPas(test_pas))
  expect_equal(load_pas(path = "tests/cache/pas.rds"), test_pas)
})


test_pat <- fetch_pat(label = label, id = id, pas = test_pas)

pat <- pat_createNew(label = label, id = id, pas = pas)

test_that("fetch_pat returns a pat when expected, and an error message when expected.", {
  expect_equal(test_pat, pat)
  expect_true(AirSensor::pat_isPat(test_pat))
})

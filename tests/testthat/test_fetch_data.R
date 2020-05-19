# test the fetch_data function does what it says and gives useful warnings and errors

# setup: load some packages
library(dplyr)
library(lubridate)
library(tidyr)

# setup: load and clean some small data files to test downloads
gauge_list <- c(225200, 405202, 406201, 409200)
# data_list <- lapply(
#   gauge_list, function(x) system.file("testdata", paste0(x, ".csv"), package = "aae.data")
# )
data_list <- lapply(
  gauge_list, function(x) read.csv(paste0("inst/testdata/", x, ".csv"))
)
data_list <- lapply(
  data_list,
  function(x)
    x %>% mutate(date_formatted = parse_date_time(date, orders = c("dmy_HM")))
)
date_range <- lapply(
  data_list,
  function(x) as.character(range(x$date_formatted))
)

# test basic data downloads against manually downloaded data
test_that("fetch_data returns correct values", {

  # download data for sites and date ranges specified above
  fetched_data <- mapply(
    function(x, y) fetch_data(sites = x, start = y[1], end = y[2], variables = c("depth", "flow"), include_missing = TRUE),
    gauge_list, date_range
  )

  # check each data set in order
  for (i in seq_along(fetched_data)) {

    # convert to wide format
    data_tmp <- fetched_data[[i]] %>%
      pivot_wider(id_cols = c("date_formatted", "site_name", "site_code"),
                  names_from = "variable_name",
                  values_from = c("value", "quality_code"))

    # check equal for both variables
    expect_equal(data_tmp$value_stream_water_level_m, data_list[[i]]$water_level_m)
    expect_equal(data_tmp$quality_code_stream_water_level_m, data_list[[i]]$qc_water_level)
    expect_equal(data_tmp$value_stream_discharge_mld, data_list[[i]]$discharge_mld)
    expect_equal(data_tmp$quality_code_stream_discharge_mld, data_list[[i]]$qc_discharge)

  }

})

# check dims of output when include_missing is TRUE/FALSE
test_that("include_missing works correctly in fetch_data", {

  # checks gap filling when multiple sites have inconsistent data availability
  with_missing <- fetch_data(
    sites = gauge_list[1],
    start = date_range[[1]][1], end = date_range[[1]][2],
    variables = c("flow", "temp"),
    include_missing = TRUE
  )
  without_missing <- fetch_data(
    sites = gauge_list[1],
    start = date_range[[1]][1], end = date_range[[1]][2],
    variables = c("flow", "temp"),
    include_missing = FALSE
  )

})

# check warnings and errors come up appropriately
test_that("fetch_data messages, warns, and errors informatively", {

  # check message for partial and fully missing data on download

  # check warnings and errors (none in fetch_data?)

})

# make sure check_quality catches all unique QC values
test_that("check_quality returns all QC values", {

})

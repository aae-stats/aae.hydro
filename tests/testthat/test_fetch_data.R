# test the fetch_data function does what it says and gives useful warnings
# and errors

# setup: load some packages
library(dplyr)
library(lubridate)
library(tidyr)

# setup: load and clean some small data files to test downloads
gauge_list <- c(225200, 405202, 406201, 409200)
data_list <- lapply(
  gauge_list,
  function(x) read.csv(
    system.file("testdata", paste0(x, ".csv"), package = "aae.data")
  )
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
    function(x, y) fetch_data(
      sites = x,
      start = y[1],
      end = y[2],
      variables = c("depth", "flow"),
      include_missing = TRUE
    ),
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
    expect_equal(data_tmp$value_stream_water_level_m,
                 as.numeric(data_list[[i]]$water_level_m))
    expect_equal(data_tmp$quality_code_stream_water_level_m,
                 data_list[[i]]$qc_water_level)
    expect_equal(data_tmp$value_stream_discharge_mld,
                 as.numeric(data_list[[i]]$discharge_mld))
    expect_equal(data_tmp$quality_code_stream_discharge_mld,
                 data_list[[i]]$qc_discharge)

  }

})

# check dims of output when include_missing is TRUE/FALSE
test_that("include_missing works correctly in fetch_data", {

  # checks gap filling when multiple sites have inconsistent data availability
  with_missing <- fetch_data(
    sites = gauge_list[1],
    start = "2017-09-29", end = "2017-10-02",
    variables = c("flow", "temp"),
    include_missing = TRUE
  )
  without_missing <- fetch_data(
    sites = gauge_list[1],
    start = "2017-09-29", end = "2017-10-02",
    variables = c("flow", "temp"),
    include_missing = FALSE
  )

  # expect padded NAs in with_missing, not there in without_missing
  expect_equal(nrow(with_missing), 2 * nrow(without_missing))

  # expect only a single variable when !include_missing
  expect_length(unique(without_missing$variable_code), 1L)
  expect_length(unique(with_missing$variable_code), 2L)

  # check that missing values are all NA
  expect_equal(with_missing$value[with_missing$variable_code == "450.00"],
               as.numeric(rep(NA, 4L)))

})

# check warnings and errors come up appropriately
test_that("fetch_data messages, warns, and errors informatively", {

  # check message for fully missing data on download
  expect_message(
    fetch_data(
      sites = gauge_list[1],
      start = date_range[[1]][1], end = date_range[[1]][2],
      variables = c("flow", "temp"),
      include_missing = FALSE
    ),
    "No data for the following sites and variables:\n225200: temperature"
  )

  # check message for partially missing data on download
  expect_message(
    fetch_data(
      sites = gauge_list[1],
      start = "1992-04-25", end = "1992-05-02",
      variables = c("flow"),
      include_missing = FALSE
    ),
    "Incomplete data for the following sites and variables:\n225200: discharge"
  )

  # check message for partial and fully missing data on download
  expect_message(
    fetch_data(
      sites = gauge_list[1],
      start = "1992-04-25", end = "1992-05-02",
      variables = c("flow", "temp"),
      include_missing = FALSE
    ),
    "No data for the following sites and variables:\n225200:",
    " temperature\n\nIncomplete data"
  )

  # check message for multiple sites
  expect_message(
    fetch_data(
      sites = gauge_list[1:3],
      start = "1975-06-11", end = "1975-06-13",
      variables = c("flow"),
      include_missing = FALSE
    ),
    "No data for the following sites and variables:\n225200:",
    " temperature\n\nIncomplete data for the following sites and",
    " variables:\n405202: discharge"
  )

  # check message for list_variables call
  expect_message(
    fetch_data(
      sites = gauge_list[1:3],
      start = "1975-06-11", end = "1975-06-13",
      variables = c("flow"),
      include_missing = FALSE
    ),
    "Use list_variables\\(c\\(225200, 405202\\)\\)"
  )

})

# make sure check_quality catches all unique QC values
test_that("check_quality returns all QC values", {

  flow <- fetch_data(
    sites = gauge_list[1],
    start = date_range[[1]][1], end = date_range[[1]][2],
    variables = c("flow", "temp"),
    include_missing = FALSE
  )

  qc <- check_quality(flow)
  value <- levels(qc$quality_code)
  target <- levels(factor(flow$quality_code))
  expect_equal(value, target)

})

# setup: load some packages
library(lubridate)

# setup: simulate some data to test against
nyear <- years(9)
start <- dmy("01012010")
flow_sim <- data.frame(
  date = seq(start, start + nyear, by = 1)
)
flow_sim$value <- exp(rnorm(nrow(flow_sim)))

# create sample data sets for the two impute cases (years and rolling)
flow_year <- flow_sim
flow_year$value[year(flow_year$date) == 2010] <- NA
flow_year$value[year(flow_year$date) == 2012] <- NA
flow_rolling <- flow_sim
flow_rolling$value[c(100:103, 200:203, 300, 501, 673)] <- NA

test_that("impute_year fills gaps correctly", {

  # should fill without issues
  flow_impute <- impute_year(
    flow_year$value,
    date = flow_year$date,
    threshold = 200
  )
  expect_equal(sum(is.na(flow_impute)), 0L)

  # but not with high threshold
  flow_impute <- impute_year(
    flow_year$value,
    date = flow_year$date,
    threshold = 400
  )
  expect_equal(anyNA(flow_impute), TRUE)

  # check leap years (only one possible replacement
  #   to choose from in this case)
  flow_impute <- impute_year(
    flow_year$value,
    date = flow_year$date,
    threshold = 200
  )
  expect_equal(
    flow_impute[year(flow_year$date) == 2012],
    flow_impute[year(flow_year$date) == 2016]
  )

})

test_that("impute_rolling works as expected", {

  # check 5-day median
  value <- impute_rolling(flow_rolling$value, n = 5, fun = median)
  target <- flow_rolling$value
  observed_na <- which(is.na(target))
  for (i in seq_along(observed_na)) {
    idx <- observed_na[i]
    idy <- observed_na[i] - 4
    target[idx] <- median(flow_rolling$value[idy:idx], na.rm = TRUE)
  }
  expect_equal(value, target)

})

test_that("impute_rolling works with different functions", {

  # check 5-day median
  flow_impute <- impute_rolling(flow_rolling$value, n = 5, fun = median)
  expect_equal(sum(is.na(flow_impute)), 0L)

  # check 5-day mean
  flow_impute <- impute_rolling(flow_rolling$value, n = 5, fun = mean)
  expect_equal(sum(is.na(flow_impute)), 0L)

  # check 5-day max
  flow_impute <- impute_rolling(flow_rolling$value, n = 5, fun = max)
  expect_equal(sum(is.na(flow_impute)), 0L)

  # check 5-day custom function
  flow_impute <- impute_rolling(
    flow_rolling$value,
    n = 8,
    fun = function(x, ...) sd(x, ...) / mean(x, ...)
  )
  expect_equal(sum(is.na(flow_impute)), 0L)

})

test_that("impute_rolling works recursively", {

  # try again with more NAs
  run_length <- 200
  flow_rolling_many <- flow_rolling
  flow_rolling_many$value[366:(366 + run_length)] <- NA

  # check 5-day median without recursion
  niter <- 1
  flow_impute <- impute_rolling(flow_rolling_many$value, n = 5, fun = median)
  expected_na <- run_length - 4L * (niter - 1) - 3L
  expect_equal(sum(is.na(flow_impute)), ifelse(expected_na > 0, expected_na, 0L))

  # check 5-day median with recursion and 10 repeats
  niter <- 10
  flow_impute <- impute_rolling(flow_rolling_many$value, n = 5, fun = median, recursive = TRUE, max_iter = niter)
  expected_na <- run_length - 4L * (niter - 1) - 3L
  expect_equal(sum(is.na(flow_impute)), ifelse(expected_na > 0, expected_na, 0L))

  # check 5-day median with recursion and 30 repeats
  niter <- 30
  flow_impute <- impute_rolling(flow_rolling_many$value, n = 5, fun = median, recursive = TRUE, max_iter = niter)
  expected_na <- run_length - 4L * (niter - 1) - 3L
  expect_equal(sum(is.na(flow_impute)), ifelse(expected_na > 0, expected_na, 0L))

  # check 5-day median with recursion and 30 repeats
  niter <- 51
  flow_impute <- impute_rolling(flow_rolling_many$value, n = 5, fun = median, recursive = TRUE, max_iter = niter)
  expected_na <- run_length - 4L * (niter - 1) - 3L
  expect_equal(sum(is.na(flow_impute)), ifelse(expected_na > 0, expected_na, 0L))

})

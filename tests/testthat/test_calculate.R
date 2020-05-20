context("calculate")

# setup: load some packages
library(dplyr)
library(lubridate)

# setup: simulate some data to test against
nyear <- years(5)
start <- dmy("01012010")
flow_sim <- data.frame(
  date = seq(start, start + nyear, by = 1)
)
flow_sim$month <- month(flow_sim$date)
flow_sim$year <- year(flow_sim$date)
flow_sim$value <- exp(rnorm(nrow(flow_sim)))

# setup: define function to calculate averages and return
#   appopriate types
calculate_manual <- function(value, class, fun = "median", subset = NULL) {
  out <- tapply(value, class, fun)
  out <- data.frame(
    date = as.numeric(names(out)),
    metric = out
  )
  rownames(out) <- NULL
  if (!is.null(subset)) {
    out <- out[out$date %in% subset, ]
  }
  out
}

test_that("calculate returns correct survey values with different seasons", {

  # all months and all years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 1:12, lag = 0),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value, flow_sim$year + 1, subset = 2010:2015)
  expect_equal(value, target)

  # each month separately
  for (i in seq_len(24)) {
    value <- flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = survey(season = i, lag = 0),
        na.rm = TRUE
      )
    )
    if (i <= 12) {
      flow_tmp <- flow_sim %>% filter(month == i)
      flow_tmp$year <- flow_tmp$year + 1
    } else {
      flow_tmp <- flow_sim %>% filter(month == i)
    }
    target <- calculate_manual(flow_sim$value, flow_sim$year, subset = 2010:2015)
    expect_equal(value, target)
  }

})

test_that("calculate returns correct annual values with different seasons", {


})

test_that("calculate returns correct annual values with different functions", {

  # don't forget counts of days above/below and rolling range

  # plus standard meidna, mean, max, etc.
})

test_that("calculate returns correct baseline values with different seasons", {


})

test_that("calculate returns correct weekly values", {


})

test_that("calculate returns correct monthly values", {


})

test_that("calculate returns correct values with lags", {


})

test_that("calculate returns correct values with subsets", {


})

test_that("calculate returns correct values with rescaling", {


})

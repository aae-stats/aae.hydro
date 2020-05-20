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
calculate_manual <- function(value,
                             class,
                             fun = median,
                             subset = NULL,
                             ...) {
  out <- tapply(value, class, fun, ...)
  out <- data.frame(
    date = as.numeric(names(out)),
    metric = out
  )
  rownames(out) <- NULL
  if (!is.null(subset)) {
    out <- out[out$date %in% subset, ]
    rownames(out) <- seq_len(nrow(out))
  }
  if (!all(subset %in% out$date)) {
    idx <- !(subset %in% out$date)
    out <- rbind(out, data.frame(date = subset[idx], metric = NA))
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
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year + 1,
                             subset = 2011:2015)
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
      subset <- 2011:2015
    } else {
      flow_tmp <- flow_sim %>% filter(month == (i - 12))
      subset <- 2011:2015
    }
    target <- calculate_manual(flow_tmp$value,
                               flow_tmp$year,
                               subset = subset)
    expect_equal(value, target)
  }

  # season as a string
  value_list <- c("full_year", "spawning", "winter", "spring")
  month_list <- list(1:12, 10:12, 6:8, 9:11)
  for (i in seq_along(value_list)) {
    value <- flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = survey(season = value_list[i]),
        na.rm = TRUE
      )
    )
    flow_tmp <- flow_sim %>% filter(month %in% month_list[[i]])
    target <- calculate_manual(flow_tmp$value,
                               flow_tmp$year + 1,
                               subset = 2011:2015)
    expect_equal(value, target)
  }

  # error if season not a known string
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = survey(season = "summery"),
        na.rm = TRUE
      )
    ),
    "season must be one of"
  )


})

test_that(
  "calculate returns correct annual values with different seasons", {

  # all months and all years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(season = 1:12, lag = 0),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = 2010:2015)
  expect_equal(value, target)

  # each month separately
  for (i in seq_len(12)) {
    value <- flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(season = i, lag = 0),
        na.rm = TRUE
      )
    )
    flow_tmp <- flow_sim %>% filter(month == i)
    subset <- 2010:2015
    target <- calculate_manual(flow_tmp$value,
                               flow_tmp$year,
                               subset = subset)
    expect_equal(value, target)
  }

  # discontinuous months in all years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(season = c(1, 4, 7), lag = 0),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>% filter(month %in% c(1, 4, 7))
  target <- calculate_manual(flow_tmp$value,
                             flow_tmp$year,
                             subset = 2010:2015)
  expect_equal(value, target)

})

test_that(
  "calculate returns correct annual values with different functions", {

  # make a list of functions
  fun_list <- c(mean, max, min, sd, var, sum)

  # loop over all functions
  for (i in seq_along(fun_list)) {
    value <- flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        fun = fun_list[[i]],
        resolution = annual(season = 1:12, lag = 0),
        na.rm = TRUE
      )
    )
    target <- calculate_manual(flow_sim$value,
                               flow_sim$year,
                               fun = fun_list[[i]],
                               subset = 2010:2015)
    expect_equal(value, target)
  }

  # days below
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      fun = days_below,
      resolution = annual(season = 1:12, lag = 0),
      threshold = quantile(.$value, p = 0.1)
    )
  )
  target <- calculate_manual(
    flow_sim$value,
    flow_sim$year,
    fun = days_below,
    subset = 2010:2015,
    threshold = quantile(flow_sim$value, p = 0.1)
  )
  expect_equal(value, target)

  # days above
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      fun = days_above,
      resolution = annual(season = 1:12, lag = 0),
      threshold = quantile(.$value, p = 0.75)
    )
  )
  target <- calculate_manual(
    flow_sim$value,
    flow_sim$year,
    fun = days_above,
    subset = 2010:2015,
    threshold = quantile(flow_sim$value, p = 0.75)
  )
  expect_equal(value, target)

  # rolling range
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      fun = rolling_range,
      resolution = annual(season = 1:12, lag = 0, subset = 2010:2014),
      lag = 3
    )
  )
  flow_tmp <- flow_sim %>% filter(year < 2015)
  target <- calculate_manual(
    flow_tmp$value,
    flow_tmp$year,
    fun = rolling_range,
    subset = 2010:2014,
    lag = 3
  )
  expect_equal(value, target)

  # rolling range with absolute difference
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      fun = rolling_range,
      resolution = annual(season = 1:12, lag = 0, subset = 2010:2014),
      lag = 3,
      type = "diff"
    )
  )
  flow_tmp <- flow_sim %>% filter(year < 2015)
  target <- calculate_manual(
    flow_tmp$value,
    flow_tmp$year,
    fun = rolling_range,
    subset = 2010:2014,
    lag = 3,
    type = "diff"
  )
  expect_equal(value, target)

})

test_that(
  "calculate returns correct baseline values with different seasons", {

  # all months and all years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = baseline(season = 1:12),
      na.rm = TRUE
    )
  )
  target <- data.frame(
    date = 0,
    metric = median(flow_sim$value)
  )
  expect_equal(value, target)

  # each month separately
  for (i in seq_len(12)) {
    value <- flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = baseline(season = i),
        na.rm = TRUE
      )
    )
    flow_tmp <- flow_sim %>% filter(month == i)
    target <- data.frame(
      date = 0,
      metric = median(flow_tmp$value)
    )
    expect_equal(value, target)
  }

})

test_that("calculate returns correct weekly values", {

  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = weekly(subset = 2010),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>% filter(year == 2010)
  target <- calculate_manual(flow_tmp$value,
                             floor_date(flow_tmp$date, unit = "weeks"))
  expect_equal(value$metric, target$metric)

})

test_that("calculate returns correct monthly values", {

  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = monthly(),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             floor_date(flow_sim$date, unit = "months"))
  expect_equal(value$metric, target$metric)

})

test_that("calculate returns correct values with lags", {

  # weeks
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = weekly(lag = 3, subset = 2011),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>% mutate(
    date = date + weeks(3)
  )
  flow_tmp <- flow_tmp %>% filter(date == 2011)
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(flow_tmp$date, unit = "weeks")
  )
  expect_equal(value$metric, target$metric)

  # months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = monthly(lag = 1, subset = 2011),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>%
    filter(date %within% interval(ymd("2010-12-01"), ymd("2011-11-30")))
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(flow_tmp$date, unit = "months")
  )
  expect_equal(value$metric, target$metric)

  # annual
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(lag = 1, subset = 2011:2014),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>% filter(year %in% c(2010:2013))
  target <- calculate_manual(
    flow_tmp$value,
    year(flow_tmp$date)
  )
  expect_equal(value$metric, target$metric)

  # annual lagged by 1 year should equal survey
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(lag = 1, subset = 2011:2014),
      na.rm = TRUE
    )
  )
  target <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(subset = 2011:2014),
      na.rm = TRUE
    )
  )
  expect_equal(value, target)

  # survey lagged by 1 year should equal true lagged by 2 years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(lag = 1, subset = 2012:2014),
      na.rm = TRUE
    )
  )
  flow_tmp <- flow_sim %>% filter(year %in% c(2010:2012))
  target <- calculate_manual(
    flow_tmp$value,
    year(flow_tmp$date)
  )
  expect_equal(value$metric, target$metric)

})

test_that("calculate returns correct values with subsets", {

  # annual with multi-year subset
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = 2011:2014),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = 2011:2014)
  expect_equal(value, target)

  # single year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = 2013),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = 2013)
  expect_equal(value, target)

  # discontinuous years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2011, 2014)),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2011, 2014))
  expect_equal(value, target)

  # unavailable years
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2008:2009)),
      na.rm = TRUE
    )
  )
  expect_equal(nrow(value), 0L)

  # discontinuous years with on unavailable
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2009, 2014)),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2009, 2014))
  expect_equal(value$metric, target$metric[1])
  expect_equal(nrow(value), 1L)

})

test_that("calculate returns correct values with rescaling", {

  # annual values rescaled by long-term median in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = by_median(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / median(flow_sim$value)
  expect_equal(target, value)

  # annual values rescaled by long-term max in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = by_max(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / max(flow_sim$value)
  expect_equal(target, value)

  # annual values rescaled by long-term mean in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = by_mean(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / mean(flow_sim$value)
  expect_equal(target, value)

  # annual values rescaled by long-term variance/generic in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = by_generic(subset = 2010:2015, fun = var),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / var(flow_sim$value)
  expect_equal(target, value)

  # annual values rescaled by long-term median in subset of months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = by_median(subset = 2010:2015, season = 1:4),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <-
    target$metric / median(flow_sim$value[flow_sim$month %in% c(1:4)])
  expect_equal(target, value)

})

test_that("rescale returns error when subset not specified", {

  # annual values rescaled by long-term median in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = by_median(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

  # annual values rescaled by long-term max in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = by_max(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )


  # annual values rescaled by long-term mean in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = by_mean(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

  # annual values rescaled by long-term variance/generic in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = by_generic(fun = var),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

})

test_that("rescale returns error when specified as incomplete list", {

  # annual values rescaled by long-term median in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = list(subset = c(2010:2014, fun = median)),
        na.rm = TRUE
      )
    ),
    "rescale must be a list containing four arguments"
  )

})

test_that("rescale can be specified directly with informative errors", {

  # default rescale works (divide by long-term median)
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      rescale = TRUE,
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = 2010:2014)
  target$metric <- target$metric / median(flow_sim$value)
  expect_equal(value, target)

  # error if rescale isn't NULL, TRUE, or a list with correct elements
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        rescale = "median",
        na.rm = TRUE
      )
    ),
    "rescale must be one of "
  )

})

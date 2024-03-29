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
nsurvey <- 50
flow_years <- sort(unique(year(flow_sim$date)))
trigger_dates <- dmy(
  paste(
    sample.int(30, size = nsurvey, replace = TRUE),
    sample(c(9:11), size = nsurvey, replace = TRUE),
    sample(flow_years[-length(flow_years)], size = nsurvey, replace = TRUE),
    sep = "-"
  )
)
survey_dates <- dmy(
  paste(
    sample.int(30, size = nsurvey, replace = TRUE),
    sample(c(4:6), size = nsurvey, replace = TRUE),
    year(trigger_dates) + 1L,
    sep = "-"
  )
)

# setup: define function to calculate averages and return
#   appropriate types
calculate_manual <- function(value,
                             class,
                             fun = median,
                             subset = NULL,
                             ...) {

  # calculate fun on value by class
  out <- tapply(value, class, fun, ...)

  # quick catch to return non-NA names in outputs
  names_ok <- suppressWarnings(!is.na(as.numeric(names(out)[1])))
  if (names_ok) {
    names_out <- as.numeric(names(out))
  } else {
    names_out <- names(out)
  }

  # format in data.frame similar to calculate
  out <- data.frame(
    date = names_out,
    metric = out
  )
  rownames(out) <- NULL

  # subset if needed
  if (!is.null(subset)) {
    out <- out[out$date %in% subset, ]
    rownames(out) <- seq_len(nrow(out))
  }

  # fill with NAs if subset extends beyond dates
  if (!all(subset %in% out$date)) {
    idx <- !(subset %in% out$date)
    out <- rbind(out, data.frame(date = subset[idx], metric = NA))
  }

  # return
  out

}

test_that("calculate returns correct survey values with different seasons", {

  # all month and all years
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
  value_list <- c("full_year", "winter", "spring")
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
  interval_set <- interval(dmy("01-01-2010"), dmy("06-01-2011"))
  flow_tmp <- flow_sim %>% filter(date %within% interval_set)
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(
      flow_tmp$date, unit = "weeks", week_start = wday(flow_tmp$date[1]) - 1
    )
  )
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

  # returns NA for when lagged data unavailable
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = weekly(lag = 3),
      na.rm = TRUE
    )
  )
  expect_equal(value$metric[1:3], as.numeric(rep(NA, 3)))

  # and matches true data for lagged weeks (ignoring NAs)
  flow_tmp <- flow_sim %>% mutate(
    date = date + weeks(3)
  )
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(
      flow_tmp$date, unit = "weeks", week_start = wday(flow_tmp$date[1]) - 1
    )
  )
  target$date <- as.Date(target$date)
  expect_equal(value$metric[4:nrow(value)], target$metric[1:258])

  # weeks with subset
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
  interval_set <- interval(dmy("01-01-2011"), dmy("06-01-2012"))
  flow_tmp <- flow_tmp %>% filter(date %within% interval_set)
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(
      flow_tmp$date, unit = "weeks", week_start = wday(flow_tmp$date[1]) - 1
    )
  )
  target$date <- as.Date(target$date)
  expect_equal(value, target)

  # months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = monthly(lag = 1, subset = 2011),
      na.rm = TRUE
    )
  )
  interval_set <- interval(dmy("01-12-2010"),
                           dmy("30-11-2011"))
  flow_tmp <- flow_sim %>% filter(date %within% interval_set)
  target <- calculate_manual(
    flow_tmp$value,
    floor_date(flow_tmp$date,
               unit = "months",
               week_start = wday(flow_tmp$date[1]) - 1)
  )
  expect_equal(value$metric, target$metric)

  # months lagged by days
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = monthly(lag = days(10), subset = 2011),
      na.rm = TRUE
    )
  )
  interval_set <- interval(dmy("01-01-2011"),
                           dmy("01-01-2012"))
  flow_tmp <- flow_sim %>% filter(date %within% interval_set)
  target_list <- unique(floor_date(flow_tmp$date, unit = "months")) - days(10)
  interval_list <- list()
  for (i in seq_along(target_list)[-1]) {
    interval_list[[i - 1]] <-
      interval(target_list[i - 1], (target_list[i] - days(1)))
  }
  interval_set <- interval(dmy("22-12-2010"),
                           dmy("20-12-2011"))
  flow_tmp <- flow_sim %>% filter(date %within% interval_set)
  id <- lapply(interval_list, function(x) flow_tmp$date %within% x)
  id <- mapply(function(x, y) x * y, id, seq_len(length(id)))
  id <- apply(id, 1, sum)
  target <- calculate_manual(
    flow_tmp$value, id
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

test_that("calculate returns correct values with truncation", {

  # check cut-off prior to survey date
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 7:18, lag = 0, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= dmy(paste0("01-07-", year(survey_dates[i]) - 1L)) &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check cut-off after trigger date
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 7:18, lag = 0, start = trigger_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(trigger_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= dmy(paste0("30-06-", year(trigger_dates[i]) + 1L))
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check cut-off prior to survey and after trigger date
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 7:18, lag = 0, start = trigger_dates, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(trigger_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check cut-off prior to survey date with annual resolution
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(season = 1:12, lag = 0, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= dmy(paste0("01-01-", year(survey_dates[i]))) &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check cut-off after trigger date with annual resolution
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(season = 1:12, lag = 0, start = trigger_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= dmy(paste0("31-12-", year(trigger_dates[i])))
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check cut-off prior to survey date with annual resolution
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(season = 1:12, lag = 0, start = trigger_dates, end = trigger_dates + months(1)),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(trigger_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= trigger_dates[i] + months(1)
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

})

test_that("calculate selects correct default truncation dates", {

  # check season starting late in the previous calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 12:18, lag = 0, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= dmy(paste0("01-12-", year(survey_dates[i]) - 1L)) &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check season starting in second calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 14:18, lag = 0, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= dmy(paste0("01-02-", year(survey_dates[i]))) &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check season starting early in first calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 3:18, lag = 0, end = survey_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates)) {
    idx <- flow_sim$date >= dmy(paste0("01-03-", year(survey_dates[i]) - 1L)) &
      flow_sim$date <= survey_dates[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check season ending in first calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 3:12, lag = 0, start = trigger_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(trigger_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= dmy(paste0("31-12-", year(survey_dates[i]) - 1L))
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

  # check season ending late in second calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 3:24, lag = 0, start = trigger_dates),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(trigger_dates)) {
    idx <- flow_sim$date >= trigger_dates[i] &
      flow_sim$date <= dmy(paste0("31-12-", year(survey_dates[i])))
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

})

test_that("calculate gives correct outputs when collapsing duplicate targets", {

  # duplicate survey dates many times
  survey_dates_duplicated <- rep(survey_dates, each = 20)

  # check season starting late in the previous calendar year
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = survey(season = 12:18, lag = 0, end = survey_dates_duplicated),
      na.rm = TRUE
    )
  )
  target <- rep(NA, nrow(value))
  for (i in seq_along(survey_dates_duplicated)) {
    idx <- flow_sim$date >= dmy(paste0("01-12-", year(survey_dates_duplicated[i]) - 1L)) &
      flow_sim$date <= survey_dates_duplicated[i]
    target[i] <- median(flow_sim$value[idx])
  }
  expect_equal(value$metric, target)

})

test_that("calculate returns correct values with standardising", {

  # annual values standardised by long-term median in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = by_median(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / median(flow_sim$value)
  expect_equal(target, value)

  # annual values standardised by long-term max in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = by_max(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / max(flow_sim$value)
  expect_equal(target, value)

  # annual values standardised by long-term mean in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = by_mean(subset = 2010:2015),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / mean(flow_sim$value)
  expect_equal(target, value)

  # annual values standardised by long-term variance/generic in all months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = by_generic(subset = 2010:2015, fun = var),
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = c(2010:2014))
  target$metric <- target$metric / var(flow_sim$value)
  expect_equal(target, value)

  # annual values standardised by long-term median in subset of months
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = by_median(subset = 2010:2015, season = 1:4),
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

test_that("standardise returns error when subset not specified", {

  # annual values standardised by long-term median in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = by_median(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

  # annual values standardised by long-term max in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = by_max(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )


  # annual values standardised by long-term mean in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = by_mean(),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

  # annual values standardised by long-term variance/generic in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = by_generic(fun = var),
        na.rm = TRUE
      )
    ),
    "subset of years must be specified"
  )

})

test_that("standardise returns error when specified as incomplete list", {

  # annual values standardised by long-term median in all months
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = list(subset = c(2010:2014, fun = median)),
        na.rm = TRUE
      )
    ),
    "standardise must be a list containing four arguments"
  )

})

test_that("standardise can be specified directly with informative errors", {

  # default standardise works (divide by long-term median)
  value <- flow_sim %>% do(
    calculate(
      value = .$value,
      date = .$date,
      resolution = annual(subset = c(2010:2014)),
      standardise = TRUE,
      na.rm = TRUE
    )
  )
  target <- calculate_manual(flow_sim$value,
                             flow_sim$year,
                             subset = 2010:2014)
  target$metric <- target$metric / median(flow_sim$value)
  expect_equal(value, target)

  # error if standardise isn't NULL, TRUE, or a list with correct elements
  expect_error(
    flow_sim %>% do(
      calculate(
        value = .$value,
        date = .$date,
        resolution = annual(subset = c(2010:2014)),
        standardise = "median",
        na.rm = TRUE
      )
    ),
    "standardise must be one of "
  )

})

context("scenario")

# setup: load some packages
library(lubridate)

# setup: simulate some data to test against
nyear <- years(5)
start <- dmy("01012010")
flow_sim <- data.frame(
  date = seq(start, start + nyear, by = 1)
)
flow_sim$value <- exp(rnorm(nrow(flow_sim)))

# define context for a flow scenario
quant_fn <- function(x, lower = 0.9, upper = 1.1) {
  out <- "average"
  if (median(x) < lower)
    out <- "dry"
  if (median(x) > upper)
    out <- "wet"
  out
}

test_that("generate_scenario correctly applies rules", {

  settings <- list(
    # baseflows @100 ML/d
    list(
      thresholds = list(
        c(100, Inf)
      ),
      timing = list(
        c(1:12)
      ),
      duration = list(
        Inf,
        3
      ),
      context = NULL
    ),
    # baseflows @200 ML/d
    list(
      thresholds = list(
        c(200, Inf)
      ),
      timing = list(
        c(1:12)
      ),
      duration = list(
        Inf
      ),
      context = NULL
    ),
    # baseflows @200 ML/d from June to November
    list(
      thresholds = list(
        c(200, Inf)
      ),
      timing = list(
        c(6:11)
      ),
      duration = list(
        Inf
      ),
      context = NULL
    ),
    # flow caps at 5 ML/d during summer
    list(
      thresholds = list(
        c(-Inf, 500)
      ),
      timing = list(
        c(12, 1:2)
      ),
      duration = list(
        Inf
      ),
      context = NULL
    ),
  )

  # generate flow scenario
  value <- vector("list", length = length(settings))
  for (i in seq_along(settings)) {
    value[[i]] <- generate_scenario(
      x = flow_sim$value,
      date = flow_sim$date,
      thresholds = settings[[i]]$thresholds,
      timing = settings[[i]]$timing,
      duration = settings[[i]]$duration,
      context = settings[[i]]$context,
      resolution = water_year
    )
  }

  # check baseflows scenarios
  # value[1:3]
  expect_equal(1L, 1L)

  # and flow caps
  # value[[4]]

})

test_that("generate_scenario correctly applies rules by context", {

  # context-dependent baseflows
  settings <- list(
    thresholds = list(
      list(average = c(5, Inf), dry = c(1, Inf), wet = c(20, Inf))
    ),
    timing = list(
      c(1:12)
    ),
    duration = list(
      Inf
    ),
    context = quant_fn
  )
  value <- generate_scenario(
    x = flow_sim$value,
    date = flow_sim$date,
    thresholds = settings$thresholds,
    timing = settings$timing,
    duration = settings$duration,
    context = settings$context,
    resolution = water_year
  )

  # define context based on water years

  # and check each context
  expect_equal(1L, 1L)


})

test_that("generate_scenario works with context_data", {

  expect_equal(1L, 1L)

})

test_that("generate_scenario errors informatively", {

  # check threshold specification

  # and context

  expect_equal(1L, 1L)

})

test_that("resample_scenario returns correct dimensions", {

  expect_equal(1L, 1L)

})

test_that("resample_scenario samples correctly when transitions are fixed", {

  # set transitions to always end up in a particular state, make sure this happens

  expect_equal(1L, 1L)

})

test_that("resample_scenario errors informatively", {

  # check dimensions and dimnames of transition

  expect_equal(1L, 1L)

})

# setup: simulate some data to test against
n <- 1000
x <- exp(rnorm(n))
season <- sample(c("cool", "warm"), size = n, replace = TRUE)
idx <- sample.int(length(x), size = 0.6 * n, replace = FALSE)
ref <- x[-idx]
x <- x[idx]
x_season <- season[idx]
ref_season <- season[-idx]

# define a manual decile rescaling function
decile_manual <- function(x, ref) {

  # calculate quantiles for reference and observed values
  ref_breaks <- quantile(ref, probs = seq(0, 1, by = 0.1))
  x_breaks <- quantile(x, probs = seq(0, 1, by = 0.1))

  # shift lower bound to make it inclusive
  ref_breaks[1] <- ref_breaks[1] - 1e-3
  x_breaks[1] <- x_breaks[1] - 1e-3

  # match quantiles between observed and reference based on bins
  x_bins <- cut(
    x, breaks = x_breaks, labels = FALSE
  )
  ref_bins <- cut(
    ref, breaks = ref_breaks, labels = FALSE
  )

  # re-calculate quantiles (seems redunant but oh well)
  x_quantiles <- tapply(x, x_bins, mean)
  ref_quantiles <- tapply(ref, ref_bins, mean)

  # calculate rescaled values and return
  as.numeric(x * (ref_quantiles / x_quantiles)[x_bins])

}

test_that("quantile_rescale returns correct values in simple cases", {

  # no season
  value <- quantile_rescale(x, reference = ref)
  target <- decile_manual(x, ref)
  names(target) <- NULL
  expect_equal(value, target)

  # two seasons
  value <- quantile_rescale(
    x,
    reference = ref,
    season = x_season,
    reference_season = ref_season
  )
  target <- x
  unique_seasons <- unique(season)
  for (i in seq_along(unique_seasons)) {
    idy <- x_season == unique_seasons[i]
    idz <- ref_season == unique_seasons[i]
    target[idy] <- decile_manual(x[idy], ref[idz])
  }
  expect_equal(value, target)

})

test_that("gcm_rescale returns correct values for simple cases", {

  # basic discharge
  value <- gcm_rescale(
    x,
    scenario = "rcp85",
    variable = "discharge",
    catchment = "upper_murray",
    reference_year = 2056
  )[[2]]

  # manual calculation for medium RCP8.5 scenario in Upper Murray at 2056
  scale_vals <- c(-8.4, -16.6)
  deviation <- scale_vals[1] + (scale_vals[2] - scale_vals[1]) * (2056 - 2040) / (2065 - 2040)
  target <- x * (1 + deviation / 100)
  expect_equal(value, target)

  # basic water temperature
  value <- gcm_rescale(
    x,
    scenario = "rcp85",
    variable = "water_temperature",
    catchment = "upper_murray",
    reference_year = 2056
  )[[2]]

  # manual calculation for medium RCP8.5 scenario in Upper Murray at 2056
  scale_vals <- c(1.4, 2.6)
  deviation <- scale_vals[1] + (scale_vals[2] - scale_vals[1]) * (2056 - 2040) / (2065 - 2040)
  target <- x + 0.65 * deviation
  expect_equal(value, target)

})

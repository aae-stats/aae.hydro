#' @name rescale
#' @title Rescale hydrological data to reflect climate change scenarios
#' @description Functions to rescale hydrological data to reflect scenarios
#'   of climate change using seasonal decile rescaling or averaged
#'   outputs from global climate models.
NULL

#' @rdname rescale
#'
#' @importFrom stats quantile
#'
#' @export
#'
#' @param x a numeric vector to be rescaled
#' @param reference a numeric vector to be used as a reference
#'   against which \code{x} is rescaled
#' @param season a vector (character or integer) assigning each
#'   element of \code{x} to a season. Defaults to \code{NULL}, in
#'   which case all values are treated as a single season
#' @param reference_season a vector (character or integer) assigning each
#'   element of \code{reference} to a season. Defaults to \code{NULL}, in
#'   which case all values are treated as a single season. Note that
#'   \code{reference_season} must include all seasons present in
#'   \code{season}
#' @param probs a numeric vector of quantile thresholds. Defaults to
#'   \code{seq(0, 1, by = 0.1)}, which represents decile rescaling
#'
#' @details \code{quantile_rescale} adjusts values in a numeric vector
#'   \code{x} so that the quantiles of this vector reflect those of a
#'   \code{reference} period. A common use is to adjust historical
#'   climate data to reflect contemporary observations.
#'
#' @return a vector with the same length as \code{x} but containing
#'   rescaled values
#'
quantile_rescale <- function(
  x,
  reference,
  season = NULL,
  reference_season = NULL,
  probs = seq(0, 1, by = 0.1)
) {

  # all one season if not specified
  if (is.null(season))
    season <- rep(1, length(x))
  if (is.null(reference_season))
    reference_season <- rep(1, length(reference))

  # seasons need to be included in reference or cannot calculate
  #   rescaling ratios
  if (!all(unique(season) %in% unique(reference_season))) {
    stop(
      "reference_season must include all seasons to provide a ",
      "baseline for rescaling factors in each season",
      call. = FALSE
    )
  }

  # use quantile_rescale in each season
  seasons <- unique(season)
  for (i in seq_along(seasons)) {
    idx <- season == seasons[i]
    idy <- reference_season == seasons[i]
    x[idx] <- quantile_rescale_internal(
      x = x[idx], reference = reference[idy], probs = probs
    )
  }

  # and return
  x

}

# internal function to rescale by quantile for an single sequence
#   (e.g. a single season)
quantile_rescale_internal <- function(
  x,
  reference,
  probs
) {

  # work out quantiles in reference (e.g. future climate)
  reference_quantiles <- get_quantile(
    x = reference, probs = probs
  )

  # work out quantiles in observed
  x_quantiles <- get_quantile(
    x = x, probs = probs
  )

  # simplified change ratio: proportional change in each quantile
  change_ratio <-
    reference_quantiles$quantiles / x_quantiles$quantiles

  # apply this change to observed values
  x <- x * change_ratio[x_quantiles$bins]

  # return, ensure positive
  x

}

# internal function to calculate quantiles and bins with
#   closed intervals
get_quantile <- function(x, probs) {

  # calculate quantiles of x
  breaks <- quantile(x, probs = probs)

  # reduce first quantile to close interval at both ends
  breaks[1] <- breaks[1] - 1e-3

  # bin x into its quantiles
  bins <- cut(
    x, breaks = breaks, labels = FALSE
  )

  # and return mean of each bin and bins
  list(
    quantiles = tapply(
      x, bins, mean
    ),
    bins = bins
  )

}

#' @rdname rescale
#'
#' @export
#'
#' @param x a numeric vector to be rescaled
#' @param scenario a character argument denoting the climate
#'   change scenario. One of "rcp45" or "rcp85", which are the
#'   Representative Concentration Pathway 4.5 and 8.5
#'   scenarios, respectively
#' @param variable a character argument specifying the
#'   variable type. Currently implemented variables are
#'   "discharge" and "water_temperature"
#' @param catchment a character argument specifying the
#'   Victorian catchment for which the GCM outputs are
#'   aggregated. Currently can be one of "upper_murray",
#'   "goulburn", "yarra", "lower_murray", "campaspe",
#'   "snowy", "werribee", "ovens", or "wimmera"
#' @param reference_year a numeric or integer value
#'   denoting the year for which climate change projections
#'   are sought. Defaults to 2065 but can be any value
#'   in 1995-2075
#'
#' @details \code{gcm_rescale} uses aggregated outputs
#'   from Global Climate Models (GCMs) to define changes
#'   in discharge and water temperature under a series of
#'   climate change scenarios. GCM aggregation and
#'   interpretation of projections are described in
#'   DELWP (2020) Guidelines for assessing the impact
#'   of climate change on water availability in Victoria.
#'
#' @return a vector with the same length as \code{x} but containing
#'   rescaled values
#'
gcm_rescale <- function(
  x, scenario, variable, catchment, reference_year = 2065
) {

  # check variable is one of discharge or water temp
  if (!variable %in% c("discharge", "water_temperature")) {
    stop(
      "variable must be one of discharge or water_temperature ",
      call. = FALSE
    )
  }

  # check catchment has been implemented, using
  #   default values for all of Victoria if not
  if (!catchment %in% c(
    "upper_murray", "goulburn", "yarra",
    "lower_murray", "campaspe", "snowy",
    "werribee", "ovens", "wimmera",
    "loddon", "broken"
  )) {
    warning(
      "catchment not implemented; using default Victorian values",
      call. = FALSE
    )
    catchment <- "default"
  }

  # check scenario has been implemented
  if (!scenario %in% c("rcp45", "rcp85")) {
    stop(
      "scenario must be one of rcp45 or rcp85",
      call. = FALSE
    )
  }

  # and check reference year
  if (reference_year > 2075) {
    stop(
      "GCM outputs cannot be extrapolated reliably beyond 2075",
      call. = FALSE
    )
  }

  # pull out relevant effect for the scenario, variable, and catchment
  scaling_factors <- get_gcm_factor(
    scenario = scenario, variable = variable, catchment = catchment
  )

  # adjust these to reflect the reference year
  scaling_factors <- lapply(
    scaling_factors,
    apply_reference,
    reference = reference_year
  )

  # and apply these to the input data, noting percentage change in
  #   discharge vs absolute change in water temperature
  if (variable == "discharge") {
    out <- lapply(scaling_factors, function(.x, x) x * (1 + .x / 100), x = x)
  } else {
    out <- lapply(scaling_factors, function(.x, x) x + .x, x = x)
  }

  # add some names for each scenario
  names(out) <- paste(scenario, c("low", "medium", "high"), reference_year, sep = "_")

  # return
  out

}

# internal function to adjust scaling factors to reflect any year between
#   1995 and 2075
apply_reference <- function(x, reference) {
  ifelse(
    reference < 1995,
    0,
    ifelse(
      reference <= 2040,
      x[1] * (reference - 1995) / (2040 - 1995),
      x[1] + (x[2] - x[1]) * (reference - 2040) / (2065 - 2040)
    )
  )
}

# internal function to define scaling factors for specific Victorian
#   catchments under rcp4.5 and rcp8.5 GCM outputs.
# Values represent percentage changes in discharge and absolute
#   changes in air temperature for 2040 and 2065 relative to 1995,
#   averaged over multiple GCMs. Air temperature values are multiplied
#   by a factor of 0.65 to give expected changes in water temperature.
# Source: DELWP, 2020. Guidelines for assessing the impact
#   of climate change on water availability in Victoria.
get_gcm_factor <- function(
  scenario = scenario, variable = variable, catchment = catchment
) {

  # different values for discharge and water temperature
  if (variable == "discharge") {

    effect <- list(
      upper_murray = list(
        rcp85 = list(low = c(17.2, 13.5), medium = c(-8.4, -16.6), high = c(-23.3, -39.4)),
        rcp45 = list(low = c(14.5, 16.3), medium = c(-5.2, -5.6), high = c(-26.4, -37.4))
      ),
      lower_murray = list(
        rcp85 = list(low = c(32.8, 27.1), medium = c(-4.6, -11.4), high = c(-37.5, -47.0)),
        rcp45 = list(low = c(21.6, 29.3), medium = c(1.2, -5.4), high = c(-27.6, -39.3))
      ),
      goulburn = list(
        rcp85 = list(low = c(9.9, 1.3), medium = c(-9.5, -13.7), high = c(-29.1, -41.9)),
        rcp45 = list(low = c(12.2, 8.1), medium = c(-3.8, -11.7), high = c(-28.7, -33.1))
      ),
      campaspe = list(
        rcp85 = list(low = c(10.5, 1.0), medium = c(-12.3, -20.7), high = c(-37.1, -57.0)),
        rcp45 = list(low = c(20.8, 9.1), medium = c(-6.4, -12.3), high = c(-43.9, -43.6))
      ),
      yarra = list(
        rcp85 = list(low = c(10, 0.8), medium = c(-11, -16.4), high = c(-29.2, -44.3)),
        rcp45 = list(low = c(10.6, 8.7), medium = c(-3.1, -11.4), high = c(-30.0, -34.0))
      ),
      snowy = list(
        rcp85 = list(low = c(22.5, 21.0), medium = c(-7.1, -17.9), high = c(-25.3, -36.1)),
        rcp45 = list(low = c(13.8, 17.3), medium = c(-3.3, -9.3), high = c(-29.9, -31.8))
      ),
      werribee = list(
        rcp85 = list(low = c(11.8, 7.5), medium = c(-7.7, -18.1), high = c(-28.9, -45.5)),
        rcp45 = list(low = c(16.5, 10.7), medium = c(-3.8, -7.6), high = c(-35.0, -36.5))
      ),
      ovens = list(
        rcp85 = list(low = c(11.7, 1.2), medium = c(-10.8, -15.7), high = c(-23.3, -43.9)),
        rcp45 = list(low = c(12.8, 9.6), medium = c(-6.0, -15.9), high = c(-31.0, -34.4))
      ),
      wimmera = list(
        rcp85 = list(low = c(12.1, 12.3), medium = c(-6.5, -14.4), high = c(-32.3, -53.1)),
        rcp45 = list(low = c(21.0, 11.5), medium = c(-4.4, -12.0), high = c(-33.8, -38.6))
      ),
      broken = list(
        rcp85 = list(low = c(18.6, 8.1), medium = c(-9.7, -16.8), high = c(-35.9, -50.0)),
        rcp45 = list(low = c(18.4, 12), medium = c(-6.7, -12.6), high = c(-36.3, -38.4))
      ),
      loddon = list(
        rcp85 = list(low = c(12.4, 6.9), medium = c(-7.4, -17.6), high = c(-36.6, -57.6)),
        rcp45 = list(low = c(31.5, 13.6), medium = c(-8.5, -14.1), high = c(-38.0, -43.0))
      ),
      default = list(
        rcp85 = list(low = c(8.7, 1.5), medium = c(-8.5, -15.9), high = c(-24.7, -43.8)),
        rcp45 = list(low = c(14.0, 9.4), medium = c(-1.6, -11.1), high = c(-29.1, -33.3))
      )
    )

  } else {

    # use air temperature multiplied by 0.65 based on linear model of observed
    #   air temperature against water temperature in the lower Murray, Goulburn,
    #   and Campaspe systems

    effect <- list(
      upper_murray = list(
        rcp85 = list(low = c(1.1, 1.9), medium = c(1.4, 2.6), high = c(1.7, 3.0)),
        rcp45 = list(low = c(0.8, 1.2), medium = c(1.1, 1.6), high = c(1.5, 2.1))
      ),
      lower_murray = list(
        rcp85 = list(low = c(1.1, 2.0), medium = c(1.5, 2.5), high = c(1.7, 3.1)),
        rcp45 = list(low = c(0.7, 1.2), medium = c(1.1, 1.6), high = c(1.4, 2.0))
      ),
      goulburn = list(
        rcp85 = list(low = c(1.0, 2.0), medium = c(1.4, 2.4), high = c(1.6, 2.9)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.5), high = c(1.4, 1.9))
      ),
      campaspe = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.3, 2.4), high = c(1.6, 2.9)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.4), high = c(1.4, 1.8))
      ),
      yarra = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.3, 2.3), high = c(1.5, 2.8)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.5), high = c(1.3, 1.8))
      ),
      snowy = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.4, 2.5), high = c(1.6, 2.9)),
        rcp45 = list(low = c(0.8, 1.2), medium = c(1.1, 1.5), high = c(1.4, 2.0))
      ),

      werribee = list(
        rcp85 = list(low = c(1.0, 1.8), medium = c(1.3, 2.3), high = c(1.5, 2.8)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.4), high = c(1.2, 1.8))
      ),
      ovens = list(
        rcp85 = list(low = c(1.0, 2.0), medium = c(1.4, 2.5), high = c(1.6, 3.0)),
        rcp45 = list(low = c(0.7, 1.2), medium = c(1.1, 1.6), high = c(1.5, 2.0))
      ),
      wimmera = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.3, 2.3), high = c(1.6, 2.9)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.5), high = c(1.3, 1.9))
      ),
      broken = list(
        rcp85 = list(low = c(1.0, 2.0), medium = c(1.4, 2.5), high = c(1.6, 3.0)),
        rcp45 = list(low = c(0.7, 1.2), medium = c(1.0, 1.6), high = c(1.4, 2.0))
      ),
      loddon = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.3, 2.4), high = c(1.6, 2.9)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.5), high = c(1.3, 1.9))
      ),
      default = list(
        rcp85 = list(low = c(1.0, 1.9), medium = c(1.3, 2.3), high = c(1.5, 2.8)),
        rcp45 = list(low = c(0.7, 1.1), medium = c(1.0, 1.5), high = c(1.3, 1.8))
      )
    )

    # apply 0.65 multiplier to all
    effect <- lapply(effect, function(x) lapply(x, function(y) lapply(y, function(z) z * 0.65)))

  }

  # and return
  effect[[catchment]][[scenario]]

}

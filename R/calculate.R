#' @name calculate
#' @title Calculate custom metrics from streamflow data
#' @description Calculate metrics at different temporal resolutions and with
#'   different lags from streamflow or related data downloaded with
#'   \code{fetch_data}.
NULL

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate year
#' @importFrom stats median
#'
#' @param value a time series of streamflow data. \code{calculate} is written
#'   and designed for daily data but may work in other cases
#' @param date a vector of dates in any format accepted by \pkg{lubridate}.
#'   There should be one date for each observation in \code{value}
#' @param resolution a function defining the temporal resolution of calculations.
#'   See details
#' @param fun a function (unquoted) used to calculate the final metric. Defaults
#'   to \code{median} but any R function that returns a single numeric value will
#'   work. The \pkg{aae.data} provides three alternatives: \code{days_below},
#'   \code{days_above}, and \code{rolling_range}
#' @param rescale a \code{list} containing any of four elements that define how
#'   a metric should be rescaled following calculation. Defaults to NULL, in which
#'   case the output metric is not rescaled. The four options are: \code{subset},
#'   \code{fun}, \code{season}, and \code{args}. See details for further information.
#' @param \dots any additional arguments to be passed to \code{fun}
#'
#' @details \code{calculate} is the main function used to calculate metrics from
#'   input time series data. This function is designed to be highly flexible, and is
#'   used in conjunction with a set of \code{resolution} functions that define the
#'   temporal resolution and any subsetting of data within seasons or years. Currently
#'   available \code{resolution} functions are \code{survey}, \code{weekly}, \code{monthly},
#'   \code{annual}, and \code{baseline}. These functions calculate metrics by survey season,
#'   week, month, year, and averaged over all years, respectively. See below for further
#'   details of seasonal subsetting in the \code{survey} and \code{baseline} functions.
#'
#'   The \code{rescale} argument gives the option of rescaling metrics by some other value
#'   calculated from the data, such as a long-term average. This is included to allow
#'   standardisation of metrics among rivers. \code{rescale} is a list that takes any of
#'   four elements. \code{subset} is a numeric vector that defines the years over which a
#'   long-term value should be calculated (defaults to all years included in \code{value}).
#'   \code{fun} defines the function used to calculate a long-term value (defaults to
#'   \code{median}). \code{season} is a numeric vector that defines the months in which
#'   the long-term value should be calculated (defaults to all months, \code{1:12}). \code{args}
#'   is a list containing any further arguments to pass to the \code{fun} defined in \code{rescale}
#'   (e.g. \code{na.rm = TRUE}).
#'
#' @examples
#' \dontrun{
#' # calculate basic metric (annual)
#'
#' # calculate weekly metric
#'
#' # calculate weekly metric with lag
#'
#' # calculate survey metric
#'
#' # example of num_below and num_above
#'
#' # example of rolling range
#'
#' # example of multiple metrics with dplyr
#'
#' }
calculate <- function(value, date, resolution, fun = median, rescale = NULL, ...) {

  # reduce value and date to subset if required, but keep a full copy for rescale
  if (!is.null(resolution$subset)) {
    idx <- identify_subset(date, resolution)
    rescale_date <- date
    rescale_value <- value
    date <- date[idx]
    value <- value[idx]
  }

  # define minimal target, accounting for possible lag
  target <- define_target(date, resolution)

  # work out which dates line up with intervals defined by resolution
  intervals <- lapply(target, define_interval, date = date, settings = resolution)

  # calculate `fun` for each survey year
  out <- sapply(intervals, function(idx, ...) fun(value[idx], ...), ...)

  # check whether we need to rescale the output
  if (!is.null(rescale)) {

    # if so, set some defaults
    rescale_list <- list(
      subset = min(year(rescale_date)):max(year(rescale_date)),
      fun = median,
      season = 1:12,
      args = list()
    )

    # and overwrite these if specified
    rescale_list[names(rescale)]  <- rescale

    # define settings for baseline calculation
    baseline_settings <- baseline(season = rescale_list$season, subset = rescale_list$subset)

    # reduce value and date to subset if required
    if (!is.null(rescale_list$subset)) {
      idx <- identify_subset(rescale_date, baseline_settings)
      rescale_date <- rescale_date[idx]
      rescale_value <- rescale_value[idx]
    }

    # now calculate the target for baseline calculation
    rescale_target <- define_target(rescale_date, settings = baseline_settings)
    rescale_interval <- define_interval(rescale_target, date = rescale_date, settings = baseline_settings)

    # now calculate `fun` for what's left
    baseline <- do.call(rescale_list$fun, c(list(rescale_value[rescale_interval]), rescale_list$args))

    # and rescale output
    out <- out / baseline

  }

  # reformat output? (e.g. convert to data.frame, add date info, add column names)
  out <- format_output(out, target, resolution)

  # return output
  out

}

#'
#' @importFrom lubridate parse_date_time year
#'
# identify which observations sit within a specified subset
identify_subset <- function(date, settings) {

  # reduce date to range defined in `subset`, but keep track of lag
  subset <- parse_date_time(settings$subset, orders = c("y"))
  subset <- subset - settings$lag

  # return
  year(date) %in% year(subset)

}

#'
#' @importFrom lubridate floor_date
#'
# define level at which metric is calculated
define_target <- function(date, settings) {

  # return minimal target based on type
  target <- unique(floor_date(date, unit = settings$unit))

  # return output, dropping first year for survey calculations
  switch(
    settings$type,
    "survey" = target[-1],
    target
  )

}

# parse seasons and convert to correct months
match_season <- function(season, relative = FALSE) {

  print_name <- "season"
  if (relative)
    print_name <- paste0("relative_", print_name)

  switch(
    season,
    "full_year" = 1:12,
    "summer" = 12:14,
    "winter" = 6:8,
    "spring" = 9:11,
    "spawning" = 10:12,
    stop(
      "You've set ", print_name, " = '", season, "' but ", print_name,
      " must be one of full_year, antecedent, summer, winter, spring, or spawning.",
      call. = FALSE
    )
  )

}

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate years is.period
#'
#' @param season define months of year in which to calculate the metric (see details)
#' @param lag lag in calculation. Can be defined in any time period using the methods
#'   in \pkg{lubridate} (see details)
#' @param subset a numeric vector defining years in which the metric should be calculated.
#'   Can be a continuous or discontinuous set of years
#'
#' @details The \code{survey} function defines seasons relative to survey times, so
#'   months 1-12 are Jan-Dec in the year prior to surveys (a year earlier than requested
#'   in \code{calculate}), and months 13-24 are Jan-Dec in the year of surveys.
#'
#'   The \code{baseline} function defines seasons relative to the year of flow measurement,
#'   so months 1-12 are Jan-Dec in the year requested.
#'
#'   The \code{lag} argument defaults to \code{years} for the \code{survey}, \code{monthly},
#'   \code{annual}, and \code{baseline} functions, and defaults to \code{weeks} for the
#'   \code{weekly} function. The \code{lag} argument is also used in \code{rolling_range},
#'   where it defaults to the unit of measurement in the input data (days, in most cases).
#'
survey <- function(season = 1:12, lag = 0, subset = NULL) {

  if (is.character(season))
    season <- match_season(season)

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # if subset is required, don't cut off previous year's flow because it's needed
  if (!is.null(subset))
    subset <- c(min(subset) - 1L, subset)

  # return
  list(type = "survey", season = season, lag = lag, subset = subset, unit = "years")

}

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate weeks is.period
#'
weekly <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. months) but default to weeks
  if (!is.period(lag))
    lag <- weeks(lag)

  # return
  list(type = "weekly", lag = lag, subset = subset, unit = "weeks")

}

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate years is.period
#'
monthly <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. weeks) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # return
  list(type = "monthly", lag = lag, subset = subset, unit = "months")

}

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate years is.period
#'
annual <- function(season = 1:12, lag = 0, subset = NULL) {

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # return
  list(type = "annual", season = season, lag = lag, subset = subset, unit = "years")

}

#' @rdname calculate
#'
#' @export
#'
#' @importFrom lubridate years
#'
baseline <- function(season = 1:12, subset = NULL) {
  list(type = "baseline", season = season, lag = years(0), subset = subset, unit = years(10000))
}

#'
#' @importFrom lubridate year month
#'
# identify which observations fall within a given season and target
define_season <- function(target, date, season) {

  spanning <- TRUE

  if (all(season <= 12)) {
    out <- year(date) == (year(target) - 1L) & month(date) %in% season
    spanning <- FALSE
  }
  if (all(season > 12)) {
    out <- year(date) == year(target) & month(date) %in% (season - 12L)
    spanning <- FALSE
  }
  if (spanning) {
    idx <- season > 12
    out <- (year(date) == year(target) & month(date) %in% (season[idx] - 12L)) | (year(date) == (year(target) - 1L) & month(date) %in% (season[!idx]))
  }

  out

}

#'
#' @importFrom lubridate month
#'
# define observations in each target
define_interval <- function(target, date, settings) {

  switch(settings$type,
         "survey" = define_season(target, date, settings$season),
         "baseline" = month(date) %in% settings$season,
         "annual" = month(date) %in% settings$season & year(date) == year(target),
         floor_date(date, unit = settings$unit) == target)

}

# return an output data.frame with appropriate dates
format_output <- function(x, target, resolution) {

  # add lag back on to make returned dates match requested dates
  target <- target + resolution$lag

  # weekly and monthly metrics need more resolved dates
  date <- switch(
    resolution$type,
    "weekly" = target,
    "monthly" = target,
    year(target)
  )

  # return
  data.frame(date = date, metric = x)

}

#' @rdname calculate
#'
#' @export
#'
#' @param x time series to be tested against threshold in \code{days_below} and
#'   \code{days_above}, or a time series on which to calculate rolling ranges with
#'   the \code{rolling_range} function
#' @param threshold defines level at which observations are considered
#'   low or high
#'
#' @details The \code{days_below} and \code{days_above} functions
#'   calculate the number of days below a defined threshold. Designed
#'   for use within \code{calculate}.
#'
days_below <- function(x, threshold, ...) {
  sum(x < threshold, ...)
}

#' @rdname calculate
#'
#' @export
#'
days_above <- function(x, threshold, ...) {
  sum(x > threshold, ...)
}

# calculate maximum ratio or difference
get_range <- function(x, type = "ratio") {

  out <- NA

  if (type == "ratio") {
    if (any(!is.na(x))) {
      out <- 0
      if (min(x, na.rm = TRUE) > 0)
        out <- max(x, na.rm = TRUE) / min(x, na.rm = TRUE)
    }
  } else {
    if (any(!is.na(x)))
      out <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }

  out

}

#' @rdname calculate
#'
#' @export
#'
#' @param type specify how the difference (range) is calculated in
#'   \code{rolling_range}. Defaults to "ratio" but setting any other
#'   string will switch to absolute difference
#'
#' @details \code{rolling_range} calculates the maximum ratio or
#'   absolute difference in a variable over a specified lag.
#'
rolling_range <- function(x, lag, type = "ratio", ...){

  n <- length(x)

  idx <- sapply(rev(seq_len(lag)) - 1, function(x) rep(x, n))
  idx <- sweep(idx, 1, seq_len(n), "+")
  idx <- ifelse(idx > n, NA, idx)

  df <- matrix(x[idx], nrow = n)

  diff <- apply(df, 1, get_range, type = type)

  max(diff, ...)

}

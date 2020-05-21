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
#' @param resolution a function defining the temporal resolution of
#'   calculations. See \link{resolution}.
#' @param fun a function (unquoted) used to calculate the final metric. Defaults
#'   to \code{median} but any R function that returns a single numeric value
#'   will work. The \pkg{aae.data} provides three additional options:
#'   \code{days_below}, \code{days_above}, and \code{rolling_range}
#' @param rescale a logical or function specifying the how a metric should be
#'   rescaled following calculation. Defaults to NULL, in which case the output
#'   metric is not rescaled. If `TRUE`, then values are rescaled by the
#'   long-term median of the input data. See \link{rescale} for details on
#'   specifying functions with more nuanced calculations of rescale values.
#' @param \dots any additional arguments to be passed to \code{fun}
#'
#' @details \code{calculate} is the main function used to calculate metrics from
#'   input time series data. This function is designed to be highly flexible,
#'   and is used in conjunction with a set of \code{resolution} functions that
#'   define the temporal resolution and any subsetting of data within seasons or
#'   years. Currently available \code{resolution} functions are \code{survey},
#'   \code{weekly}, \code{monthly}, \code{annual}, and \code{baseline}. These
#'   functions calculate metrics by survey season, week, month, year, and
#'   averaged over all years, respectively. See \link{resolution} for further
#'   details of seasonal subsetting in the \code{survey} and \code{baseline}
#'   functions.
#'
#'   The \code{rescale} argument gives the option of rescaling metrics by some
#'   other value calculated from the data, such as a long-term average. This is
#'   included to allow standardisation of metrics among rivers. \code{rescale}
#'   is defined by a function that specifies four elements; the subset of years
#'   over which the scaling value is calculated, the fun used to calculate the
#'   scaling value, the season in which a scaling value is calculated, and any
#'   further arguments used by the function calculating a scaling value. For
#'   details, see \link{rescale}.
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
calculate <- function(value,
                      date,
                      resolution,
                      fun = median,
                      rescale = NULL,
                      ...) {

  # make copy of dates for rescale if needed
  if (!is.null(rescale)) {
    rescale_date <- date
    rescale_value <- value
  }

  # define minimal target, accounting for possible lag
  target <- define_target(date, resolution)

  # work out which dates line up with intervals defined by resolution
  intervals <- lapply(target,
                      define_interval,
                      date = date,
                      settings = resolution)

  # calculate `fun` for each survey year
  out <- sapply(intervals, function(idx, ...) fun(value[idx], ...), ...)

  # check whether we need to rescale the output
  if (!is.null(rescale)) {

    # if so, set some defaults
    if (isTRUE(rescale)) {
      rescale <- by_generic(
        min(year(rescale_date)):max(year(rescale_date)),
        fun = median,
        season = 1:12
      )
    } else {
      if (!is.list(rescale))
        stop("rescale must be one of NULL, TRUE, or an evaluated call",
             " to a rescale function. See ?rescale for details",
             call. = FALSE)
      check_rescale(rescale)
    }

    # define settings for baseline calculation
    baseline_settings <- baseline(season = rescale$season,
                                  subset = rescale$subset)

    # reduce value and date to subset if required
    if (!is.null(rescale$subset)) {
      idx <- identify_subset(rescale_date, baseline_settings)
      rescale_date <- rescale_date[idx]
      rescale_value <- rescale_value[idx]
    }

    # now calculate the target for baseline calculation
    rescale_target <- define_target(rescale_date, settings = baseline_settings)
    rescale_interval <- define_interval(rescale_target,
                                        date = rescale_date,
                                        settings = baseline_settings)

    # now calculate `fun` for what's left
    baseline <- do.call(rescale$fun,
                        c(list(rescale_value[rescale_interval]), rescale$args))

    # and rescale output
    out <- out / baseline

  }

  # reformat output (convert to data.frame, add date info, add column names)
  out <- format_output(out, target, resolution)

  # return output
  out

}

#'
#' @importFrom lubridate parse_date_time year years days interval
#'
# identify which observations sit within a specified subset
identify_subset <- function(date, settings) {

  # reduce date to entirety of each calendar year defined in `subset`
  #   but keep track of lag
  subset <- parse_date_time(settings$subset, orders = c("y"))
  subset <- lapply(
    subset,
    function(x)
      interval(x, x + years(1) - days(1))
  )

  # return
  date %within% subset

}

#'
#' @importFrom lubridate floor_date
#'
# define level at which metric is calculated
define_target <- function(date, settings) {

  # reduce date to subset if required
  if (!is.null(settings$subset)) {
    idx <- identify_subset(date, settings)
    date <- date[idx]
  }

  # return minimal target based on type
  target <- unique(
    floor_date(
      date, unit = settings$unit, week_start = (wday(date[1]) - 1)
    )
  )

  # remove any target points that fall beyond available dates
  #   account for subsetting
  idx <- target %within% interval(min(date), max(date))
  target <- target[idx]

  # add lag at this point
  target <- target - settings$lag

  # return output, dropping first year for survey calculations
  switch(
    settings$type,
    "survey" = target[-1],
    target
  )

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
    out <- (year(date) == year(target) & month(date) %in% (season[idx] - 12L)) |
      (year(date) == (year(target) - 1L) & month(date) %in% (season[!idx]))
  }

  out

}

#'
#' @importFrom lubridate month interval %within%
#'
# define observations in each target
define_interval <- function(target, date, settings) {

  switch(
    settings$type,
    "survey" = define_season(target, date, settings$season),
    "baseline" = month(date) %in% settings$season,
    "annual" = month(date) %in% settings$season &
      year(date) == year(target),
    date %within% interval(target, target + get(settings$unit)(1) - days(1))
  )

}

# return an output data.frame with appropriate dates
format_output <- function(x, target, settings) {

  # account for lag
  target <- target + settings$lag

  # weekly and monthly metrics need more resolved dates
  date <- switch(
    settings$type,
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
#'   \code{days_above}, or a time series on which to calculate rolling ranges
#'   with the \code{rolling_range} function
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
#' @param lag lag in calculation. Can be defined in any time period
#'   using the methods in \pkg{lubridate} (see details)
#'
#' @details \code{rolling_range} calculates the maximum ratio or
#'   absolute difference in a variable over a specified lag.
#'   The \code{lag} argument defaults to the unit of measurement
#'   in the input data (days, in most cases).
#'
rolling_range <- function(x, lag, type = "ratio", ...) {

  n <- length(x)

  idx <- sapply(rev(seq_len(lag)) - 1, function(x) rep(x, n))
  idx <- sweep(idx, 1, seq_len(n), "+")
  idx <- ifelse(idx > n, NA, idx)

  df <- matrix(x[idx], nrow = n)

  diff <- apply(df, 1, get_range, type = type)

  max(diff, ...)

}

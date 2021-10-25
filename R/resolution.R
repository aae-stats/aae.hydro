#' @name resolution
#' @title Functions to define resolutions for metric calculations
#' @description Functions to define temporal resolutions used in
#'   \code{calculate}.
NULL

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
    stop(
      "You've set ",
      print_name, " = '", season,
      "' but ", print_name,
      " must be one of full_year, antecedent,",
      " summer, winter, or spring.",
      call. = FALSE
    )
  )

}

#' @rdname resolution
#'
#' @export
#'
#' @importFrom lubridate years is.period
#'
#' @param season define months of year in which to calculate the
#'   metric (see details)
#' @param lag lag in calculation. Can be defined in any time period
#'   using the methods in \pkg{lubridate} (see details)
#' @param subset a numeric vector defining years in which the
#'   metric should be calculated. Can be a continuous or discontinuous
#'   set of years
#' @param start a vector of start dates specifying when to begin calculating
#'   metrics if this start date is after the first day of a specified season
#'   (see details)
#' @param end a vector of end dates specifying when to end calculation of
#'   metrics if this end date is before the final day of a specified season
#'   (see details)
#'
#' @details The \code{survey} function defines seasons relative to
#'   survey times, so months 1-12 are Jan-Dec in the year prior to
#'   surveys (a year earlier than requested in \code{calculate}),
#'   and months 13-24 are Jan-Dec in the year of surveys.
#'
#'   The \code{baseline} function defines seasons relative to the year
#'   of flow measurement, so months 1-12 are Jan-Dec in the year requested.
#'
#'   The \code{lag} argument defaults to \code{years} for the \code{survey},
#'   \code{monthly}, \code{annual}, and \code{baseline} functions, and
#'   defaults to \code{weeks} for the \code{weekly} function. The \code{lag}
#'   argument is also used in \code{rolling_range}, where it defaults to the
#'   unit of measurement in the input data (days, in most cases).
#'
#'   The \code{truncation} argument is experimental but is intended to
#'   remove the leading or trailing days of a season. A common use-case
#'   is when metrics need to ignore days after some event (e.g. a field
#'   survey), when this event occurs before the final day of a specified season.
#'   Truncation can only be used with \code{survey} and \code{annual}
#'   resolutions. Truncation overwrites all other target date specifications,
#'   so that one value will be returned for each element of \code{start}
#'   or \code{end}.
#'
#' @return a list defining the resolution type, season (if relevant),
#'   lag, subset, and appropriate units for use by \pkg{lubridate}.
#'
survey <- function(
  season = 1:12, lag = 0, subset = NULL, start = NULL, end = NULL
) {

  if (is.character(season))
    season <- match_season(season)

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # if subset is required, don't cut off previous year's flow because
  #   it's needed
  if (!is.null(subset)) {
    subset <- c(min(subset) - 1L, subset)

    # can't use subset and start or end dates because these define
    #   year subsets directly
    if (!is.null(start) | !is.null(end)) {
      warning(
        "subset cannot be specified if start or end are provided; ",
        " defaulting to years provided in start or end",
        call. = FALSE
      )
      subset <- NULL
    }

  }

  # check start and end dates
  if (!is.null(start)) {

    # start needs to be a date
    if (!is.Date(start))
      stop("start must be a Date object", call. = FALSE)

    # and want to make sure season doesn't conflict with start
    month_test <- month(start)
    month_test[month_test < 7L] <- month_test + 12L
    if (!all(month_test %in% season)) {
      warning(
        "start date is outside of specified season; ",
        "start date provided will be used instead of season",
        call. = FALSE
      )
    }

  }
  if (!is.null(end)) {

    # end needs to be a date
    if (!is.Date(end))
      stop("end must be a Date object", call. = FALSE)

    # and want to make sure season doesn't conflict with end
    month_test <- month(end)
    month_test[month_test < 7L] <- month_test + 12L
    if (!all(month_test %in% season)) {
      warning(
        "end date is outside of specified season; ",
        "end date provided will be used instead of season",
        call. = FALSE
      )
    }

  }

  # return
  list(
    type = "survey",
    season = season,
    lag = lag,
    subset = subset,
    start = start,
    end = end,
    unit = "years"
  )

}

#' @rdname resolution
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
  list(
    type = "weekly",
    lag = lag,
    subset = subset,
    unit = "weeks"
  )

}

#' @rdname resolution
#'
#' @export
#'
#' @importFrom lubridate is.period
#'
monthly <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. weeks) but default to months
  if (!is.period(lag))
    lag <- months(lag)

  # return
  list(
    type = "monthly",
    lag = lag,
    subset = subset,
    unit = "months"
  )

}

#' @rdname resolution
#'
#' @export
#'
#' @importFrom lubridate years is.period
#'
annual <- function(
  season = 1:12, lag = 0, subset = NULL, start = NULL, end = NULL
) {

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # can't use subset and start or end dates because these define
  #   year subsets directly
  if (!is.null(subset)) {
    if (!is.null(start) | !is.null(end)) {
      warning(
        "subset cannot be specified if start or end are provided; ",
        " defaulting to years provided in start or end",
        call. = FALSE
      )
      subset <- NULL
    }
  }

  # check start and end dates
  if (!is.null(start)) {

    # start needs to be a date
    if (!is.Date(start))
      stop("start must be a Date object", call. = FALSE)

    # and want to make sure season doesn't conflict with start
    if (!all(month(start) %in% season)) {
      warning(
        "start date is outside of specified season; ",
        "start date provided will be used instead of season",
        call. = FALSE
      )
    }

  }
  if (!is.null(end)) {

    # end needs to be a date
    if (!is.Date(end))
      stop("end must be a Date object", call. = FALSE)

    # and want to make sure season doesn't conflict with end
    if (!all(month(end) %in% season)) {
      warning(
        "end date is outside of specified season; ",
        "end date provided will be used instead of season",
        call. = FALSE
      )
    }

  }

  # return
  list(
    type = "annual",
    season = season,
    lag = lag,
    subset = subset,
    start = start,
    end = end,
    unit = "years"
  )

}

#' @rdname resolution
#'
#' @export
#'
#' @importFrom lubridate years
#'
baseline <- function(season = 1:12, subset = NULL) {
  list(
    type = "baseline",
    season = season,
    lag = years(0),
    subset = subset,
    unit = years(10000)
  )
}

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
    "spawning" = 10:12,
    stop(
      "You've set ",
      print_name, " = '", season,
      "' but ", print_name,
      " must be one of full_year, antecedent,",
      " summer, winter, spring, or spawning.",
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
#' @return a list defining the resolution type, season (if relevant), lag, subset,
#'   and appopriate units for use by \pkg{lubridate}.
#'
survey <- function(season = 1:12, lag = 0, subset = NULL) {

  if (is.character(season))
    season <- match_season(season)

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # if subset is required, don't cut off previous year's flow because
  #   it's needed
  if (!is.null(subset))
    subset <- c(min(subset) - 1L, subset)

  # return
  list(type = "survey",
       season = season,
       lag = lag,
       subset = subset,
       unit = "years")

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
  list(type = "weekly",
       lag = lag,
       subset = subset,
       unit = "weeks")

}

#' @rdname resolution
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
  list(type = "monthly",
       lag = lag,
       subset = subset,
       unit = "months")

}

#' @rdname resolution
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
  list(type = "annual",
       season = season,
       lag = lag,
       subset = subset,
       unit = "years")

}

#' @rdname resolution
#'
#' @export
#'
#' @importFrom lubridate years
#'
baseline <- function(season = 1:12, subset = NULL) {
  list(type = "baseline",
       season = season,
       lag = years(0),
       subset = subset,
       unit = years(10000))
}

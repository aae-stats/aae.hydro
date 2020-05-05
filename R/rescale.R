#' @name rescale
#' @title Functions to define long-term averages to rescale metrics
#' @description Functions to define long-term average values used to
#'   standardise metrics in \code{calculate}.
NULL


#' @rdname rescale
#'
#' @export
#'
#' @importFrom stats median
#'
#' @param subset a numeric vector defining years in which the metric should be calculated.
#'   Can be a continuous or discontinuous set of years
#' @param season define months of year in which to calculate the metric (see details)
#' @param \dots additional arguments passed to \code{median}, \code{mean}, \code{max},
#'   or \code{fun} (e.g. `na.rm = TRUE`)
#'
by_median <- function(subset, season = 1:12, ...) {

  if (missing(subset))
    stop("subset of years must be specified for rescale argument", call. = FALSE)

  args <- list(...)

  list(subset = subset, fun = median, season = season, args = args)

}

#' @rdname rescale
#'
#' @export
#'
by_mean <- function(subset, season = 1:12, ...) {

  if (missing(subset))
    stop("subset of years must be specified for rescale argument", call. = FALSE)

  args <- list(...)

  list(subset = subset, fun = mean, season = season, args = args)

}

#' @rdname rescale
#'
#' @export
#'
by_max <- function(subset, season = 1:12, ...) {

  if (missing(subset))
    stop("subset of years must be specified for rescale argument", call. = FALSE)

  args <- list(...)

  list(subset = subset, fun = max, season = season, args = args)

}

#' @rdname rescale
#'
#' @export
#'
#' @param fun a function (unquoted) used to calculate the final metric. Defaults
#'   to \code{median} but any R function that returns a single numeric value will
#'   work. The \pkg{aae.data} provides three additional options: \code{days_below},
#'   \code{days_above}, and \code{rolling_range}
#'
by_generic <- function(subset, season = 1:12, fun = median, ...) {

  if (missing(subset))
    stop("subset of years must be specified for rescale argument", call. = FALSE)

  args <- list(...)

  list(subset = subset, fun = fun, season = season, args = args)

}

# check rescale arguments are OK
check_rescale <- function(x) {

  ok <- all(c("subset", "fun", "season", "args") %in% names(x))

  if (!ok)
    stop("rescale must be a list containing four arguments. See ?rescale for details", call. = FALSE)

}

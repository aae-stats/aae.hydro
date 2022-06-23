#' @name impute
#' @title Impute missing streamflow or water temperature data
#' @description Functions to impute missing hydrological data using
#'   nearby weather data or rolling means.
NULL

#' @rdname impute
#'
#' @export
#'
#' @param x a numeric vector to be imputed
#' @param date a \code{Date} vector listing dates for each
#'   elements of \code{x}
#' @param threshold an \code{integer} value specifying the
#'   threshold number of \code{NA} values required to trigger
#'   imputation. Defaults to \code{100}
#'
#' @details \code{impute_year} replaces values for calendar
#'   years with many missing values with those from another
#'   year. The level of missingness that is accepted is
#'   specified with \code{threshold}.
#'
#' @return a numeric vector with missing values imputed
#'
impute_year <- function(x, date, threshold = 100) {

  # check x and date have the same length
  if (length(x) != length(date)) {
    stop("x and date must have the same length", call. = FALSE)
  }

  # check whether missing values exceed threshold
  available <- tapply(
    x,
    year(date),
    function(x, thresh) sum(is.na(x)) <= thresh,
    thresh = threshold,
    default = FALSE
  )

  # work out available years and target years
  year_set <- as.numeric(names(available))
  target <- year_set[!available]
  source <- year_set[available]

  # check year lengths and remove any partial years from source
  year_lengths <- sapply(
    sort(unique(year(date))),
    function(.x, .y) sum(year(.y) == .x),
    .y = date
  )
  all_years <- sort(unique(year(date)))
  full_years <- all_years[year_lengths %in% c(366, 365)]
  source <- source[source %in% full_years]

  # fill unavailable years with data from available years,
  #   accounting for leap years
  if (length(target) > 0) {
    x <- resample_discharge_internal(
      x, date, target = target, source = source
    )
  }

  # return
  x

}

# internal function to resample years of discharge, accounting for leap years
#' @importFrom dplyr `%>%` pull filter mutate left_join
#' @importFrom lubridate year month day leap_year
resample_discharge_internal <- function(x, date, target, source) {

  # create a data.frame to resample
  data <- data.frame(
    date = date,
    x = x
  )

  # sample years from source to replace target years,
  #   accounting for leap years
  idx <- leap_year(source)
  idy <- leap_year(target)
  filled_years <- rep(NA, length(target))

  # check if we need to sample with replacement (if there are too many years missing)
  replace_leap <- replace_normal <- FALSE
  if (sum(idx) < sum(idy))
    replace_leap <- TRUE
  if (sum(!idx) < sum(!idy))
    replace_normal <- TRUE

  # and do the actual resampling
  filled_years[idy] <- safe_sample(source[idx], size = sum(idy), replace = replace_leap)
  filled_years[!idy] <- safe_sample(source[!idx], size = sum(!idy), replace = replace_normal)

  # resample observed discharge years and collapse into a single data.frame
  resampled <- do.call(
    rbind,
    lapply(
      filled_years,
      function(.x, data) data %>% filter(year(date) == .x),
      data = data
    )
  )

  # fix the dates on these years to match target years
  resampled <- resampled %>%
    mutate(
      date = ymd(
        paste(
          rep(target, times = ifelse(leap_year(target), 366, 365)),
          month(date),
          day(date),
          sep = "-"
        )
      )
    )

  # combine with target years and arrange chronologically
  data <- data %>%
    left_join(resampled, by = "date", suffix = c("_source", "_target")) %>%
    mutate(x_imputed = ifelse(is.na(x_source), x_target, x_source))

  # and return value only
  data %>% pull(x_imputed)

}

#' @rdname impute
#'
#' @export
#'
#' @param x a numeric vector to be imputed
#' @param n a \code{integer} value specifying the number of
#'   values over which to roll \code{fun}. Defaults to 5
#' @param fun a \code{function} (unquoted) to calculate rolling
#'   values. Defaults to \code{median}, which specifies an
#'   n-day rolling median to fill missing values. \code{fun}
#'   must take an \code{na.rm} argument
#' @param recursive a \code{logical} specifying whether
#'   rolling values should be calculated recursively to fill
#'   gaps longer than \code{n}
#' @param max_iter set maximum number of iterations if
#'   \code{recursive} is \code{TRUE}
#'
#' @details \code{impute_rolling} replaces NA values with
#'   rolling averages (e.g. rolling means or medians) over
#'   the preceding \code{n} days. Common implementations
#'   use this approach to fill short gaps in data, such as
#'   rolling means or medians over 5-10 days.
#'
#'   The \code{impute_rolling} function can optionally be
#'   applied recursively, which fills gaps in data repeatedly
#'   and allows, for example, a 5-day rolling mean to fill
#'   gaps longer than 5 days.
#'
#' @return a numeric vector with missing values imputed
#'
impute_rolling <- function(
  x, n = 5, fun = median, recursive = FALSE, max_iter = 20, ...
) {

  # only need to do this if some but not all values are NA
  if (any(is.na(x)) & !all(is.na(x))) {

    # is this applied repeatedly?
    if (recursive) {

      # initialise counter to keep track of recursive calcs
      counter <- 1
      while(any(is.na(x)) & counter < (max_iter + 1)) {

        # calculate rolling values
        x <- impute_rolling_internal(x = x, n = n, fun = fun, ...)

        # increment counter
        counter <- counter + 1

      }

    } else {

      # if not, it's easy
      x <- impute_rolling_internal(x = x, n = n, fun = fun, ...)

    }

  }

  # return
  x

}

# internal function to specify rolling imputation
impute_rolling_internal <- function(x, n, fun, ...) {

  # create a matrix with one column for each "lag" (n value)
  xlen <- length(x)
  idx <- sapply(1 - rev(seq_len(n)), function(x) rep(x, xlen))
  idx <- sweep(idx, 1, seq_len(xlen), "+")
  idx <- ifelse(idx < 1, NA, idx)
  df <- matrix(x[idx], nrow = xlen)

  # calculate fun over rows of this matrix
  mean_val <- apply(df, 1, fun, na.rm = TRUE, ...)

  # and fill missing values in x
  x[is.na(x)] <- mean_val[is.na(x)]

  # return
  x

}

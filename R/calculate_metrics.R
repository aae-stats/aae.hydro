# functions to calculate metrics from streamflow or related data downloaded with \link{\code{fetch_flow}}.

# main function to calculate metrics at a given resolution
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

# internal function to identify which data sit within a specified subset
identify_subset <- function(date, settings) {

  # reduce date to range defined in `subset`, but keep track of lag
  subset <- parse_date_time(settings$subset, order = c("y"))
  subset <- subset - settings$lag

  # return
  year(date) %in% year(subset)

}

# internal function to define level at which metric is calculated
define_target <- function(date, settings) {

  # deal with lag
  date <- date - settings$lag

  # return minimal target based on type
  target <- unique(floor_date(date, unit = settings$unit))

  # return output, dropping first year for survey calculations
  switch(
    settings$type,
    "survey" = target[-1],
    target
  )

}

# internal function to parse seasons and convert to correct months
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

# define details of survey-level metric calculations
survey <- function(season = 1:12, lag = 0, subset = NULL) {

  if (is.character(season))
    season <- match_season(season)

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # if subset is required, don't cut off previous year's flow because it's needed
  if (!is.null(subset))
    subset[1] <- subset[1] - 1L

  # return
  list(type = "survey", season = season, lag = lag, subset = subset, unit = "years")

}

# define details of weekly metric calculations
weekly <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. months) but default to weeks
  if (!is.period(lag))
    lag <- weeks(lag)

  # return
  list(type = "weekly", lag = lag, subset = subset, unit = "weeks")

}

# define details of monthly metric calculations
monthly <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. weeks) but default to months
  if (!is.period(lag))
    lag <- months(lag)

  # return
  list(type = "monthly", lag = lag, subset = subset, unit = "months")

}

# define details of annual metric calculations
annual <- function(lag = 0, subset = NULL) {

  # user can pass a different period (e.g. months) but default to years
  if (!is.period(lag))
    lag <- years(lag)

  # return
  list(type = "annual", lag = lag, subset = subset, unit = "years")

}

# define details of baseline metric calculations
baseline <- function(season = 1:12, subset = NULL) {
  list(type = "baseline", season = season, lag = years(0), subset = subset, unit = years(10000))
}

# internal function to identify which observations fall within a given season and target
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

# internal function to define observations in each target
define_interval <- function(target, date, settings) {

  switch(settings$type,
         "survey" = define_season(target, date, settings$season),
         "baseline" = month(date) %in% settings$season,
         floor_date(date, unit = settings$unit) == target)

}

# internal function to return an output data.frame with appropriate dates
format_output <- function(x, target, resolution) {

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

# function to calculate number of days below a threshold
days_below <- function(x, threshold, ...) {
  sum(x < threshold, ...)
}

# function to calculate number of days about a threshold
days_above <- function(x, threshold, ...) {
  sum(x > threshold, ...)
}

# internal functon to calculate maximum ratio or difference (used in `rolling_range()`)
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

# calculate maximum ratio or absolute difference in a variable over a specified lag
rolling_range <- function(x, lag, type = "ratio", ...){

  n <- length(x)

  idx <- sapply(rev(seq_len(lag)) - 1, function(x) rep(x, n))
  idx <- sweep(idx, 1, seq_len(n), "+")
  idx <- ifelse(idx > n, NA, idx)

  df <- matrix(x[idx], nrow = n)

  diff <- apply(df, 1, get_range, type = type)

  max(diff, na.rm = TRUE)

}

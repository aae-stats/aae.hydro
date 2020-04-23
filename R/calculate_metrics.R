# functions to calculate flow metrics from streamflow data downloaded with \link{\code{fetch_flow}}.

define_target <- function(date, settings) {

  # deal with subset

  ## PROBABLY shift this to define_interval?
  ## seems to be some repitition in here
  if (!is.null(settings$subset)) {
    if (settings$type == "baseline") {
      subset <- 1:12
      months %in% subset
    } else {
      subset <- parse_date_time(settings$subset, order = c("y", "dmy", "ymd", "my", "ym"))
      subset <- subset[1] %--% subset[2]
      date <- date[date %within% subset]
    }
  }

  # deal with lag
  date <- date - settings$lag

  # return minimal target based on type
  switch(settings$type,
         "survey" = unique(floor_date(date, unit = "years")),
         "weekly" = unique(floor_date(date, unit = "weeks")),
         "monthly" = unique(floor_date(date, unit = "months")),
         "annual" = unique(floor_date(date, unit = "years")),
         "baseline" = dmy("01012050"))

}

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
    stop("You've set ", print_name,
         " = '", season,
         "' but ", print_name,
         " must be one of full_year, antecedent, summer, winter, spring, or spawning.",
         call. = FALSE)
  )

}

survey <- function(season, lag = 0, subset = NULL) {

  if (is.character(season))
    season <- match_season(season)

  list(type = "survey", season = season, lag = years(lag), subset = subset)

}

weekly <- function(lag = 0, subset = NULL) {
  list(type = "weekly", lag = weeks(lag), subset = subset, unit = "weeks")
}

monthly <- function(lag = 0, subset = NULL) {
  list(type = "monthly", lag = months(lag), subset = subset, unit = "weeks")
}

annual <- function(lag = 0, subset = NULL) {
  list(type = "annual", lag = years(lag), subset = subset, unit = "weeks")
}

baseline <- function(lag = 0, subset = NULL) {
  list(type = "baseline", unit = years(10000))
}

define_season <- function(target, date, season) {

  spanning <- TRUE

  if (all(season) <= 12) {
    out <- year(date) == (year(target) - 1L) & month(date) %in% season
    spanning <- FALSE
  }
  if (all(season) > 12) {
    out <- year(date) == year(target) & month(date) %in% (season - 12L)
    spanning <- FALSE
  }
  if (spanning) {
    idx <- season > 12
    out <- (year(date) == year(target) & month(date) %in% (season[idx] - 12L)) | (year(date) == (year(target) - 1L) & month(date) %in% (season[!idx]))
  }

  out

}

define_interval(target, date, settings) {

  switch(settings$type,
         "survey" = define_season(target, date, settings$season),
         floor_date(date, unit = settings$unit) == target)

}

calculate(value, date, resolution, fun, rescale = NULL, ...) {

  # define minimal target, accounting for possible subset and lag
  targets <- define_target(date, resolution)

  # work out which dates line up with intervals defined by resolution
  intervals <- lapply(targets, define_interval, date = date, settings = resolution)

  # calculate `fun` for each survey year
  out <- sapply(intervals, function(idx, ...) fun(value[idx], ...), ...)

  # check whether we need to rescale the output
  if (!is.null(rescale)) {

    # if so, set some defaults
    rescale_list <- list(
      subset = subset,
      fun = median,
      season = 1:12
    )

    # and overwrite these if specified
    rescale_list[names(rescale)]  <- rescale

    # now calculate the target for baseline calculation
    rescale_target <- define_target(settings = baseline(subset = rescale_list$subset))
    rescale_interval <- define_interval(rescale_target, date = date, settings = baseline(subset = rescale_list$subset))

    # copy rescale list but remove elements that `fun` can't process
    rescale_copy <- rescale_list
    rescale_copy$subset <- NULL
    rescale_copy$season <- NULL
    rescale_copy$fun <- NULL

    # now calculate `fun` for what's left
    baseline <- do.call(rescale_list$fun, c(value[rescale_interval], rescale_copy))

    # and rescale output
    out <- out / baseline

  }

  # reformat output? (e.g. convert to data.frame, add date info, add column names)

  # return output
  out

}

# vectorised function to calculate flow metrics for multiple survey years
calc_metric <- function(value, dates,
                        survey_years,
                        season,
                        fun = median,
                        lag = 0,
                        aggregate_years = FALSE,
                        relative_to = NULL,
                        relative_season = NULL,
                        relative_fun = median,
                        ...) {

  # is the season valid?
  check_season(season, relative = FALSE)

  # do we need to aggregate values over multiple years?
  if (aggregate_years) {
    aggregate_years <- survey_years
    survey_years <- 2050
  } else {
    aggregate_years <- NULL
  }

  # account for the time lag
  survey_years <- survey_years - lag

  # work out which dates line up with each survey year and the chosen season
  targets <- lapply(survey_years, define_interval, dates = dates, season = season, aggregate_years = aggregate_years)

  # calculate `fun` for each survey year
  out <- sapply(targets, function(idx, ...) fun(value[idx], ...), ...)

  # do we want to standardise relative to some years?
  if (!is.null(relative_to)) {

    # if so, need to specify how to calculate standardisation
    if (is.null(relative_fun))
      relative_fun <- fun

    # is the season defined? If not, default to all months in all years
    if (is.null(relative_season)) {
      relative_season <- "full_year"
    }

    # check season is OK
    check_season(relative_season, relative = TRUE)

    # which years do we want to standardise against?
    relative_target <- define_interval(2050, dates, season = relative_season, aggregate_years = relative_to)

    # calculate value in these years
    longterm <- relative_fun(value[relative_target], ...)

    # and divide metric by this value for all survey years
    out <- out / longterm

  }

  # flatten to single value if spanning all years
  if (length(grep("all", season)) > 0)
    out <- unique(out)

  # return
  out

}

# function to calculate low flow days
days_below <- function(x, threshold, ...) {
  sum(x < threshold, ...)
}

# function to calculate high flow days
days_above <- function(x, threshold, ...) {
  sum(x > threshold, ...)
}

# internal functon to calculate maximum ratio or difference
#   used in `rolling_range()`
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

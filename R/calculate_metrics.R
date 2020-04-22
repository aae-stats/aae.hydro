# functions to calculate flow metrics from streamflow data downloaded with \link{\code{fetch_flow}}.


## SEEMS like a group_by(river, year, variable_code) would automatically do everything in one line?
##   BUT some things need data over multiple years, so need to handle that more carefully.

calculate_metrics <- function(data, years = NULL, funs = list(), options = list()) {

  opt <- list(
    na_thresh = 0.2,
    year_lag = 0
  )
  opt[names(options)] <- options

  systems <- unique(data$site_code)
  if (is.null(years))
    years = unique(year(data$date_formatted))

  # which months correspond to which seasons?
  month_ids <- define_seasons()

  # populate with individual flow funtions
  funs_list <- list(
    spawn_flow = NULL,
    spring_flow = NULL
  )
  funs_list[names(funs)] <- funs

  # check which metrics can be calculated
  flow_exists <- any(c("141.00", "141.50") %in% data$variable_code)
  temp_exists <- any(data$variable_code == "450.00")
  depth_exists <- any(data$variable_code == "100.00")


}

# individual functions for flow metrics
spawning_variability <- function(x, target) {

  NULL
  # out$spawning_variability <- rolling_range(flow_vector[prior & spawning], 3)

}

define_target <- function(date, year, season) {

  contemporary <- year(date) %in% year
  prior <- year(date) %in% (year - 1L)
  antecedent <- year(date) %in% (year - 2L)
  spanning <- date %within% interval(dmy(paste0("0107", year - 1L)), dmy(paste0("3006", year)))

  full_year <- month(date) %in% c(1:12)
  summer <- month(date) %in% c(12, 1:3)
  autumn <- month(date) %in% c(3:5)
  winter <- month(date) %in% c(5:7)
  spring <- month(date) %in% c(9:11)
  spawning <- month(date) %in% c(10:12)


}

# is this vectorised? Ideally, it would be.
calc_metric <- function(x, survey_year, season, fun = median, relative_to = NULL, na.rm = TRUE, ...) {

  target <- define_target(x$date_formatted, survey_year, season)

  out <- fun(x$value[target], na.rm = na.rm, ...)

  if (!is.null(relative_to)) {

    relative_target <- NULL
    longterm <- x$value[relative_target]
    out <- out / longterm

  }

  out

}

spring_median <- function(x, target, relative_to = NULL) {

  NULL
  # out$median_spring <- median(flow_vector[prior & spring], na.rm = TRUE)
  # lt_med <- median(flow_all_years[full_year_all_years], na.rm = TRUE)
  # out$prop_spring_lt <- out$median_spring / lt_med

}

summer_median <- function(x, target, relative_to = NULL) {

  NULL
  # out$median_summer <- median(flow_vector[spanning & summer], na.rm = TRUE)

}

winter_median <- function(x, target, relative_to = NULL) {

  NULL
  # out$median_winter <- median(flow_vector[contemporary & cool_season], na.rm = TRUE)

}

antecedent_max <- function(x, target, relative_to = NULL) {

  NULL
  # out$max_antecedent <- max(flow_vector[antecedent], na.rm = TRUE)

}

low_flow_days <- function(x, target, relative_to = NULL, p = 0.1) {

  NULL
  # cool_season_all_years <- month(flow_sub$Event_Date) %in% month_ids$cooler_months
  # lt_qcool <- quantile(flow_all_years[cool_season_all_years], p = 0.1, na.rm = TRUE)
  # out$number_low_days <- sum(flow_vector[prior & cool_season] < lt_qcool)

}

# can be used for any variable (temp/depth/discharge)
spawning_mean <- function(x, target, relative_to = NULL) {

  NULL
  # temperature calculations filtered to <= na_thresh missing vlaues
  # out$spawning_temp <- mean(temp_prior, na.rm = TRUE)

}



# add docs
# setup namespace through roxygen
# setup examples
# setup basic tests
# add rigorous tests, download some known values to test against
# add separate script to pull out flow metrics (work out how to handle dates)
# maybe add option to pull flow metrics directly (by calling fetch_flow internally)?

library(httr)
library(lubridate)
library(jsonlite)

## variable notes:
## varfrom gives raw data downloaded, 100.00 for water level
## varto gives converted variable, 100.00 to keep water level but 141.00 to convert to discharge
## so could set it up to:
## 1. check for true discharage (141.00-141.00)
## 2. fill with calculated if available (100.00-141.00)
## 3. fill with modelled if available (141.50-141.50)

# notes to keep
## var_list overrides varfrom/varto combo
## data_type is operation on data (observations are instantaneous)
## multiplier gives an interval multiplier, e.g., 2 means interval = 2 * interval
## need informative errors, especially when data are partially available

# basic tests

# one variable
test <- fetch_flow(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
                   variables = "streamflow")

# two variables
test <- fetch_flow(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
                   variables = c("streamflow", "watertemp"))

# one variable repeated twice
test <- fetch_flow(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
                   variables = c("streamflow", "flow"))

# one variable one site
test <- fetch_flow(sites = c(409202), start = "2019-04-02", end = "2019-04-24",
                   variables = c("streamflow"))

# three variables two sites
test <- fetch_flow(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
                   variables = c("depth", "streamflow", "temp"))

# different date ranges: overlapping, unavailable, etc.


# return a list of variables available at each site, with date ranges
list_variables <- function(sites, settings = list()) {

  # initialise some basic settings
  sett_list <- list(
    data_source = "A",
    return_type = "hash"
  )
  sett_list[names(settings)] <- settings

  # create a query string that `GET` can interpret
  query <- paste0(
    '{"params":{"site_list":"',
    paste0(sites, collapse = ","),
    '","datasource":"',
    sett_list$data_source,
    '"},"function":"get_variable_list","version":"1"}&ver=2'
  )

  # send query
  response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                  query = query)

  # convert output into a readable table
  output <- format_JSON_vars(response)

  # return
  output

}

# return a list of datasources available at each site
list_datasources <- function(sites) {

  # create a query string that `GET` can interpret
  query <- paste0(
    '{"params":{"site_list":"',
    paste0(sites, collapse = ","),
    '"},"function":"get_datasources_by_site","version":"1"}&ver=2'
  )

  # send query
  response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                  query = query)

  # return as unformatted list
  fromJSON(content(response, as = "text"))$return$sites

}

# check whether a variable is available at a site in some given dates
check_available <- function(site, start, end, variable, settings = list()) {

  # initialise some basic settings
  sett_list <- list(
    data_source = "A",
    return_type = "hash"
  )
  sett_list[names(settings)] <- settings

  # which variables are available at that site?
  var_list <- list_variables(site, sett_list)

  # is the variable in the database at that site?
  var_row <- var_list[var_list$variable_code == variable, ]

  # if it's not there, return FALSE
  out <- FALSE

  # otherwise we want to check that it overlaps with our target date range
  if (nrow(var_row) > 0) {

    # will automatically create multiple intervals for multiple dates
    database_interval <-interval(var_row$start_date, var_row$end_date)
    requested_interval <- interval(ymd_hms(start), ymd_hms(end))

    # do we have data for at least part of the requested interval?
    out <- int_overlaps(requested_interval, database_interval)
    ## OR could also check whether it's completely covered?
    ## requested_interval %within% database_interval

  }

  # return
  out

}

# turn dates from a range of formats into a consistent format for JSON calls
parse_dates <- function(start, end) {

  # try and convert whatever we've been given to some sort of date
  start <- parse_date_time(start, orders = c("ymd", "dmy", "y", "ymd_HMS", "ymd_HM"))
  end <- parse_date_time(end, orders = c("ymd", "dmy", "y", "ymd_HMS", "ymd_HM"))

  # turn it to a string so we can work with regex
  start <- as.character(start)
  end <- as.character(end)

  # remove non-numerics
  start <- gsub("-|:| ", "", start)
  end <- gsub("-|:| ", "", end)

  # just in case the HMS get dropped, add zeros in their place
  if (nchar(start) == 8)
    start <- paste0(start, paste0(rep(0, 6), collapse = ""))
  if (nchar(end) == 8)
    end <- paste0(end, paste0(rep(0, 6), collapse = ""))

  # return character and date format?
  list(start = start, end = end)
}

# convert variable names to codes used in vicwater database
parse_variables <- function(variables, sites, start, end, settings) {

  # create a vector with codes relevant to each variable
  var_list <- rep(NA, length(variables))
  for (i in seq_along(variables)) {

    # pull out a tmp var to play with
    var_tmp <- variables[i]

    # standardise names as much as possible
    var_tmp <- gsub(" |_", "", var_tmp)

    # switch out for database variable codes
    var_list[i] <- switch(
      var_tmp,
      "depth" = "100.00",
      "waterdepth" = "100.00",
      "discharge" = "141.00",
      "flow" = "141.00",
      "streamflow" = "141.00",
      "temperature" = "450.00",
      "watertemperature" = "450.00",
      "temp" = "450.00",
      "watertemp" = "450.00",
      "conductivity" = "820.00",
      "cond" = "820.00",
      var_tmp)
  }

  # create vectors of var_from and var_to to be cycled over if making multiple_queries
  varfrom <- var_list
  varto <- var_list

  # check whether variables exist in data
  available <- matrix(NA, nrow = length(sites), ncol = length(var_list))
  for (i in seq_along(sites)) {
    for (j in seq_along(var_list)) {
      available[i, j] <- check_available(sites[i], start, end, var_list[j], settings)
    }
  }

  # summarise this info by variable (check across all sites)
  missing_by_var <- apply(available, 2, function(x) any(!x))

  multiple_queries <- FALSE
  if (any(missing_by_var)) {

    # check whether we need to calculate streamflow from depth, if so, can't use var_list
    if ("141.00" %in% varfrom[missing_by_var]) {
      multiple_queries <- TRUE
      idx <- varfrom == "141.00"
      varfrom[idx] <- "100.00"
      missing_by_var[idx] <- FALSE
    }

    # otherwise we are out of luck
    if (any(missing_by_var)) {
      stop("No data available for ",
           paste0(variables[missing_by_var], collapse = ", "),
           ". Use `list_variables(c(",
           paste(sites, collapse = ", "),
           "))` to determine whether data are available for",
           " a different date range or subset of sites.",
           call. = FALSE)
    }

  }

  # collapse var_list into a single string to pass to the JSON call
  var_list <- paste0(var_list, collapse = ",")

  # turn off var_list if we need to run multiple queries
  if (multiple_queries)
    var_list <- NULL

  # return
  list(varfrom = varfrom, varto = varto, var_list = var_list, multiple_queries = multiple_queries)

}

# check that the data aggregation settings are OK
check_aggregation <- function(x) {

  # fix some common name mistakes
  x$interval <- switch(
    x$interval,
    "daily" = "day",
    "monthly" = "month",
    "yearly" = "year",
    "hourly" = "hour",
    x$interval
  )

  # is the interval setting ok?
  interval_ok <- x$interval %in%
    c("day", "month", "year", "hour", "minute", "second", "period")

  # is the data_type possible?
  data_type_ok <- x$data_type %in%
    c("mean", "maxmin", "max", "min", "start", "end", "first", "last",
      "tot", "point", "partialtot", "cum")

  # warn if these are not ok
  if (!interval_ok)
    stop("interval must be one of day, month, year, hour, minute, second, or period", call. = FALSE)
  if (!data_type_ok)
    stop("data_type must be one of mean, maxmin, max, min, start, end, first, last, tot, point, partialtot, cum", call. = FALSE)

  # return corrected names if it didn't error first
  x

}

# format variable lists returned in JSON format
format_JSON_vars <- function(response) {

  # pull out the results
  output_list <- fromJSON(content(response, as = "text"))

  # extract the relevant info into separate objects
  site_details <- output_list$return$sites$site_details
  site_names <- output_list$return$sites$site
  variable_info <- output_list$return$sites$variables

  # how many variables do we have?
  nvar <- sapply(variable_info, nrow)

  # put it all back into one data.frame
  out <- do.call(rbind, variable_info)
  out$site <- rep(site_names, times = nvar)
  out$site_name <- rep(site_details$name, times = nvar)

  # add some neatly formatted dates
  out$start_date <- ymd_hms(out$period_start)
  out$end_date <- ymd_hms(out$period_end)

  # reorder and rename some columns
  out <- out[, c("site_name", "site", "name", "units", "start_date", "end_date", "variable", "subdesc", "period_start", "period_end")]
  colnames(out) <- c("site_name", "site_code", "variable_name", "units", "start_date", "end_date", "variable_code", "details", "period_start", "period_end")

  # and return
  out

}

# format flow data returned in JSON format
format_JSON_flow <- function(response) {

  # turn the output into something readable
  output <- fromJSON(content(response, as = "text"))$return

  # now break it down into components
  raw_data <- output$traces$trace
  n_obs <- sapply(raw_data, nrow)
  site_info <- output$traces$site_details
  varfrom_info <- output$traces$varfrom_details
  varto_info <- output$traces$varto_details

  # and put it back together
  out <- do.call(rbind, raw_data)
  colnames(out) <- c("value", "date", "quality_code")

  # tidy the dates a bit
  out$date <- format(out$date, scientific = FALSE)
  out$date_formatted <- ymd_hms(out$date)

  # and add info on sites, variables, and qc
  out$site_name <- rep(site_info$name, times = n_obs)
  out$site_code <- rep(output$traces$site, times = n_obs)
  out$variable_name <- rep(varto_info$name, times = n_obs)
  out$variable_code <- rep(varto_info$variable, times = n_obs)
  out$units <- rep(varto_info$units, times = n_obs)
  out$quality_reference <- output$traces$quality_codes[rep(seq_along(raw_data), times = n_obs), ]


  # return
  out

}

# expand data set to include rows for unavailable dates
expand_missing <- function(data, start, end, variables) {

  # for each variable, check whether it's fully there from start to end
  #   variable_complete <- logical_vector

  #  use data$date_formatted to work it out (with interval calcs as needed)

  # for vars %in% !variable_complete
  #    identify missing dates, add rows.
  #    insert at correct point for that variable (probably just rbind then sort at end)
  #    data$quality_reference <- "missing"
  #    data$value <- NA
  #    all else the same

}

# grab vicwater data on one or more variables for a site or set of sites
fetch_flow <- function(
  sites,
  start,
  end,
  variables,
  include_missing = FALSE,
  aggregation = list(),
  settings = list()
) {

  # set default aggregation settings
  aggr_list <- list(
    interval = "daily",
    data_type = "mean",
    multiplier = "1"
  )
  aggr_list[names(aggregation)] <- aggregation

  # are these aggregation settings ok?
  aggr_list <- check_aggregation(aggr_list)

  # default data settings
  sett_list <- list(
    data_source = "A",
    return_type = "hash"
  )
  sett_list[names(settings)] <- settings

  # pull out
  parsed_dates <- parse_dates(start, end)
  parsed_vars <- parse_variables(variables, sites, parsed_dates$start, parsed_dates$end, sett_list)

  # if we can just run all variables at once
  if (!parsed_vars$multiple_queries) {

    # set up a query statement
    query <- list(
      "site_list" = paste0(sites, collapse = ","),
      "start_time" = parsed_dates$start,
      "end_time" = parsed_dates$end,
      "varfrom" = parsed_vars$varfrom,
      "varto" = parsed_vars$varto,
      "var_list" = parsed_vars$var_list,
      "interval" = aggr_list$interval,
      "data_type" = aggr_list$data_type,
      "multiplier" = aggr_list$multiplier,
      "datasource" = sett_list$data_source,
      "function" = "get_ts_traces",
      "version" = "3",
      "ver" = "2",
      "return_type" = sett_list$return_type
    )

    # send it off
    response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                    query = query)

    # extract and reformat the output
    output <- format_JSON_flow(response)

  } else {

    # initialise an empty list object to store outputs
    output <- vector("list", length = length(parsed_vars$varfrom))

    # we need to do it separately for each variable
    for (i in seq_along(parsed_vars$varfrom)) {

      # set up a query statement
      query <- list(
        "site_list" = paste0(sites, collapse = ","),
        "start_time" = parsed_dates$start,
        "end_time" = parsed_dates$end,
        "varfrom" = parsed_vars$varfrom[i],
        "varto" = parsed_vars$varto[i],
        "interval" = aggr_list$interval,
        "data_type" = aggr_list$data_type,
        "multiplier" = aggr_list$multiplier,
        "datasource" = sett_list$data_source,
        "function" = "get_ts_traces",
        "version" = "3",
        "ver" = "2",
        "return_type" = sett_list$return_type
      )

      # send it off
      response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                      query = query)

      # extract and reformat the output
      output[[i]] <- format_JSON_flow(response)

    }

    # flatten this into a single data.frame
    output <- do.call(rbind, output)

  }

  # expand around missing observations
  # if (include_missing)
    # output <- expand_missing(output, start, end)

  # and return
  output

}

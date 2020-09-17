#' @name query_database
#' @title Interact with WMIS database from R
#' @description Functions to prepare and submit queries to the Victorian
#'   Water Measurement Information System (WMIS) database.
NULL

#' @rdname query_database
#'
#' @export
#'
#' @importFrom httr GET
#'
#' @param sites a character or numeric vector of site codes (gauge numbers)
#' @param data_source database from which data are downloaded. Defaults to
#'   NULL, which will check all available data sources. The
#'   \code{list_datasources} function lists all available data_sources for
#'   a given site
#' @param state state of interest; one of Victoria, New South Wales, or
#'   Queensland
#'
#' @details \code{list_variables} queries the WMIS database to identify all
#'   variables avaiable at a given site.
#'
#' @return a \code{data.frame} containing information on variables and
#'   data sources at each site.
#'
#' @examples
#' \dontrun{
#'   # list all variables at three sites
#'   list_variables(
#'     sites = c("405232", "406276", "406278"),
#'   )
#'
#'   # list all variables at three sites from telemetry data
#'   list_variables(
#'     sites = c("405232", "406276", "406278"),
#'     data_source = "TELEM"
#'   )
#'
#'   # list all data sources at three sites
#'   list_datasources(
#'     sites = c("405232", "406276", "406278"),
#'   )
#' }
list_variables <- function(sites, data_source = NULL, state = "vic") {

  # parse state
  state <- parse_state(state)

  # find all available datasources by site
  if (is.null(data_source))
    data_source <- list_datasources(sites, state = state)$datasources

  data_source <- unique(unlist(data_source))

  # run through and pull out variables for each
  output <- NULL
  for (i in seq_along(data_source)) {

    # create a query string that `GET` can interpret
    query <- paste0(
      '{"params":{"site_list":"',
      paste0(sites, collapse = ","),
      '","datasource":"',
      data_source[i],
      '"},"function":"get_variable_list","version":"1"}&ver=2'
    )

    # send query
    response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                    query = query)

    # convert output into a readable table
    tmp <- format_json_vars(response)

    # add a column with datasource identifier
    tmp$datasource <- rep(data_source[i], nrow(tmp))

    # combine into one big output matrix
    output <- rbind(output, tmp)

  }

  # return
  output

}

#' @rdname query_database
#'
#' @export
#'
#' @importFrom httr GET
#'
#' @details \code{list_datasources} queries the WMIS database to identify all
#'   data sources avaiable at a given site.
#'
#' @return a \code{list} containing information on data sources at each site.
#'
list_datasources <- function(sites, state = "vic") {

  # parse state
  state <- parse_state(state)

  # get database address
  address <- get_address(state)

  # create a query string that `GET` can interpret
  query <- paste0(
    '{"params":{"site_list":"',
    paste0(sites, collapse = ","),
    '"},"function":"get_datasources_by_site","version":"1"}&ver=2'
  )

  # send query
  response <- GET(address, query = query)

  # return as unformatted list
  fromJSON(content(response, as = "text"))$return$sites

}

#'
#' @importFrom lubridate ymd_hms %--% %within% int_overlaps
#'
# check whether a variable is available at a site in some given dates
check_available <- function(sites, start, end, variables, data_source, state) {

  # which variables are available at that site?
  var_list <- list_variables(sites, data_source = data_source, state = state)

  # work out how many checks we need to do
  nsite <- length(sites)
  nvar <- length(variables)

  # default to FALSE
  partial <- matrix(FALSE, nrow = nsite, ncol = nvar)
  complete <- matrix(FALSE, nrow = nsite, ncol = nvar)

  # and then update one at a time
  for (i in seq_len(nsite)) {

    for (j in seq_len(nvar)) {

      # is the variable in the database at that site?
      row_id <- var_list$site_code == sites[i] &
        var_list$variable_code == variables[j]
      var_row <- var_list[row_id, ]

      # if it is, we want to check that it overlaps with our target date range
      if (nrow(var_row) > 0) {

        # will automatically create multiple intervals for multiple dates
        database_interval <- var_row$start_date %--% var_row$end_date
        requested_interval <- ymd_hms(start) %--% ymd_hms(end)

        # do we have data for at least part of the requested interval?
        partial[i, j] <- any(
          int_overlaps(requested_interval, database_interval)
        )
        complete[i, j] <- requested_interval %within% database_interval

      }

    }

  }

  # return
  list(partial = partial, complete = complete)

}

#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @importFrom lubridate ymd_hms
#'
# format variable lists returned in JSON format
format_json_vars <- function(response) {

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
  out <- out[, c("site_name",
                 "site",
                 "name",
                 "units",
                 "start_date",
                 "end_date",
                 "variable",
                 "subdesc",
                 "period_start",
                 "period_end")]
  colnames(out) <- c("site_name",
                     "site_code",
                     "variable_name",
                     "units",
                     "start_date",
                     "end_date",
                     "variable_code",
                     "details",
                     "period_start",
                     "period_end")

  # and return
  out

}

#'
#' @importFrom httr GET
#'
# prepare and submit query to WMIS
query_database <- function(sites,
                           start, end,
                           varfrom, varto,
                           options,
                           data_source,
                           state,
                           var_list = NULL) {

  # get database address
  address <- get_address(state)

  # define the query
  if (is.null(var_list)) {
    query <- list(
      "site_list" = paste0(sites, collapse = ","),
      "start_time" = start,
      "end_time" = end,
      "varfrom" = varfrom,
      "varto" = varto,
      "interval" = options$interval,
      "data_type" = options$data_type,
      "multiplier" = options$multiplier,
      "datasource" = data_source,
      "function" = "get_ts_traces",
      "version" = "3",
      "ver" = "2",
      "return_type" = "hash"
    )
  } else {
    query <- list(
      "site_list" = paste0(sites, collapse = ","),
      "start_time" = start,
      "end_time" = end,
      "var_list" = var_list,
      "interval" = options$interval,
      "data_type" = options$data_type,
      "multiplier" = options$multiplier,
      "datasource" = data_source,
      "function" = "get_ts_traces",
      "version" = "3",
      "ver" = "2",
      "return_type" = "hash"
    )
  }

  # send it off
  response <- GET(address, query = query)

}

# get database address from state
get_address <- function(state) {
  switch(
    state,
    "nsw" = "https://realtimedata.waternsw.com.au/cgi/webservice.exe",
    "qld" = "https://water-monitoring.information.qld.gov.au/cgi/webservice.pl",
    "http://data.water.vic.gov.au/cgi/webservice.pl"
  )
}


#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @importFrom lubridate ymd_hms
#'
# format flow data returned in JSON format
format_json_flow <- function(response) {

  # turn the output into something readable
  output <- fromJSON(content(response, as = "text"))$return

  # now break it down into components
  raw_data <- output$traces$trace

  # this only works if data exist, otherwise raw_data is NULL
  if (!is.null(raw_data)) {

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

    # and add info on sites and variables
    out$site_name <- rep(site_info$name, times = n_obs)
    out$site_code <- rep(output$traces$site, times = n_obs)
    out$variable_name <- rep(varto_info$name, times = n_obs)
    out$variable_code <- rep(varto_info$variable, times = n_obs)
    out$varfrom_code <- rep(varfrom_info$variable, times = n_obs)
    out$units <- rep(varto_info$units, times = n_obs)

    # qc_reference is tricky because it might be one or more columns of data
    qc_reference <-
      output$traces$quality_codes[rep(seq_along(raw_data), times = n_obs), ,
                                  drop = FALSE]
    colnames(qc_reference) <- paste0("quality_reference_",
                                     colnames(qc_reference))
    out <- cbind(out, qc_reference)

  } else {

    # otherwise we need to return a null object
    out <- NULL

  }

  # return
  out

}

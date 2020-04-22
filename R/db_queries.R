# Functions to prepare queries and format JSON inputs/outputs

# return a list of variables available at each site, with date ranges
list_variables <- function(sites, settings = list()) {
  
  # find all available datasources by site
  datasource_list <- list_datasources(sites)$datasources
  
  # run through and pull out variables for each
  output <- NULL
  for (i in seq_along(datasource_list)) {
    
    # list datasources for that site
    datasource_site <- datasource_list[[i]]
    
    # overwrite settings if required
    sett_list <- list(
      data_source = datasource_site
    )
    sett_list[names(settings)] <- settings
    
    for (j in seq_along(sett_list$data_source)) {
      
      # create a query string that `GET` can interpret
      query <- paste0(
        '{"params":{"site_list":"',
        paste0(sites[i], collapse = ","),
        '","datasource":"',
        sett_list$data_source[j],
        '"},"function":"get_variable_list","version":"1"}&ver=2'
      )
      
      # send query
      response <- GET("http://data.water.vic.gov.au/cgi/webservice.pl",
                      query = query)
      
      # convert output into a readable table
      tmp <- format_JSON_vars(response)
      
      # add a column with datasource identifier
      tmp$datasource <- rep(sett_list$data_source[j], nrow(tmp))
      
      # combine into one big output matrix
      output <- rbind(output, tmp)
      
    }
    
  }
  
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
check_available <- function(sites, start, end, variables, settings = list()) {
  
  # initialise some basic settings
  sett_list <- list(
    data_source = "A",
    return_type = "hash"
  )
  sett_list[names(settings)] <- settings
  
  # which variables are available at that site?
  var_list <- list_variables(sites, sett_list)
  
  # work out how many checks we need to do
  nsite <- length(sites)
  nvar <- length(variables)
  ndatasource <- length(settings$data_source)
  
  # default to FALSE
  out <- array(FALSE, dim = c(nsite, nvar, ndatasource))
  
  # and then update one at a time
  for (i in seq_len(nsite)) {
    
    for (j in seq_len(nvar)) {
      
      for (k in seq_len(ndatasource)) {
        
        # is the variable in the database at that site?
        row_id <- var_list$site_code == sites[i] &
          var_list$variable_code == variables[j] &
          var_list$datasource == settings$data_source[k]
        var_row <- var_list[row_id, ]
        
        # if it is, we want to check that it overlaps with our target date range
        if (nrow(var_row) > 0) {
          
          # will automatically create multiple intervals for multiple dates
          database_interval <-interval(var_row$start_date, var_row$end_date)
          requested_interval <- interval(ymd_hms(start), ymd_hms(end))
          
          # do we have data for at least part of the requested interval?
          out[i, j, k] <- any(int_overlaps(requested_interval, database_interval))
          
        }
        
      }
      
    }
    
  }
  
  # return
  out
  
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

# pull out the query/response handling into a separate function
query_database <- function(address, sites,
                           start, end,
                           varfrom, varto,
                           aggr_list,
                           data_source,
                           var_list = NULL,
                           return_type = "hash") {
  
  # define the query
  if (is.null(var_list)) {
    query <- list(
      "site_list" = paste0(sites, collapse = ","),
      "start_time" = start,
      "end_time" = end,
      "varfrom" = varfrom,
      "varto" = varto,
      "interval" = aggr_list$interval,
      "data_type" = aggr_list$data_type,
      "multiplier" = aggr_list$multiplier,
      "datasource" = data_source,
      "function" = "get_ts_traces",
      "version" = "3",
      "ver" = "2",
      "return_type" = return_type
    )
  } else {
    query <- list(
      "site_list" = paste0(sites, collapse = ","),
      "start_time" = start,
      "end_time" = end,
      "var_list" = var_list,
      "interval" = aggr_list$interval,
      "data_type" = aggr_list$data_type,
      "multiplier" = aggr_list$multiplier,
      "datasource" = data_source,
      "function" = "get_ts_traces",
      "version" = "3",
      "ver" = "2",
      "return_type" = return_type
    )
  }
  
  # send it off
  response <- GET(address, query = query)
  
}

# format flow data returned in JSON format
format_JSON_flow <- function(response) {
  
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
    qc_reference <- output$traces$quality_codes[rep(seq_along(raw_data), times = n_obs), , drop = FALSE]
    colnames(qc_reference) <- paste0("quality_reference_", colnames(qc_reference))
    out <- cbind(out, qc_reference)
    
  } else {
    
    # otherwise we need to return a null object
    out <- NULL
    
  }
  
  # return
  out
  
}

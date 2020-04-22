# add docs
# setup namespace through roxygen
# setup examples
# setup basic tests
# add rigorous tests, download some known values to test against
# add separate script to pull out flow metrics (work out how to handle dates)
# maybe add option to pull flow metrics directly (by calling fetch_flow internally)?

## variable notes:
# -add check that discharge is not equal to depth

# automatically check all data sources? Somehow resolve conflicts with queried data?
#   - this is clunkiest part right now -- just goes with first match, which might not have
#       greatest coverage. Will be fine if user can use list_variables and specify varfrom/varto directly.
#   - biggest issue is when a short true streamflow time series exists, which will overwrite a longer
#       time series of streamflow converted from depth.
# could warn for partial matches (in check_available or elsewhere)?
#   - separates out data sources, so this would only be an issue if a single data source has a short
#       true streamflow time series as well as a long depth time series.
#   - alternative to warning: check length of overlap and take longest?
#   - really need a function to resolve conflicts in discharge/depth data.
#   - best approach is to take both, not longest. So grab all discharge data and all converted depth data,
#       filter out overlap (prioritising true discharge data) -- this would reduce number of queries too.

# shift padding for missing data from fetch_flow function to format_JSON_flow?
#  Have added check_missing function but it needs testing. Check for missing data seems wrong (length(list) == 0).
#    -- might break on edge cases (e.g. variable there for one datasource but not another)

# notes to keep
## data_type is operation on data (observations are instantaneous)
## multiplier gives an interval multiplier, e.g., 2 means interval = 2 * interval
## need informative errors, especially when data are partially available or when qc codes are ambiguous

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

  # standardise names as much as possible
  variables <- gsub(" |_", "", variables)
  variables <- tolower(variables)
  variables[grep("flow", variables)] <- "discharge"
  variables[grep("depth", variables)] <- "depth"
  variables[grep("temp", variables)] <- "temperature"
  variables[grep("cond", variables)] <- "conductivity"
  variables[grep("ox|do", variables)] <- "dissolvedoxygen"
  variables[grep("turb", variables)] <- "turbidity"

  # create a vector with codes relevant to each variable
  var_list <- rep(NA, length(variables))
  for (i in seq_along(variables)) {

    # pull out a tmp var to play with
    var_tmp <- variables[i]

    # switch out for database variable codes
    var_list[i] <- switch(
      var_tmp,
      "depth" = "100.00",
      "discharge" = "141.00",
      "dissolvedoxygen" = "215.00",
      "temperature" = "450.00",
      "turbidity" = "810.00",
      "conductivity" = "820.00",
      var_tmp)
  }

  # create vectors of var_from and var_to to be cycled over if making multiple_queries
  nsites <- length(sites)
  nvar <- length(var_list)
  ndatasource <- length(settings$data_source)
  varfrom <- array(rep(var_list, nsites * ndatasource), dim = c(nvar, nsites, ndatasource))
  varfrom <- aperm(varfrom, perm = c(2, 1, 3))
  varto <- varfrom

  # check whether variables exist in data
  available <- check_available(sites, start, end, var_list, settings)

  # it's all good if everything exists, can flatten to one query for all sites and variables
  multiple_sites <- FALSE
  multiple_vars <- FALSE


  ### ADD STEP HERE: resolve conflicts in streamflow data
  # 1. if 141.00 is partially available, do we need multiple requests for 100.00-141.00 and 141.00-141.00?
  # 2. check overlap of both
  # 3. if required, add column to varfrom/varto; skip following checks? (should skip anyway because 141.00 will be available)

  ###

  # otherwise we need to be more careful, first checking flow is available and then
  #   working out whether other vars are available
  if (!all(available)) {

    # we only care about flow in this step; skip otherwise
    if (any(varfrom[!available] == "141.00")) {

      # work out where the flow request sits in the list of variables
      col_id <- varfrom[1, , 1] == "141.00"

      # subset varfrom to the cols/rows/slices that we care about (missing data, flow request)
      idx <- varfrom == "141.00" & !available

      # set varfrom to depth data in these cases (won't hurt, might help)
      varfrom[idx] <- "100.00"

      # check if depth data are available at that combo
      available_update <- check_available(sites, start, end, "100.00", settings)

      # update availability just for this column and unavailable variables
      #   (check_available returns all rows and slices for a given column, including
      #    previously available rows/slices)
      available <- available_update[!available[, col_id, ]]

    }

    # default in this expanded case is to expand over sites
    multiple_sites <- TRUE

    # but can flatten sites if all rows of varfrom/varto are identical
    varfrom_equal <- all(apply(varfrom, c(2, 3), function(x) length(unique(x)) == 1))
    varto_equal <- all(apply(varto, c(2, 3), function(x) length(unique(x)) == 1))
    if (varfrom_equal & varto_equal) {
      varfrom <- varfrom[1, , , drop = FALSE]
      varto <- varto[1, , , drop = FALSE]
      multiple_sites <- FALSE
    }

    # can use var_list if varfrom and varto are equal
    if (!all(varfrom == varto))
      multiple_vars <- TRUE

  }

  # if anything is missing, print it out to the screen
  missing <- c(!available)
  missing_sites <- rep(sites, times = nvar * ndatasource)[missing]
  missing_vars <- rep(rep(variables, each = nsites), times = ndatasource)[missing]
  missing_datasources <- rep(rep(settings$data_source, each = nsites), each = nvar)[missing]
  missing_details <- paste(missing_sites, missing_vars, sep = ": ")
  missing_datasource_formatted <- paste0("(data source: ", missing_datasources, ")")
  missing_details <- paste(missing_details, missing_datasource_formatted, sep = " ")
  if (any(missing)) {
    warning("No data for the following sites, variables and data sources: ",
            paste(missing_details, collapse = ", "),
            ". Use `list_variables(c(",
            paste(sites, collapse = ", "),
            "))` to work out when and where data are available.",
            call. = FALSE)
  }

  # collapse var_list into a single string to pass to the JSON call
  var_list <- paste0(var_list, collapse = ",")

  # turn off var_list if we need to run multiple queries
  if (multiple_vars)
    var_list <- NULL

  # return
  list(varfrom = varfrom, varto = varto, var_list = var_list,
       multiple_sites = multiple_sites, multiple_vars = multiple_vars)

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

# standardise column names of different requests before rbinding them
standardise_columns <- function(out) {

  # what are the unique qc reference codes that we need to inclue?
  unique_qc_refs <- unique(
    unlist(
      sapply(out, function(x) colnames(x)[grep("quality_reference", colnames(x))])
    )
  )

  # loop over each element one-at-a-time and fill missing qc references with a column of NAs
  for (i in seq_along(out)) {
    tmp <- out[[i]]
    tmp_cols <- colnames(tmp)
    idx <- grep("quality_reference", tmp_cols)
    qc_ref_null <- matrix(NA, nrow = nrow(tmp), ncol = length(unique_qc_refs))
    colnames(qc_ref_null) <- unique_qc_refs
    qc_ref_null <- as.data.frame(qc_ref_null)
    qc_ref_null[, tmp_cols[idx]] <- tmp[, idx]
    tmp <- tmp[, -idx]
    tmp <- cbind(tmp, qc_ref_null)
    out[[i]] <- tmp
  }

  # return
  out

}

check_missing <- function(output_list, id, dates, varfrom, varto, site) {

  ## CHECK: should possibly be if (length(out) < ndatasource)?
  if (length(output_list) == 0) {
    out <- data.frame(value = 0.0,
                              date = dates$start,
                              quality_code = 255,
                              date_formatted = ymd_hms(dates$start),
                              site_name = NA,
                              site_code = site,
                              variable_name = NA,
                              variable_code = varto,
                              varfrom_code = varfrom,
                              units = NA,
                              quality_reference_255 = "Missing data")
  } else {
    out <- output_list[[id]]
  }

  out

}

# expand data set to include rows for unavailable dates
expand_missing <- function(data, start, end) {

  # reformat dates with useful format
  start_formatted <- ymd_hms(start)
  end_formatted <- ymd_hms(end)

  # check minimum interval
  time_unit <- min(
    unlist(
      tapply(
        data$date_formatted,
        list(data$variable_code, data$site_code, data$datasource),
        function(x) int_length(int_diff(x))
      )
    )
  )

  # and create a sequence of requested dates
  requested_dates <- seq(start_formatted, end_formatted, by = time_unit)

  # are we dealing with multiple sites?
  site_list <- unique(data$site_code)

  # and multiple variables?
  var_list <- unique(data$variable_code)

  # loop over all
  for (i in seq_along(site_list)) {

    for (j in seq_along(var_list)) {

      # subset to single variable
      tmp <- data[data$site_code == site_list[i] & data$variable_code == var_list[j], ]

      # hack approach for now: just add missing for extensions beyond available date range
      available_dates <- tmp$date_formatted

      # do all data exist for this variable?
      data_complete <- requested_dates %in% available_dates

      # if not, add some rows
      if (!all(data_complete)) {

        # create a data.frame from dates that don't have a match in the data
        data_fill <- tmp[rep(1, times = sum(!data_complete)), ]
        data_fill$value <- NA
        data_fill$quality_code <- 255
        data_fill$date_formatted <- requested_dates[!data_complete]
        data_fill$quality_reference_255 <- "Missing data"
        data_fill$date <- as.character(data_fill$date_formatted)
        data_fill$date <- gsub("-|:| ", "", data_fill$date)
        data_fill$date <- paste0(data_fill$date, paste0(rep(0, 6), collapse = ""))

        # fill other columns if no data exist for this variable at all
        if (all(is.na(available_dates))) {
          data_fill$site_name <- NA
          data_fill$site_code <- site_list[i]
          data_fill$variable_name <- NA
          data_fill$variable_code <- var_list[j]
          data_fill$units <- NA
        }

        # we've added a column to data_fill with qc_ref255 info, add this to data if not already there
        if (is.null(data$quality_reference_255))
          data$quality_reference_255 <- NA

        # bind this to the end of data
        data <- rbind(data, data_fill)

      }

    }

  }

  # finalise by sorting data by date within variable
  data <- data[order(data$variable_code, data$site_code, data$datasource, data$date_formatted), ]

  # return
  data

}

# grab vicwater data on one or more variables for a site or set of sites
fetch_data <- function(sites, start, end, variables,
                       include_missing = FALSE,
                       aggregation = list(),
                       settings = list(),
                       options = list()) {

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

  # set list of variable options
  parsed_vars <- list(
    varfrom = NULL,
    varto = NULL,
    var_list = NULL,
    multiple_sites = FALSE,
    multiple_vars = FALSE
  )
  parsed_vars[names(options)] <- options

  # reformat dates
  parsed_dates <- parse_dates(start, end)

  # get variables if varfrom/varto/var_list not provided
  if (!is.null(parsed_vars$varfrom) & !is.null(parsed_vars$varto) | !is.null(parsed_vars$var_list)) {
    if (is.null(var_list))
      parsed_vars$multiple_vars <- TRUE
  } else {
    parsed_vars <- parse_variables(variables, sites, parsed_dates$start, parsed_dates$end, sett_list)
  }

  # if we can just run all variables and sites at once
  if (!parsed_vars$multiple_sites & !parsed_vars$multiple_vars) {

    # run through all requested data sources
    output <- vector("list", length = length(sett_list$data_source))
    for (i in seq_along(sett_list$data_source)) {

      # send off a query
      response <- query_database(
        address = "http://data.water.vic.gov.au/cgi/webservice.pl",
        sites = paste0(sites, collapse = ","),
        start = parsed_dates$start,
        end = parsed_dates$end,
        var_list = parsed_vars$var_list,
        aggr_list = aggr_list,
        data_source = sett_list$data_source[j],
      )

      # extract and reformat the output
      output[[i]] <- format_JSON_flow(response)

      # add datasource identifier to data set
      output[[i]]$datasource <- sett_list$data_source[i]

    }

    # collapse list output into a matrix
    output <- do.call(rbind, output)

  } else {

    if (!parsed_vars$multiple_sites) {

      # initialise an empty list object to store outputs
      output <- vector("list", length = length(parsed_vars$varfrom))

      # we need to do it separately for each variable
      for (i in seq_along(parsed_vars$varfrom)) {

        # run through all requested data sources
        tmp <- vector("list", length = length(sett_list$data_source))
        for (j in seq_along(sett_list$data_source)) {

          # send off a query
          response <- query_database(
            address = "http://data.water.vic.gov.au/cgi/webservice.pl",
            sites = paste0(sites, collapse = ","),
            start = parsed_dates$start,
            end = parsed_dates$end,
            varfrom = parsed_vars$varfrom[1, i, j],
            varto = parsed_vars$varto[1, i, j],
            aggr_list = aggr_list,
            data_source = sett_list$data_source[j],
          )

          # extract and reformat the output
          tmp[[j]] <- format_JSON_flow(response)

          # fill with minimal info if variable is missing
          tmp[[j]] <- check_missing(tmp, j,
                                    parsed_dates,
                                    parsed_vars$varfrom[1, i, j],
                                    parsed_vars$varto[1, i, j],
                                    sites)

        }

        # add datasource identifier to data set
        tmp[[j]]$datasource <- sett_list$data_source[j]

        # make sure all tmp lists have same column headings for quality reference
        tmp <- standardise_columns(tmp)

        # can now flatten into a single matrix
        output[[i]] <- do.call(rbind, tmp)

      }

      # make sure all list elements have the same columns for quality_reference
      output <- standardise_columns(output)

      # flatten this into a single data.frame
      output <- do.call(rbind, output)

    } else {

      # initialise an empty list object to store outputs
      output <- vector("list", length = nrow(parsed_vars$varfrom))

      # we need to do it separately for each variable
      for (i in seq_len(nrow(parsed_vars$varfrom))) {

        # run through all requested data sources
        tmp <- vector("list", length = ncol(parsed_vars$varfrom))

        for (j in seq_len(ncol(parsed_vars$varfrom))) {

          tmp[[j]] <- vector("list", length = length(sett_list$data_source))
          for (k in seq_along(sett_list$data_source)) {

            # send off a query
            response <- query_database(
              address = "http://data.water.vic.gov.au/cgi/webservice.pl",
              sites = sites[i],
              start = parsed_dates$start,
              end = parsed_dates$end,
              varfrom = parsed_vars$varfrom[i, j, k],
              varto = parsed_vars$varto[i, j, k],
              aggr_list = aggr_list,
              data_source = sett_list$data_source[k],
            )

            # extract and reformat the output
            tmp[[j]][[k]] <- format_JSON_flow(response)

            # fill with minimal info if variable is missing
            tmp[[j]][[k]] <- check_missing(tmp[[j]], k,
                                           parsed_dates,
                                           parsed_vars$varfrom[i, j, k],
                                           parsed_vars$varto[i, j, k],
                                           sites[i])

          }

          # add datasource identifier to data set
          tmp[[j]][[k]]$datasource <- sett_list$data_source[j]

          # make sure all tmp lists have same column headings for quality reference
          tmp[[j]] <- standardise_columns(tmp[[j]])

          # combine all datasources for variable j
          tmp[[j]] <- do.call(rbind, tmp[[j]])

        }

        # make sure all list elements have the same columns for quality_reference
        tmp <- standardise_columns(tmp)

        # can now flatten into a single matrix for site i
        output[[i]] <- do.call(rbind, tmp)

      }

      # make sure all list elements have the same columns for quality_reference
      output <- standardise_columns(output)

      # flatten this into a single data.frame
      output <- do.call(rbind, output)

    }

  }

  # expand around missing observations
  if (include_missing)
    output <- expand_missing(output, parsed_dates$start, parsed_dates$end)

  # check if vicwater has included missing data with a value (usually 0.0)
  missing_with_value <- output$quality_code == 255 & !is.na(output$value)
  if (any(missing_with_value)) {

    # make output columns slightly more informative in this case
    output$value[missing_with_value] <- NA
    output$quality_reference_255[missing_with_value] <- "Missing data"

    # remove these t if !include_missing
    if (!include_missing)
      output <- output[!missing_with_value, ]

  }

  #### TO COMPLETE
  # standardise column classes (check others)
  output$value <- as.numeric(output$value)

  # and return
  output

}

# tabulate date by qc codes
check_quality <- function(data) {

  # running list of qc codes
  code_list <- list(
    "1" = "Unedited data",
    "2" = "Good quality, minimal editing required. Drift correction",
    "3" = "Used only by TELEM data",
    "10" = "Data transposed from recorder chart",
    "15" = "Minor editing. >+/-10mm drift correction",
    "50" = "Medium editing >+/-30mm drift correction, significant single spike removal etc.",
    "82" = "Linear interpolation across gap in records (< 0.5 day)",
    "100" = "Irregular data, beyond QC=50 or unexplained",
    "104" = "Records manually estimated",
    "149" = "Rating extrapolated witin 1.5x max Qm",
    "150" = "Rating extrapolated due to insufficient gaugings",
    "170" = "Raw unedited data from secondary/backup sensor",
    "255" = "Missing data"
  )
  recommendation_list <- list(
    "1" = "Use",
    "2" = "Use",
    "3" = "Use",
    "10" = "Use",
    "15" = "Use",
    "50" = "Use with caution",
    "82" = "Use with caution",
    "100" = "Use with caution",
    "104" = "Use with caution",
    "149" = "Use with caution",
    "150" = "Use with caution",
    "170" = "Do not use",
    "255" = "Missing data"
  )

  # tabulate data
  codes_breakdown <- table(data$quality_code, data$variable_code, data$site_code)

  # tidy the tabulated data, add code info
  classifiers <- dimnames(codes_breakdown)
  lengths <- sapply(classifiers, length)
  codes_breakdown <- data.frame(
    site = rep(classifiers[[3]], each = lengths[1] * lengths[2]),
    variable = rep(rep(classifiers[[2]], each = lengths[1]), times = lengths[3]),
    quality_code = rep(classifiers[[1]], times = lengths[2] * lengths[3]),
    count = c(codes_breakdown)
  )
  codes_breakdown$description <- code_list[as.character(codes_breakdown$quality_code)]
  codes_breakdown$recommendation <- recommendation_list[as.character(codes_breakdown$quality_code)]

  # add variable names
  var_names <- c(
    "100.00" = "depth",
    "141.00" = "discharge",
    "215.00" = "dissolvedoxygen",
    "450.00" = "temperature",
    "810.00" = "turbidity",
    "820.00" = "conductivity"
  )
  codes_breakdown$variable <- var_names[as.character(codes_breakdown$variable)]

  # return table with qc info
  codes_breakdown[codes_breakdown$count > 0, ]

}


#' @name fetch_hydro
#' @title Download data from Victorian WMIS
#' @description Download data from the Victorian Water Measurement
#'   Information System (WMIS) for one or more variables at one or more sites.
NULL

#' @rdname fetch_hydro
#'
#' @export
#'
#' @param sites a character or numeric vector of site codes (gauge numbers)
#' @param start start date for downloaded data. Date format is flexible but
#'    for most reliable results use yyyy-mm-dd
#' @param end end date for downloaded data. Date format is flexible but
#'    for most reliable results use yyyy-mm-dd
#' @param variables a character or numeric vector of variable names or codes.
#'    Many variations are accounted for (e.g. streamflow, flow, discharge).
#'    WMIS variable codes will work.
#' @param include_missing a \code{logical} defining whether to pad output data
#'   to include dates where data are missing
#' @param include_qc a \code{logical} defining whether to return information on
#'   quality control values. This information can be retrieved with
#'   \link{check_quality}
#' @param options a \code{list} specifying one of four advanced options:
#'    - the interval type (defaults to "daily") for calculation of downloaded
#'      values
#'    - the data_type (defaults to "mean") for aggregation of downloaded values
#'    - the multiplier (defaults to "1") used to expand or shrink the requested
#'      interval
#'    - varfrom and varto (default to NULL) to specify explicit variable
#'      conversions with WMIS variable codes. Both varfrom and varto must
#'      be provided
#' @param data_source database from which data are downloaded. Defaults to "A",
#'   which covers most cases. The \code{list_datasources} function will list
#'   all available data_sources for a given site
#' @param state state of interest; one of Victoria, New South Wales, or
#'   Queensland
#'
#' @details Streamflow and related data (e.g. temperature) are recorded as
#'   instantaneous values, so must be aggregated prior to download. The default
#'   settings specify daily means, but other options are available with the
#'   \code{options} parameter.
#'
#'   Dates and variable names are interpreted loosely. Dates will recognise any
#'   of the following formats: \code{"ymd"}, \code{"dmy"}, \code{"y"},
#'   \code{"ymd_HMS"}, and \code{"ymd_HM"}. Variables will ignore case and
#'   remove all spaces and underscores. Once tidied, any variable name
#'   containing any of the following will be recognised: \code{"flow"},
#'   \code{"discharge"}, \code{"depth"}, \code{"height"}, \code{"temp"},
#'   \code{"cond"}, \code{"ox"}, \code{"do"}, and \code{"turb"}.
#'
#' @return a \code{data.frame} containing the downloaded flow data and relevant
#'   identifiers for each observation.
#'
#' @examples
#' \dontrun{
#'
#' # grab data for three variables at three sites
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "2004-01-01",
#'   end = "2020-04-21",
#'   variables = c("flow", "temp", "depth")
#' )
#'
#' # try a different date range
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "1991-01-01",
#'   end = "1993-04-21",
#'   variables = c("flow", "temp", "depth")
#' )
#'
#' # or a different date format
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "01-01-1994",
#'   end = "1996",
#'   variables = c("flow", "temp", "depth")
#' )
#'
#' # repeat, but fill gaps with placeholders
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "2004-01-01",
#'   end = "2020-04-21",
#'   variables = c("flow", "temp", "depth"),
#'   include_missing = TRUE
#' )
#'
#' # check for data from the telemetry data source
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "2004-01-01",
#'   end = "2020-04-21",
#'   variables = c("flow", "temp", "depth"),
#'   data_source = "TELEM"
#' )
#'
#' # advanced options: return weekly median values
#' #   rather than daily mean values
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "2004-01-01",
#'   end = "2020-04-21",
#'   variables = c("flow", "temp", "depth"),
#'   options = list(
#'     interval = "weekly",
#'     data_type = "median"
#'   )
#' )
#'
#' # advanced options: specify variables explicitly
#' #   by WMIS codes
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "2004-01-01",
#'   end = "2020-04-21",
#'   variables = c("flow", "temp", "depth"),
#'   options = list(
#'     varfrom = "100.00",
#'     varto = "141.00"
#'   )
#' )
#'
#' }
#'
fetch_hydro <- function(sites, start, end, variables,
                        include_missing = FALSE,
                        include_qc = FALSE,
                        options = list(),
                        data_source = "A",
                        state = "vic") {

  # set default options
  opt <- list(
    interval = "daily",
    data_type = "mean",
    multiplier = "1",
    varfrom = NULL,
    varto = NULL
  )
  opt[names(options)] <- options

  # are these options?
  opt <- check_options(opt)

  # reformat dates
  dates <- parse_dates(start, end)

  # check state and tidy the name if needed
  state <- parse_state(state)

  # get variables if varfrom/varto/var_list not provided
  if (is.null(opt$varfrom) | is.null(opt$varto)) {
    vars <- parse_variables(variables,
                            sites,
                            dates$start,
                            dates$end,
                            data_source = data_source,
                            state = state)
  } else {
    vars <- list(varfrom = opt$varfrom, varto = opt$varto)
  }

  # check if we can use var_list to vectorise the query
  var_list <- NULL
  if (all(vars$varfrom == vars$varto))
    var_list <- paste0(vars$varto, collapse = ",")

  # if we can just run all variables and sites at once
  if (!is.null(var_list)) {

    # send off a query
    response <- query_database(
      sites = paste0(sites, collapse = ","),
      start = dates$start,
      end = dates$end,
      var_list = var_list,
      options = opt,
      data_source = data_source,
      state = state
    )

    # extract and reformat the output
    output <- format_json_flow(response)

  } else {

    output <- vector("list", length = length(vars$varfrom))

    # we need to do it separately for each variable
    for (i in seq_along(vars$varfrom)) {

      # send query
      response <- query_database(
        sites = paste0(sites, collapse = ","),
        start = dates$start,
        end = dates$end,
        varfrom = vars$varfrom[i],
        varto = vars$varto[i],
        options = opt,
        data_source = data_source,
        state = state
      )

      # format output so it's readable
      tmp <- format_json_flow(response)

      # if there's no output, add a placeholder so we know that
      #   variable is missing
      if (is.null(tmp)) {
        output[[i]] <- fill_missing(dates,
                                    vars$varfrom[i],
                                    vars$varto[i],
                                    sites)
      } else {
        output[[i]] <- tmp
      }

    }

    # make sure all list elements have the same columns for quality_reference
    output <- standardise_columns(output)

    # flatten this into a single data.frame
    output <- do.call(rbind, output)

  }

  # do we want to keep qc info?
  if (!include_qc)
    output <- output[, !grepl("quality_reference", colnames(output))]

  # expand around missing observations
  if (include_missing)
    output <- expand_missing(output, dates$start, dates$end, include_qc)

  # check if vicwater has included missing data with a value (usually 0.0)
  missing_with_value <- output$quality_code >= 250 & !is.na(output$value)
  if (any(missing_with_value)) {

    # make output columns slightly more informative
    output$value[missing_with_value] <- NA

    # add qc info if required
    if (include_qc)
      output$quality_reference_255[missing_with_value] <- "Missing data"

    # remove these if !include_missing
    if (!include_missing)
      output <- output[!missing_with_value, ]

  }

  # standardise column classes
  output$value <- as.numeric(output$value)

  # and clean up variable names
  output$variable_name <- gsub("\\(|\\)|/", "", output$variable_name)
  output$variable_name <- gsub(" ", "_", output$variable_name)
  output$variable_name <- gsub("\u00B0C", "c", output$variable_name)
  output$variable_name <- tolower(output$variable_name)


  # and return
  output

}

#'
#' @importFrom lubridate parse_date_time
#'
# turn dates from a range of formats into a consistent format for JSON calls
parse_dates <- function(start, end) {

  # try and convert whatever we've been given to some sort of date
  start <- parse_date_time(
    start,
    orders = c("ymd", "dmy", "y", "ymd_HMS", "ymd_HM")
  )
  end <- parse_date_time(
    end,
    orders = c("ymd", "dmy", "y", "ymd_HMS", "ymd_HM")
  )

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

# standardise state names
parse_state <- function(state) {

  # standardise as much as possible
  state <- gsub(" |_", "", state)
  state <- tolower(state)
  state[grep("vic", state)] <- "vic"
  state[grep("nsw|new|south|wales", state)] <- "nsw"
  state[grep("qld|qu", state)] <- "qld"

  # check whether state is OK
  if (!state %in% c("vic", "nsw", "qld")) {
    stop("state should partially match one of Victoria, ",
         "New South Wales, or Queensland",
         call. = FALSE)
  }

  # return
  state

}

# convert variable names to codes used in vicwater database
parse_variables <- function(variables, sites, start, end, data_source, state) {

  # standardise names as much as possible
  variables <- gsub(" |_", "", variables)
  variables <- tolower(variables)
  variables[grep("flow", variables)] <- "discharge"
  variables[grep("depth|height", variables)] <- "depth"
  variables[grep("storage|dam|level", variables)] <- "storage_water_level"
  variables[grep("temp", variables)] <- "temperature"
  variables[grep("cond", variables)] <- "conductivity"
  variables[grep("ox|do", variables)] <- "dissolvedoxygen"
  variables[grep("turb", variables)] <- "turbidity"

  # we only need to download each variable once
  variables <- unique(variables)

  # create a vector with codes relevant to each variable
  varfrom <- rep(NA, length(variables))
  for (i in seq_along(variables)) {

    # pull out a tmp var to play with
    var_tmp <- variables[i]

    # switch out for database variable codes
    varfrom[i] <- switch(
      var_tmp,
      "depth" = "100.00",
      "storage_water_level" = "130.00",
      "discharge" = "141.00",
      "dissolvedoxygen" = "215.00",
      "temperature" = "450.00",
      "turbidity" = "810.00",
      "conductivity" = "820.00",
      var_tmp)
  }

  # create vectors of varfrom and varto to be cycled over if making
  #   multiple queries
  varto <- varfrom
  expanded_flow <- FALSE
  if ("141.00" %in% varfrom) {
    varfrom <- c(varfrom, "100.00", "141.50")
    varto <- c(varto, "141.00", "141.50")
    variables <- c(variables, rep("discharge", 2))
    expanded_flow <- TRUE
  }

  # check whether variables exist in data
  available <- check_available(sites, start, end, varfrom, data_source, state)
  partial <- available$partial
  complete <- available$complete

  # remove excess flow variables if not available
  missing <- apply(partial, 2, function(x) all(!x))
  if (expanded_flow & any(missing)) {
    is_flow <- varto %in% c("141.00", "141.50")
    if (all(missing[is_flow]))
      is_flow[1] <- FALSE
    to_keep <- !(missing & is_flow)
    varfrom <- varfrom[to_keep]
    varto <- varto[to_keep]
    variables <- variables[to_keep]
    partial <- partial[, to_keep, drop = FALSE]
    complete <- complete[, to_keep, drop = FALSE]
  }

  # if anything is missing, print out a message
  check <- partial + complete
  if (any(!complete) | any(!partial))
    send_message(check, sites, variables, state)

  # return
  list(varfrom = varfrom, varto = varto)

}

# create an informative warning about missing or partial data
send_message <- function(check, sites, vars, state) {

  # create a matrix of sites/vars to match dims of check
  sites_expanded <- matrix(rep(sites, times = length(vars)),
                           ncol = length(vars))
  vars_expanded <- matrix(rep(vars, each = length(sites)),
                          ncol = length(vars))

  # check is 0 if both missing (!complete & !partial),
  #   1 if partial (!complete & partial) and 2 if complete
  #   data are available (complete & partial)
  missing_sites <- sites_expanded[check == 0]
  missing_vars <- vars_expanded[check == 0]
  partial_sites <- sites_expanded[check == 1]
  partial_vars <- vars_expanded[check == 1]

  # make up a vector of missing site/var combos
  missing <- paste(missing_sites, missing_vars, sep = ":")
  partial <- paste(partial_sites, partial_vars, sep = ":")

  # just want unique combos, no need to print the same thing twice
  missing <- unique(missing)
  partial <- unique(partial)

  # partials will not be complete by definition, just return the
  #   true partial variables
  partial <- partial[!partial %in% missing]

  # format a neat warning
  missing_msg <- special_paste(missing)
  partial_msg <- special_paste(partial)

  if (length(missing > 0) & length(partial > 0)) {

    # print full message if anything is missing
    message(
      "No data for the following sites and variables:\n",
      missing_msg,
      "\nIncomplete data for the following sites and variables:\n",
      partial_msg,
      "\nUse list_variables(c(",
      paste0(unique(c(missing_sites, partial_sites)),
             collapse = ", "),
      "), state = ",
      state,
      ") to check data availability."
    )

  } else {

    # or reduced message if there is nothing partially missing
    if (length(missing > 0)) {
      message(
        "No data for the following sites and variables:\n",
        missing_msg,
        "\nUse list_variables(c(",
        paste0(unique(c(missing_sites, partial_sites)),
               collapse = ", "),
        "), state = ",
        state,
        ") to check data availability."
      )
    }

    # or reduced message if there are nothing is fully missing
    if (length(partial > 0)) {
      message(
        "Incomplete data for the following sites and variables:\n",
        partial_msg,
        "\nUse list_variables(c(",
        paste0(unique(c(missing_sites, partial_sites)),
               collapse = ", "),
        "), state = ",
        state,
        ") to check data availability."
      )
    }

  }

}

# paste together multiple objects with commas and ands in the right place
special_paste <- function(obj) {

  split_obj <- strsplit(obj, ":")
  site_list <- sapply(split_obj, function(x) x[1])
  var_list <- sapply(split_obj, function(x) x[2])

  unique_sites <- unique(site_list)
  var_by_site <- match(site_list, unique_sites)

  out <- ""
  for (i in seq_along(unique_sites)) {

    var_tmp <- var_list[var_by_site == i]
    ntmp <- length(var_tmp)

    if (ntmp == 1)
      var_tmp <- var_tmp

    if (ntmp == 2)
      var_tmp <- paste0(var_tmp, collapse = " and ")

    if (ntmp > 2) {
      var_tmp <- paste0(
        paste0(var_tmp[seq_len(ntmp - 1)], collapse = ", "),
        " and ", var_tmp[ntmp]
      )
    }

    out <- paste0(out, paste(unique_sites[i], var_tmp, sep = ": "), "\n")

  }

  out

}

# check that the options are OK
check_options <- function(x) {

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
  if (!interval_ok) {
    stop("interval must be one of day, month,",
         " year, hour, minute, second, or period",
         call. = FALSE)
  }
  if (!data_type_ok) {
    stop("data_type must be one of mean, maxmin, max, min, start,",
         " end, first, last, tot, point, partialtot, cum",
         call. = FALSE)
  }

  # return corrected names if it didn't error first
  x

}

# standardise column names of different requests before rbinding them
standardise_columns <- function(out) {

  # what are the unique qc reference codes that we need to inclue?
  unique_qc_refs <- unique(
    unlist(
      sapply(out,
             function(x) colnames(x)[grep("quality_reference",
                                          colnames(x))])
    )
  )

  # loop over each element one-at-a-time and fill missing qc
  #   references with a column of NAs
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

#'
#' @importFrom lubridate ymd_hms
#'
# create a placeholder to catch missing variables
fill_missing <- function(dates, varfrom, varto, sites) {

  data.frame(
    value = 0.0,
    date = dates$start,
    quality_code = 255,
    date_formatted = ymd_hms(dates$start),
    site_name = NA,
    site_code = sites,
    variable_name = NA,
    variable_code = varto,
    varfrom_code = varfrom,
    units = NA,
    quality_reference_255 = "Missing data"
  )

}

#'
#' @importFrom lubridate ymd_hms int_length int_diff
#'
# expand data set to include rows for unavailable dates
expand_missing <- function(data, start, end, include_qc) {

  # reformat dates with useful format
  start_formatted <- ymd_hms(start)
  end_formatted <- ymd_hms(end)

  # check minimum interval
  time_unit <- min(
    unlist(
      tapply(
        data$date_formatted,
        list(data$variable_code, data$site_code),
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
      tmp <- data[data$site_code == site_list[i] &
                    data$variable_code == var_list[j], ]

      # hack approach for now: just add missing for extensions
      #   beyond available date range
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
        data_fill$date <- paste0(data_fill$date,
                                 paste0(rep(0, 6), collapse = ""))

        # fill other columns if no data exist for this variable at all
        if (all(is.na(available_dates))) {
          data_fill$site_name <- NA
          data_fill$site_code <- site_list[i]
          data_fill$variable_name <- NA
          data_fill$variable_code <- var_list[j]
          data_fill$units <- NA
        }

        # we've added a column to data_fill with qc_ref255 info,
        #   add this to data if not already there
        if (is.null(data$quality_reference_255))
          data$quality_reference_255 <- NA

        # bind this to the end of data
        data <- rbind(data, data_fill)

      }

    }

  }

  # remove qc info if not required
  if (!include_qc)
    data <- data[, !grepl("quality_reference_255", colnames(data))]

  # finalise by sorting data by date within variable
  data <- data[order(data$variable_code, data$site_code, data$date_formatted), ]

  # return
  data

}

#' @rdname fetch_hydro
#'
#' @export
#'
#' @param data a \code{data.frame} returned by \code{fetch_hydro}
#'
#' @details tabulates data by QC code, with descriptions of each QC code.
#'
#' @examples
#' \dontrun{
#'
#' # download some flow data
#' flow_data <- fetch_hydro(
#'   sites = c("405232", "406276", "406278"),
#'   start = "1991-01-01",
#'   end = "1993-04-21",
#'   variables = c("flow", "temp", "depth")
#' )
#'
#' # and print a table of QC codes to the console
#' check_quality(flow_data)
#'
#' }
# tabulate date by qc codes
check_quality <- function(data) {

  # running list of qc codes
  code_list <- list(

    # VIC
    "1" = "Unedited data",
    "2" = "Good quality, minimal editing required. Drift correction",
    "3" = "Used only by TELEM data",
    "10" = "Data transposed from recorder chart",
    "11" = "Raw data used for operational purposes. Not validated",
    "15" = "Minor editing. >+/-10mm drift correction",
    "50" = paste0("Medium editing >+/-30mm drift correction, ",
                  "significant single spike removal etc."),
    "76" = paste0("Reliable non-linear interpolation using ",
                  "other data sources, not a correlation"),
    "77" = "Correlation with other station. Same variable only.",
    "82" = "Linear interpolation across gap in records (< 0.5 day)",
    "100" = "Irregular data, beyond QC=50 or unexplained",
    "104" = "Records manually estimated",
    "149" = "Rating extrapolated witin 1.5x max Qm",
    "150" = "Rating extrapolated due to insufficient gaugings",
    "151" = "Data lost due to natural causes / vandalism",
    "153" = "Data not recorded. Probe out of water/below instrument threshold",
    "161" = "Poor quality data from debris affecting sensor",
    "170" = "Raw unedited data from secondary/backup sensor",
    "180" = "Data not recorded, equipment malfunction",
    "250" = "Rating table suspended",
    "255" = "Missing data",

    # QLD
    "20" = "Fair",
    "30" = "Poor",

    # NSW
    "34" = "Pre 21 Aug 2004 - RATINGS - Good record where >= 5 gaugings, 95% within 10% curve",
    "41" = "Pre 21 Aug 2004 - RATINGS/TS - Fair measured data"
  )

  # and what should we do with each code?
  recommendation_list <- list(
    "1" = "Use",
    "2" = "Use",
    "3" = "Use",
    "10" = "Use",
    "11" = "Use with caution",
    "15" = "Use",
    "20" = "Use with caution",
    "30" = "Use with caution",
    "34" = "Use with caution",
    "41" = "Use",
    "50" = "Use with caution",
    "76" = "Use with caution",
    "77" = "Use with caution",
    "82" = "Use with caution",
    "100" = "Use with caution",
    "104" = "Use with caution",
    "149" = "Use with caution",
    "150" = "Use with caution",
    "151" = "Do not use",
    "153" = "Do not use",
    "161" = "Do not use",
    "170" = "Do not use",
    "180" = "Do not use",
    "250" = "Missing data",
    "255" = "Missing data"
  )

  # tabulate data
  codes_breakdown <- table(data$quality_code,
                           data$variable_code,
                           data$site_code)

  # tidy the tabulated data, add code info
  classifiers <- dimnames(codes_breakdown)
  lengths <- sapply(classifiers, length)
  codes_breakdown <- data.frame(
    site = rep(classifiers[[3]], each = lengths[1] * lengths[2]),
    variable = rep(rep(classifiers[[2]], each = lengths[1]),
                   times = lengths[3]),
    quality_code = rep(classifiers[[1]], times = lengths[2] * lengths[3]),
    count = c(codes_breakdown)
  )
  codes_breakdown$description <-
    code_list[as.character(codes_breakdown$quality_code)]
  codes_breakdown$recommendation <-
    recommendation_list[as.character(codes_breakdown$quality_code)]

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

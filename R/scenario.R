#' @name scenario
#' @title Generate hydrological scenarios from data
#' @description Functions to update hydrological data to reflect
#'   management scenarios (esp. environmental watering) and resample
#'   from existing data to create hypothetical climate change scenarios
NULL

#' @rdname scenario
#'
#' @export
#'
#' @param x discharge data from gauge or a date vector in the case of
#'   \code{water_year}
#' @param date formatted date corresponding to gauge flows
#' @param thresholds list of flow thresholds to be combined with
#'   timing and context to modify gauge flows. See Details for
#'   usage.
#' @param timing a list specifying the timing of each threshold;
#'   currently restricted to (integer) calendar months (1 = Jan,
#'   12 = Dec)
#' @param duration a list specifying the duration for which
#'   each threshold is applied
#' @param change a list specifying the minimum or maximum daily change
#'   allowed in a hydrological sequence
#' @param n a list specifying the number of times a given event is
#'   repeated
#' @param context a character vector denoting the context for each
#'   time period or a function that can be applied to discharge data
#'   (default) or context data (if provided) to determine the context
#'   for each time period
#' @param context_data additional data used to define context
#'   when this depends on factors other than discharge in the
#'   defined time period (e.g. rainfall, temperature, or
#'   antecedent discharge). Note that context data will be
#'   compiled according to the specified resolution and context
#'   should be provided as a character vector if this is not
#'   sufficient
#' @param resolution a function to classify dates into discrete
#'   units (e.g. calendar years, water years, months) over which
#'   the context, timing, and thresholds can be applied. See
#'   \code{water_year} for an example
#' @param \dots additional arguments passed to context
#'
#' @details \code{generate_scenario} is a function to define
#'   flow scenarios from gauge data based on a series of rules
#'   establishing flow thresholds, timings, and durations.
#'
#'   Each element of thresholds can be a named list or vector
#'   with three values. Vectors denote lower, upper, and maximum thresholds
#'   to modify discharge values, e.g., a value of c(1, 10, 100) will set
#'   all values below 1 to a random value between 1 and 10 and all values
#'   above 100 to 100. If only one of these bounds is required
#'   vectors can include -Inf and Inf or NA values, which are ignored.
#'   Named lists are used when context is necessary. In this case, the
#'   context argument assigns each time period to a category that
#'   determines which threshold is applied.
#'
#'   Thresholds are applied first-to-last, so precedence is given to
#'   those later in the list.
#'
#'   Timing must be provided for each threshold, and denotes the
#'   months in which each threshold is applied. Months are defined
#'   based on calendar months (1 to 12) and must be a full vector
#'   including all relevant months, not just the bounds. For example,
#'   May to August is specified by c(5:8) not c(5, 8).
#'
#'   Duration (in days) of each threshold is used primarily to
#'   define large flow events (freshes and overbanks), which often
#'   occur for several days within a month or season. If threshold
#'   instead defines a longer flow duration (e.g. a baseflow), then
#'   duration can be set to Inf. The parameter n specifies the
#'   number of times a given event can occur in a given time
#'   period. This is a vector with two values that specify lower
#'   and upper bounds on the number of events, and can be set to NA
#'   in cases where it is not required (e.g. baseflows).
#'
#'   Change can be used to specify an upper bound on the daily
#'   change in flow. If a daily change exceeds this value, the
#'   second day (i.e. the day of the change) is replaced with
#'   the mean of surrounding values (in an iteratively widening window)
#'   until the difference is less than that specified by change.
#'
#'   Resolution is not checked internally for consistency with timing
#'   or context, so it is assumed that the provided arguments are
#'   appropriate and compatible.
#'
#' @examples
#' \dontrun{
#' #
#'
#' }
generate_scenario <- function(
  x,
  date,
  thresholds,
  timing,
  duration,
  change,
  n,
  context = NULL,
  context_data = NULL,
  resolution = water_year,
  ...
) {

  # should be one threshold per timing
  if (length(thresholds) != length(timing))
    stop("one threshold required per timing", call. = FALSE)

  # and thresholds should be simple vectors if context
  #   is not provided
  if (is.null(context) & any(sapply(thresholds, is.list)))
    stop("context must be provided if thresholds are lists", call. = FALSE)

  # collate x into a list by resolution
  xlist <- define_resolution(x = x, date = date, resolution = resolution)
  datelist <- define_resolution(x = date, date = date, resolution = resolution)

  # evaluate and check context if provided
  if (!is.null(context))
    context <- define_context(xlist, context = context, context_data = context_data, ...)

  # can work through each threshold, one by one
  for (i in seq_along(thresholds)) {

    # grab relevant threshold
    threshold_tmp <- thresholds[[i]]
    duration_tmp <- duration[[i]]
    change_tmp <- change[[i]]
    n_tmp <- n[[i]]

    # and apply it
    if (!is.null(context)) {

      # by context if needed
      threshold_tmp <- threshold_tmp[context]
      duration_tmp = duration_tmp[context]
      change_tmp = change_tmp[context]
      n_tmp = n_tmp[context]
      xlist <- mapply(
        apply_threshold,
        xlist,
        datelist,
        threshold_tmp,
        duration_tmp,
        change_tmp,
        n_tmp,
        MoreArgs = list(timing = timing[[i]])
      )

    } else {

      # fixed thresholds if not
      xlist <- mapply(
        apply_threshold,
        xlist,
        datelist,
        MoreArgs = list(
          threshold = threshold_tmp,
          timing = timing[[i]],
          duration = duration_tmp,
          change = change_tmp,
          n = n_tmp
        )
      )

    }

  }

  # return
  do.call(c, xlist)

}

#' @rdname scenario
#'
#' @export
#'
#' @importFrom lubridate year
#'
#' @param n number of time periods to sample
#' @param transition matrix with one row and column for each level
#'   of context, defining the transition probabilities between each
#'   context pair (e.g. dry to dry, dry to wet). Matrix structures
#'   should reflect columns moving to rows (i.e. x_new = A x_old),
#'   and matrices will be internally standardised so that each
#'   column sums to one
#' @param rep number of replicate scenarios to generate
#' @param init string denoting the initial context
#' @param init_year initial year for resampled dates
#' @param y extra variable to be resampled according to transitions
#'   and contexts defined by \code{x}
#' @param \dots additional arguments passed to \code{context}
#'
#' @details \code{resample_scenario} is a function to resample from
#'   data (e.g. discharge or water temperature) to create sequences
#'   representing plausible future climates. Future climates are
#'   defined by transitions between different contexts (e.g. dry and
#'   wet years), with transition probabilities (and associated return
#'   times) allowing a range of scenarios (e.g. longer droughts,
#'   more frequent extreme events).
#'
#'   Both the resolution and context can be changed to match specific
#'   applications, such as resampling months or weeks and specifying
#'   nuanced contexts based on any features of interest in hydrological
#'   data or other data (provided as \code{context_data}).
#'
#' @examples
#' \dontrun{
#' #
#'
#' }
resample_scenario <- function(
  x,
  date,
  n,
  transition,
  context,
  context_data = NULL,
  resolution = water_year,
  rep = 1,
  init = NULL,
  init_year = NULL,
  y = NULL,
  ...
) {

  # collate x into a list by resolution
  xlist <- define_resolution(x = x, date = date, resolution = resolution)
  datelist <- define_resolution(x = date, date = date, resolution = resolution)

  # repeat for y if provided
  ylist <- NULL
  if (!is.null(y))
    ylist <- define_resolution(x = y, date = date, resolution = resolution)

  # drop partial years
  partial <- sapply(xlist, function(x) length(x) < 365)
  xlist <- xlist[!partial]
  if (!is.null(y))
    ylist <- ylist[!partial]
  datelist <- datelist[!partial]

  # evaluate and check context
  context <- define_context(xlist, context = context, context_data = context_data, ...)

  # check transition and standardise
  if (ncol(transition) != nrow(transition))
    stop("transition must be a square matrix", call. = FALSE)
  if (!all.equal(rownames(transition), colnames(transition)))
    stop("transition must be a square matrix with row and column names matching provided contexts", call. = FALSE)
  transition <- sweep(transition, 2, colSums(transition), "/")

  # and check all required contexts are provided
  if (!all(unique(context) %in% rownames(transition)))
    stop("all contexts must have an associated transition", call. = FALSE)

  # set initial year, defaulting to one year after the final observation
  if (is.null(init_year))
    init_year <- max(year(date)) + 1L

  # draw sequences of years, with rep(licates) as required
  new_states <- lapply(
    seq_len(rep),
    update_state,
    x = xlist,
    date = datelist,
    n = n,
    transition = transition,
    context = context,
    init = init,
    init_year = init_year,
    y = ylist
  )

  # collate values and dates
  out <- data.frame(
    date = new_states[[1]]$date,
    do.call(cbind, lapply(new_states, function(x) x$value))
  )

  # add extra variable if required
  extra_colnames <- NULL
  if (!is.null(y)) {
    out <- cbind(out, do.call(cbind, lapply(new_states, function(x) x$value_additional)))
    extra_colnames <- paste0("value_additional", seq_len(rep))
  }

  # add column names
  colnames(out) <- c("date", paste0("value", seq_len(rep)), extra_colnames)

  # and return
  out

}

#' @rdname scenario
#'
#' @export
#'
#' @importFrom lubridate month year
#'
#' @details \code{water_year} is a function to define
#'   water years from date data. Included here to demonstrate
#'   how similar functions might be specified.
#'
#' @examples
#' \dontrun{
#' #
#'
#' }
water_year <- function(x) {

  # pull out months and years
  month_id <- month(x)
  year_id <- year(x)

  # now adjust years based on month and return
  ifelse(month_id <= 6, year_id - 1L, year_id)

}

#' @rdname scenario
#'
#' @export
#'
#' @importFrom lubridate month year
#'
#' @details \code{calendar_year} is a function to define
#'   calendar years from date data. Included along with
#'   \code{water_year} to demonstrate how similar
#'   functions might be specified.
#'
#' @examples
#' \dontrun{
#' #
#'
#' }
calendar_year <- function(x) {

  # return calendar year directly
  year(x)

}

#' @rdname scenario
#'
#' @export
#'
#' @details \code{define_transition} is a function to define
#'  transition probabilities from existing data and contexts.
#'
#' @examples
#' \dontrun{
#' #
#'
#' }
define_transition <- function(
  x,
  date,
  context,
  context_data = NULL,
  resolution = water_year,
  ...
) {

  # collate x into a list by resolution
  xlist <- define_resolution(x = x, date = date, resolution = resolution)

  # evaluate and check context
  context <- define_context(xlist, context = context, context_data = context_data, ...)

  # create table of pairwise contexts
  unique_contexts <- unique(context)
  ncontext <- length(unique_contexts)
  transition <- matrix(NA, nrow = ncontext, ncol = ncontext)
  rownames(transition) <- colnames(transition) <- unique_contexts

  # and work out probab of moving from one to any other
  for (i in seq_along(unique_contexts)) {
    for (j in seq_along(unique_contexts)) {
      idx <- which(context == unique_contexts[i])
      if (any(idx == 1))
        idx <- idx[idx != 1]
      idy <- which(context[idx - 1] == unique_contexts[j])
      transition[j, i] <- length(idy) / length(idx)
    }
  }

  # return
  transition

}

# break a vector x up into a list with separate elements for
#   each unit of resolution
define_resolution <- function(x, date, resolution) {

  # collate x into a list by resolution
  res <- resolution(date)
  res <- lapply(unique(res), function(.x, .y) .y == .x, .y = res)

  # return
  lapply(res, function(idx, .x) .x[idx], .x = x)

}

# define context from input data (x or context_data) or check
#   that context is an appropriately specified character vector
#   if context is not a function
define_context <- function(x, context, context_data, ...) {

  # evaluate context if it's a function
  if (is.function(context)) {

    if (is.null(context_data)) {
      context <- sapply(x, context, ...)
    } else {
      if (is.list(context_data))
        context <- sapply(context_data, context, ...)
      else
        context <- context(context_data, ...)
    }

  } else {

    # otherwise make sure it's a string vector
    if (!is.character(context))
      stop("context must be a function or character vector", call. = FALSE)

    # with one entry for each element of x
    if (length(context) != length(x))
      stop("character-valued context must have one value for each time period", call. = FALSE)

  }

  # return
  context

}

#'
#' @importFrom lubridate month
#'
# internal function to apply a threshold based on key arguments
apply_threshold <- function(
  x,
  date,
  threshold,
  duration,
  change,
  n,
  timing,
  ntry = 20,
  ...
) {

  # pull out the target months
  idx <- month(date) %in% timing

  # check that at least some dates fall within the timing
  #   otherwise just return x unchanged
  if (any(idx)) {

    # work out duration and number of events if it's a short flow event
    if (any(!is.infinite(duration))) {

      # specify default n if NA
      if (all(is.na(n)))
        n <- c(1, 1)

      # work out number of events needed
      n <- sample(n[1]:n[2], size = 1, replace = FALSE)

      # sample duration if needed
      duration <- sample(duration[1]:duration[2], size = 1, replace = FALSE)

      # loop over all n
      idy <- matrix(FALSE, nrow = length(date), ncol = n)
      for (i in seq_len(n)) {

        # choose a random start date for the flow event,
        #   assuming there's at least one day within this window
        if (any(idx)) {
          start <- sample(which(idx), size = 1)
          end <- start + duration - 1L

          # pull it back if the end date is beyond the final available date
          if (end > max(which(idx))) {
            end <- max(which(idx))
            start <- end - duration + 1L
          }

          # update for that event
          idy[, i][start:end] <- TRUE

          # and drop these out of idx, the available dates for other events,
          #   including a window to ensure events don't overlap
          idx_start <- (start - duration - 1L)
          idx_start <- ifelse(idx_start < 1, 1, idx_start)
          idx[idx_start:(end + duration + 1L)] <- FALSE
        }

      }

      # collapse into a vector for all events
      idx <- apply(idy, 1, any)

    }

    # apply the threshold if not NA
    if (is.na(threshold[2]))
      threshold[2] <- threshold[1]
    if (!is.na(threshold[1]))
      x[idx] <- ifelse(x[idx] < threshold[1], runif(sum(idx), min = threshold[1], max = threshold[2]), x[idx])
    if (!is.na(threshold[3]))
      x[idx] <- ifelse(x[idx] > threshold[3], threshold[3], x[idx])

    # and apply the change rules if required
    ## TODO: remove for now, doing some weird things
    # if (!is.na(change)) {
    #   xdiff <- abs(diff(x[idx]))
    #   to_smooth <- which(xdiff > change)
    #   if (length(to_smooth) > 0) {
    #     for (i in seq_along(to_smooth)) {
    #       diff_cur <- x[idx][to_smooth[i] + 1L] - x[idx][to_smooth[i]]
    #       tries <- 0L
    #       while(diff_cur > change & tries < ntry) {
    #         start <- ifelse(to_smooth[i] > tries, to_smooth[i] - tries, 1)
    #         end <- ifelse((to_smooth[i] + tries + 2L) < sum(idx), to_smooth[i] + tries + 2L, sum(idx))
    #         x[idx][to_smooth[i] + 1L] <-  mean(x[idx][start:end])
    #         tries <- tries + 1L
    #         if (start == 1 & end == sum(idx))
    #           tries <- ntry
    #         diff_cur <- x[idx][to_smooth[i] + 1L] - x[idx][to_smooth[i]]
    #       }
    #     }
    #   }
    # }

  }

  # and return
  x

}

#'
#' @importFrom stats rmultinom
#' @importFrom lubridate day month dmy
#'
# function to initialise and sample years based on a sequence of context
#   and state transition probabilities
update_state <- function(x, date, n, transition, context, init, init_year, y = NULL, ...) {

  # define new sequence starting from init
  all_states <- rownames(transition)
  if (is.null(init))
    init <- sample(context, size = 1)
  state <- matrix(0, nrow = length(all_states), ncol = n)
  state[all_states == init, 1] <- 1
  for (i in seq_len(n - 1))
    state[, i + 1] <- rmultinom(n = 1, size = 1, prob = transition %*% state[, i])

  # and sample random elements of x for each state
  idx <- rep(NA, times = n)
  context_set <- all_states[apply(state, 2, function(x) which(x == 1))]
  for (i in seq_len(n))
    idx[i] <- sample(seq_along(x)[context == context_set[i]], size = 1)

  # define dates with updated years
  date <- date[idx]
  for (i in seq_along(date)) {

    # need to work out if dates are water years or not
    if (month(date[[i]])[1] == 7) {
      year_set <- ifelse(month(date[[i]]) > 6, init_year + i - 1L, init_year + i)
    } else {
      year_set <- rep(init_year + i, length(date[[i]]))
    }

    # and paste together with days and months
    date[[i]] <- dmy(
      paste(
        day(date[[i]]),
        month(date[[i]]),
        year_set,
        sep = "-"
      ),
      quiet = TRUE
    )

  }

  # flatten outputs
  out <- data.frame(
    date = do.call(c, date),
    value = do.call(c, x[idx])
  )

  # add extra variables if required
  if (!is.null(y))
    out$value_additional <- do.call(c, y[idx])

  # remove Feb 29 from any sequences to keep everything the same length
  idy <- day(out$date) == 29 & month(out$date) == 2
  out <- out[!idy, ]

  # check for NAs in dates (leap years may cause this) and remove
  out <- out[!is.na(out$date), ]

  # return
  out

}

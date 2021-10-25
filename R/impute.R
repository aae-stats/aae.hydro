#' @name impute
#' @title Impute missing streamflow or water temperature data
#' @description Functions to impute missing hydrological data using
#'   nearby weather data or rolling means.
NULL

#' @rdname impute
#'
#' @export
#'
#' @param x kdjf
#' @param source dkjf
#' @param threshold kdjfd
#'
#' @return a data.frame
#'
impute_year <- function(x, source = NULL, threshold = 100) {

  NULL

}

#' @rdname impute
#'
#' @export
#'
#' @param x kdjf
#' @param n dkjf
#' @param fun kdjfd
#'
#' @return a data.frame
#'
impute_rolling <- function(x, n = 5, fun = median) {

  NULL

}

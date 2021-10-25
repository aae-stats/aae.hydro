#' @name rescale
#' @title Rescale hydrological data to reflect climate change scenarios
#' @description Functions to rescale hydrological data to reflect scenarios
#'   of climate change using seasonal decile rescaling or averaged
#'   outputs from global climate models.
NULL

#' @rdname rescale
#'
#' @export
#'
#' @param x dkjf
#' @param target dkfjd
#' @param season dkjfd
#'
#' @return a vector
#'
decile_rescale <- function(x, target, season = NULL) {

  NULL

}


#' @rdname rescale
#'
#' @export
#'
#' @param x dkjf
#' @param variable
#' @param catchment
#'
#' @return a vector
#'
gcm_rescale <- function(x, variable, catchment) {

  # check inputs
  if (!variable %in% c("discharge", "water_temperature")) {
    stop(
      "variable must be one of discharge or water_temperature ",
      call. = FALSE
    )
  }
  if (!catchment %in% catchment_list) {
    stop(
      "catchment must be one of XYZ",
      call. = FALSE
    )
  }

  NULL

}

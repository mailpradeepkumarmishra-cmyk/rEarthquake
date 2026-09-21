#' Clean earthquake location names
#'
#' Removes the country name and colon from the NOAA
#' LOCATION_NAME variable and converts the remaining
#' location name to title case.
#'
#' @param x A character vector containing earthquake
#'   location names.
#'
#' @return A character vector containing cleaned
#'   location names.
#'
#' @examples
#' eq_location_clean("JORDAN: BAB-A-DARAA,AL-KARAK")
#'
#' @export
eq_location_clean <- function(x) {
  
  x <- sub("^[^:]+:\\s*", "", x)
  
  stringr::str_to_title(x)
}

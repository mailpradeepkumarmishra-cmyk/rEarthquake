#' Clean NOAA significant earthquake data
#'
#' Creates a DATE variable from YEAR, MONTH, and DAY,
#' converts latitude and longitude to numeric values,
#' and cleans earthquake location names.
#'
#' @param data A data frame containing NOAA significant
#'   earthquake data.
#'
#' @return A data frame containing the cleaned earthquake data.
#'
#' @export
eq_clean_data <- function(data) {
  
  data <- tidyr::unite(
    data,
    "DATE",
    "YEAR",
    "MONTH",
    "DAY",
    sep = "-",
    remove = FALSE,
    na.rm = FALSE
  )
  
  valid_date <- !is.na(data$YEAR) &
    data$YEAR >= 1 &
    !is.na(data$MONTH) &
    !is.na(data$DAY)
  
  date_result <- as.Date(
    rep(NA_character_, nrow(data))
  )
  
  date_result[valid_date] <- as.Date(
    data$DATE[valid_date],
    format = "%Y-%m-%d"
  )
  
  data <- dplyr::mutate(
    data,
    DATE = date_result,
    LATITUDE = as.numeric(data$LATITUDE),
    LONGITUDE = as.numeric(data$LONGITUDE),
    LOCATION_NAME = eq_location_clean(data$LOCATION_NAME)
  )
  
  data
}

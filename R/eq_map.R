#' Map earthquake epicenters
#'
#' Creates an interactive leaflet map showing earthquake epicenters.
#' Each earthquake is represented by a circle whose radius is
#' proportional to the earthquake magnitude.
#'
#' The column used for the popup annotation is selected using
#' the `annot_col` argument.
#'
#' @param data A data frame containing cleaned earthquake data.
#' @param annot_col Character string giving the name of the column
#'   to use for popup annotations.
#'
#' @return A leaflet map object.
#'
#' @examples
#' \dontrun{
#' mexico_data <- readr::read_delim(
#'   "earthquakes.tsv.gz",
#'   delim = "\t"
#' ) |>
#'   eq_clean_data() |>
#'   dplyr::filter(
#'     COUNTRY == "MEXICO",
#'     !is.na(DATE)
#'   )
#'
#' eq_map(
#'   mexico_data,
#'   annot_col = "DATE"
#' )
#' }
#'
#' @export
eq_map <- function(
    data,
    annot_col
) {
  
  # Convert earthquake magnitude to numeric
  data$EQ_PRIMARY <- as.numeric(
    trimws(
      as.character(data$EQ_PRIMARY)
    )
  )
  
  # Check that the requested annotation column exists
  if (!annot_col %in% names(data)) {
    stop(
      "Column specified by annot_col does not exist in data."
    )
  }
  
  # Keep observations with valid coordinates
  data <- data[
    !is.na(data$LATITUDE) &
      !is.na(data$LONGITUDE),
  ]
  
  leaflet::leaflet(data) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~LONGITUDE,
      lat = ~LATITUDE,
      radius = ~EQ_PRIMARY,
      popup = data[[annot_col]]
    )
}

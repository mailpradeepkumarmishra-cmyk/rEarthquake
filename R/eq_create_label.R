#' Create HTML labels for earthquake map popups
#'
#' Creates an HTML character string for each earthquake containing
#' its cleaned location, magnitude, and total number of deaths.
#'
#' The labels use boldface for "Location", "Magnitude", and
#' "Total deaths". If a value is missing, that element is omitted
#' from the corresponding label.
#'
#' @param data A data frame containing cleaned earthquake data.
#'
#' @return A character vector containing HTML labels.
#'
#' @examples
#' \dontrun{
#' popup_text <- eq_create_label(earthquake_data)
#' }
#'
#' @export
eq_create_label <- function(data) {
  
  location <- eq_location_clean(
    data$LOCATION_NAME
  )
  
  magnitude <- trimws(
    as.character(data$EQ_PRIMARY)
  )
  
  deaths <- trimws(
    as.character(data$TOTAL_DEATHS)
  )
  
  labels <- vector(
    "character",
    nrow(data)
  )
  
  for (i in seq_len(nrow(data))) {
    
    parts <- character(0)
    
    if (
      !is.na(location[i]) &&
      location[i] != ""
    ) {
      parts <- c(
        parts,
        paste0(
          "<b>Location:</b> ",
          location[i]
        )
      )
    }
    
    if (
      !is.na(magnitude[i]) &&
      magnitude[i] != ""
    ) {
      parts <- c(
        parts,
        paste0(
          "<b>Magnitude:</b> ",
          magnitude[i]
        )
      )
    }
    
    if (
      !is.na(deaths[i]) &&
      deaths[i] != ""
    ) {
      parts <- c(
        parts,
        paste0(
          "<b>Total deaths:</b> ",
          deaths[i]
        )
      )
    }
    
    labels[i] <- paste(
      parts,
      collapse = "<br>"
    )
  }
  
  labels
}

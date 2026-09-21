#' Add labels to an earthquake timeline
#'
#' Adds labels to earthquake points on a timeline. A vertical line
#' connects each earthquake to its label.
#'
#' When `n_max` is supplied, only the `n_max` earthquakes with the
#' largest magnitudes are labelled.
#'
#' The x aesthetic should contain earthquake dates and the label
#' aesthetic should contain the variable used for annotation,
#' such as `LOCATION_NAME`.
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`.
#' @param data A data frame containing earthquake data.
#' @param stat Statistical transformation to use.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param n_max Maximum number of earthquakes to label. The earthquakes
#'   with the largest `EQ_PRIMARY` values are selected.
#' @param na.rm Logical indicating whether missing values should be
#'   silently removed.
#' @param show.legend Logical indicating whether the layer should
#'   appear in the legend.
#' @param inherit.aes Logical indicating whether to inherit aesthetics
#'   from the plot.
#' @param ... Additional arguments passed to the layer.
#'
#' @return A ggplot2 layer containing earthquake labels.
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(
#'   earthquake_data,
#'   ggplot2::aes(
#'     x = DATE,
#'     y = COUNTRY,
#'     label = LOCATION_NAME
#'   )
#' ) +
#'   geom_timeline_label(n_max = 5)
#' }
#'
#' @export
geom_timeline_label <- function(
    mapping = NULL,
    data = NULL,
    stat = StatTimelineLabel,
    position = "identity",
    n_max = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  
  ggplot2::layer(
    stat = stat,
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n_max = n_max,
      na.rm = na.rm,
      ...
    )
  )
}


# Statistical transformation used by geom_timeline_label
StatTimelineLabel <- ggplot2::ggproto(
  "StatTimelineLabel",
  ggplot2::Stat,
  
  required_aes = c("x", "label"),
  
  compute_panel = function(
    data,
    scales,
    n_max = NULL
  ) {
    
    # If n_max is not supplied, retain all observations
    if (is.null(n_max)) {
      return(data)
    }
    
    # EQ_PRIMARY is the earthquake magnitude.
    # Convert it to numeric in case it is stored as character.
    magnitude <- suppressWarnings(
      as.numeric(
        trimws(
          as.character(data$EQ_PRIMARY)
        )
      )
    )
    
    # Remove observations with missing magnitude
    valid <- !is.na(magnitude)
    
    data <- data[valid, ]
    magnitude <- magnitude[valid]
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    # Order earthquakes from largest to smallest magnitude
    keep <- order(
      magnitude,
      decreasing = TRUE
    )
    
    # Retain at most n_max observations
    keep <- keep[
      seq_len(
        min(n_max, length(keep))
      )
    ]
    
    data[keep, ]
  }
)


# Graphical object used by geom_timeline_label
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  
  required_aes = c("x", "label"),
  
  default_aes = ggplot2::aes(
    y = 0,
    colour = "black",
    alpha = 1,
    EQ_PRIMARY = NULL
  ),
  
  draw_panel = function(
    data,
    panel_params,
    coord,
    na.rm = FALSE
  ) {
    
    coords <- coord$transform(
      data,
      panel_params
    )
    
    # Remove rows with missing x or y
    keep <- is.finite(coords$x) &
      is.finite(coords$y)
    
    coords <- coords[keep, ]
    
    if (nrow(coords) == 0) {
      return(grid::nullGrob())
    }
    
    # Position labels above the timeline
    label_y <- coords$y + 0.10
    
    # Vertical lines connecting earthquake points
    # to their labels
    vertical_lines <- grid::segmentsGrob(
      x0 = coords$x,
      x1 = coords$x,
      y0 = coords$y,
      y1 = label_y,
      default.units = "npc",
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha
      )
    )
    
    # Text labels
    text_labels <- grid::textGrob(
      label = coords$label,
      x = coords$x,
      y = label_y,
      default.units = "npc",
      just = "bottom",
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha,
        fontsize = 8
      )
    )
    
    grid::grobTree(
      vertical_lines,
      text_labels
    )
  },
  
  draw_key = ggplot2::draw_key_blank
)
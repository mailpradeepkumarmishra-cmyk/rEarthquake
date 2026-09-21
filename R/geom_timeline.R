#' Draw a timeline of earthquakes
#'
#' Creates a custom ggplot2 geom for displaying earthquakes on a
#' horizontal timeline. Each earthquake is represented by a point.
#' Multiple timelines can be created by mapping a variable to `y`,
#' such as country.
#'
#' The x aesthetic should contain earthquake dates. The optional
#' `xmin` and `xmax` arguments restrict the earthquakes displayed
#' to a specified date range.
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`.
#' @param data A data frame containing earthquake data.
#' @param stat Statistical transformation to use. Defaults to
#'   `StatTimeline`.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param xmin Minimum date to display.
#' @param xmax Maximum date to display.
#' @param na.rm Logical indicating whether missing values should be
#'   silently removed.
#' @param show.legend Logical indicating whether the layer should
#'   appear in the legend.
#' @param inherit.aes Logical indicating whether to inherit aesthetics
#'   from the plot.
#' @param ... Additional arguments passed to the layer.
#'
#' @return A ggplot2 layer representing an earthquake timeline.
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(
#'   earthquake_data,
#'   ggplot2::aes(
#'     x = DATE,
#'     y = COUNTRY
#'   )
#' ) +
#'   geom_timeline()
#' }
#'
#' @export
geom_timeline <- function(
    mapping = NULL,
    data = NULL,
    stat = "timeline",
    position = "identity",
    xmin = NULL,
    xmax = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  
  ggplot2::layer(
    stat = StatTimeline,
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmin = xmin,
      xmax = xmax,
      na.rm = na.rm,
      ...
    )
  )
}


# Statistical transformation used by geom_timeline
StatTimeline <- ggplot2::ggproto(
  "StatTimeline",
  ggplot2::Stat,
  
  required_aes = c("x"),
  
  compute_group = function(
    data,
    scales,
    xmin = NULL,
    xmax = NULL
  ) {
    
    if (!is.null(xmin)) {
      data <- data[data$x >= xmin, ]
    }
    
    if (!is.null(xmax)) {
      data <- data[data$x <= xmax, ]
    }
    
    data
  }
)


# Graphical object used by geom_timeline
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  
  required_aes = c("x"),
  
  default_aes = ggplot2::aes(
    y = 0,
    colour = "black",
    size = 1,
    alpha = 1
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
    
    # Remove rows with missing coordinates
    coords <- coords[
      is.finite(coords$x) &
        is.finite(coords$y),
    ]
    
    if (nrow(coords) == 0) {
      return(grid::nullGrob())
    }
    
    # Create a separate horizontal timeline
    # for each y level
    y_levels <- sort(unique(coords$y))
    
    timeline <- grid::segmentsGrob(
      x0 = min(coords$x, na.rm = TRUE),
      x1 = max(coords$x, na.rm = TRUE),
      y0 = y_levels,
      y1 = y_levels,
      gp = grid::gpar(
        col = "grey50",
        lwd = 1
      )
    )
    
    # Draw earthquake points
    points <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = 19,
      size = grid::unit(
        coords$size * 2,
        "mm"
      ),
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha
      )
    )
    
    grid::grobTree(
      timeline,
      points
    )
  },
  
  draw_key = ggplot2::draw_key_point
)

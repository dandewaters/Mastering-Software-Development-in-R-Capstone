#' @title Construct the geom_timeline class
#'
#' @description Plots a timeline of earthquake events in the NOAA dataset with
#'              size and color of plotted points indicating information about
#'              each earthquake.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom dplyr arrange
#' @importFrom grid segmentsGrob
#' @importFrom grid circleGrob
#' @importFrom grid gpar
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
GeomTimeline <-
  ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   # Aesthetics
                   required_aes = c("x"),
                   default_aes = ggplot2::aes(y = 0.5, size=1, color="grey50", alpha = 0.5,
                                              shape=19, stroke=0.5, fill=NA, n_max = 5),

                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {

                     # Transform data
                     coords <- coord$transform(data, panel_scales)

                     # Scale points size
                     coords$size <- coords$size * 0.01

                     # Construct timeline
                     g_timeline <-
                       grid::segmentsGrob(default.units="npc",
                                          x0 = 0,
                                          y0 = coords$y,
                                          x1 = 1,
                                          y1 = coords$y,
                                          gp = gpar(lwd = 3,
                                                    alpha = 1,
                                                    col = "black"))
                     # Construct grid points
                     g_points <-
                       grid::circleGrob(x = coords$x,
                                        y = coords$y,
                                        r = coords$size,
                                        default.units="npc",
                                        gp = grid::gpar(colour = coords$colour,
                                                        alpha = coords$alpha,
                                                        fill = coords$fill))

                     grid::gTree(children = grid::gList(g_timeline, g_points))
                   }
)



#' @title Create geom_timeline Layer
#'
#' @description Plots a timeline of earthquake events in the NOAA dataset with
#'              size and color of plotted points indicating information about
#'              each earthquake.
#'
#'
#' @param mapping An aesthetic mapping
#' @param data A data frame of earthquake observations
#' @param stat A string of the stat applied to the data for plot layer
#' @param position A string of the position adjustment
#' @param na.rm A logical to remove NAs from data
#' @param show.legend A logical to display the legend
#' @param inherit.aes A logical to override the default aesthetics
#' @param ... Other arguments passed on to methods.
#'
#' @return a ggproto layer to plot a timeline of earthquakes with their magnitude and death toll
#'
#' @examples # Read in raw NOAA data
#'           \dontrun{raw_data <- eq_read_data("./inst/extdata/earthquakes.tsv")}
#'           \dontrun{clean_data <- eq_clean_data(raw_data)}
#'           \dontrun{min_date <- ymd("2020-01-01")}
#'           \dontrun{max_date <- ymd("2020-01-31")}
#'           \dontrun{data <- filter(clean_data, date >= min_date & date <= max_date)}
#'           \dontrun{ggplot(data, aes(x=date, size=magnitude, fill=deaths, color=deaths,)) +
#'                    geom_timeline()}
#'
#' @importFrom ggplot2 layer
#'
#' @seealso GeomTimeLine
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {

  ggplot2::layer(geom = GeomTimeline,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

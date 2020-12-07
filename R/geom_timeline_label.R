#' @title Construct the geom_timeline class
#'
#' @description plots a timeline of the earthquakes in the NOAA data set with their magnitude and number of deaths
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom dplyr arrange
#' @importFrom grid segmentsGrob
#' @importFrom grid circleGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
GeomTimelineLabel <-
  ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                   # Aesthetics
                   required_aes = c("x", "label"),
                   default_aes = ggplot2::aes(y = 0.5, size=1, color="grey50", alpha = 0.5,
                                              shape=19, stroke=0.5, fill=NA, n_max = 5),

                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {
                     coords <- coord$transform(data, panel_scales)

                     # Keep only top_n earthquakes labels
                     n_max = coords$n_max[1]
                     coords <- dplyr::arrange(coords, desc(size))
                     coords$label[(n_max + 1):length(coords$label)] <- ""

                     # Adjust the size of the circle Grob
                     coords$size <- coords$size * 0.01

                     # Construct timeline
                     g_timeline <-
                       grid::segmentsGrob(default.units="npc",
                                          x0 = 0,
                                          y0 = coords$y,
                                          x1 = 1,
                                          y1 = coords$y,
                                          gp = gpar(lwd = 2,
                                                    alpha = 0.8,
                                                    col = "black"))
                     # Construct points
                     g_points <-
                       grid::circleGrob(x = coords$x,
                                        y = coords$y,
                                        r = coords$size,
                                        default.units="npc",
                                        gp = grid::gpar(colour = coords$colour,
                                                        alpha = coords$alpha,
                                                        fill = coords$fill))

                     # Construct labels
                     g_labels <-
                       grid::textGrob(label = coords$label,
                                      x = coords$x,
                                      y = coords$y + 0.1,
                                      default.units = "npc",
                                      rot = 30,
                                      just = "left",
                                      gp = grid::gpar(fontsize = 10))

                     # Construct lines to that point to labels
                     g_lines <-
                       grid::segmentsGrob(default.units="npc",
                                          x0 = coords$x[coords$label != ""],
                                          x1 = coords$x[coords$label != ""],
                                          y0 = coords$y[coords$label != ""],
                                          y1 = coords$y[coords$label != ""] + 0.1,
                                          gp = gpar(lwd = 1,
                                                    alpha = 1,
                                                    col = "black"))

                     grid::gTree(children = grid::gList(g_timeline, g_points, g_labels, g_lines))
                     }
)



#' @title Create geom_timeline_label Layer
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
#' @return a ggproto layer to plot labels of top earthquakes by magnitude
#'
#'@examples # Read in raw NOAA data
#'          \dontrun{raw_data <- eq_read_data("./inst/extdata/earthquakes.tsv")}
#'          \dontrun{clean_data <- eq_clean_data(raw_data)}
#'          \dontrun{min_date <- ymd("2020-01-01")}
#'          \dontrun{max_date <- ymd("2020-01-31")}
#'          \dontrun{data <- filter(clean_data, date >= min_date & date <= max_date)}
#'          \dontrun{ggplot(data, aes(x=date, label=location_name,
#'                                    size=magnitude, fill=deaths,
#'                                    color=deaths, n_max=5)) +
#'                   geom_timeline_label()}
#'
#' @importFrom ggplot2 layer
#'
#' @seealso GeomTimeLine
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {

    ggplot2::layer(geom = GeomTimelineLabel,
                   mapping = mapping,
                   data = data,
                   stat = stat,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(na.rm = na.rm, ...))
}

#' sspLNIRT Plot Theme
#'
#' @description
#' A consistent ggplot2 theme used across all plotting functions in the
#' package. Extends [ggplot2::theme_minimal()] with tighter panel grids,
#' consistent strip styling for facets, and slightly larger axis text.
#'
#' Use as the final layer of any ggplot, e.g.
#' `p + theme_sspLNIRT()`.
#'
#' @param base_size Numeric. Base font size in points. Default `11`.
#' @param base_family Character. Base font family. Default `""` (system).
#'
#' @return A [ggplot2::theme] object.
#'
#' @seealso [scale_colour_sspLNIRT()], [scale_fill_sspLNIRT()] for the
#'   matching discrete palette helpers; [plot.sspLNIRT()] for the main
#'   plotting entry point.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_sspLNIRT()
#' }
#'
#' @export
theme_sspLNIRT <- function(base_size = 11, base_family = "") {

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title           = ggplot2::element_text(face = "bold",
                                                   size = base_size + 1,
                                                   margin = ggplot2::margin(b = 6)),
      plot.subtitle        = ggplot2::element_text(colour = "grey30",
                                                   margin = ggplot2::margin(b = 6)),
      plot.caption         = ggplot2::element_text(colour = "grey40",
                                                   size = base_size - 2,
                                                   hjust = 1),
      axis.title           = ggplot2::element_text(colour = "grey20"),
      axis.text            = ggplot2::element_text(colour = "grey25"),
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major     = ggplot2::element_line(colour = "grey92",
                                                   linewidth = 0.3),
      strip.background     = ggplot2::element_rect(fill = "grey95",
                                                   colour = NA),
      strip.text           = ggplot2::element_text(face = "bold",
                                                   colour = "grey15",
                                                   margin = ggplot2::margin(t = 3, b = 3)),
      legend.position      = "right",
      legend.key.size      = ggplot2::unit(0.9, "lines"),
      legend.title         = ggplot2::element_text(face = "plain", size = base_size - 1),
      plot.margin          = ggplot2::margin(8, 8, 8, 8)
    )
}

#' Internal sspLNIRT colour palette
#'
#' @description
#' A perceptually-ordered grey-to-blue ramp used by the package plots. Returns
#' `n` colours sampled from a fixed anchor sequence.
#'
#' @param n Integer. Number of colours to return.
#' @return Character vector of hex colour codes of length `n`.
#' @noRd
sspLNIRT_palette <- function(n) {
  anchors <- c("#2A2A2A", "#4C5B72", "#6E8AA8", "#9CB7CF", "#C7D6E4")
  if (n <= length(anchors)) return(anchors[seq_len(n)])
  grDevices::colorRampPalette(anchors)(n)
}

#' Discrete colour / fill scales for sspLNIRT plots
#'
#' @description
#' Discrete ggplot2 scales matching [theme_sspLNIRT()]. The palette is a
#' grey-to-slate-blue ramp.
#'
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 discrete scale.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   scale_colour_sspLNIRT() +
#'   theme_sspLNIRT()
#' }
#'
#' @name sspLNIRT_scales
#' @export
scale_colour_sspLNIRT <- function(...) {
  ggplot2::discrete_scale("colour", palette = sspLNIRT_palette, ...)
}

#' @rdname sspLNIRT_scales
#' @export
scale_fill_sspLNIRT <- function(...) {
  ggplot2::discrete_scale("fill", palette = sspLNIRT_palette, ...)
}

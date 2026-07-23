#' Plot Method for sspLNIRT Objects
#'
#' @description
#' S3 method for the [base::plot()] generic, dispatched on objects of class
#' `"sspLNIRT"` (returned by [optim_sample()], [comp_rmse()], or
#' [get_sspLNIRT()]). The `type` argument selects the visualization:
#'
#' \describe{
#'   \item{`"estimation"` (default)}{Per-parameter RMSE or bias as a function
#'     of the true (simulated) parameter value, faceted by parameter. See
#'     [plot_estimation()].}
#'   \item{`"power_curve"`}{Optimization trace as a log-log power curve plus
#'     the original-scale view, with the RMSE threshold and minimum \eqn{N}
#'     overlaid. Requires an [optim_sample()] result. See
#'     [plot_power_curve()].}
#' }
#'
#' Arguments specific to each `type` are passed through `...`.
#'
#' @param x An object of class `"sspLNIRT"`.
#' @param type Character. One of `"estimation"` or `"power_curve"`.
#' @param ... Additional arguments passed to the underlying plot helper.
#'   For `type = "estimation"`: `pars` (`"item"` / `"person"`),
#'   `y.val` (`"rmse"` / `"bias"`), `n.bins`. For `type = "power_curve"`:
#'   `out.par`, `thresh`.
#' @param y Unused (required by the generic). Ignored with a warning if
#'   supplied.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [plot_estimation()], [plot_power_curve()], [theme_sspLNIRT()].
#'
#' @examples
#' \dontrun{
#' result <- get_sspLNIRT(
#'   thresh = 0.10, out.par = "alpha",
#'   K = 30, mu.alpha = 1,
#'   meanlog.sigma2 = log(0.6), rho = 0.2
#' )
#'
#' # estimation accuracy
#' plot(result$object, type = "estimation", pars = "item", y.val = "rmse")
#'
#' # power curve from the optimization trace
#' plot(result$object, type = "power_curve",
#'      out.par = "alpha", thresh = 0.10)
#' }
#'
#' @method plot sspLNIRT
#' @exportS3Method plot sspLNIRT
plot.sspLNIRT <- function(x,
                          y = NULL,
                          type = c("estimation", "power_curve"),
                          ...) {

  if (!is.null(y))
    warning("'y' is ignored by plot.sspLNIRT().", call. = FALSE)

  type <- match.arg(type)

  switch(
    type,
    "estimation"  = plot_estimation_internal(x, ...),
    "power_curve" = plot_power_curve_internal(x, ...)
  )
}

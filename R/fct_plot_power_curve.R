#' Plot Power Curve from Sample Size Optimization (worker)
#'
#' @inheritParams plot_power_curve
#' @return A [ggplot2::ggplot] object.
#' @keywords internal
#' @noRd
plot_power_curve_internal <- function(object,
                                      out.par = NULL,
                                      thresh) {

  if (!inherits(object, "sspLNIRT"))
    stop("'object' must be an sspLNIRT object from optim_sample().")

  if (is.null(out.par)) {
    cand <- names(object$res.best)
    if (is.null(cand) || length(cand) == 0)
      stop("'out.par' is missing and could not be inferred from 'object'.")
    out.par <- cand[1]
  }

  ## Input checks
  if (!is.character(out.par) || length(out.par) != 1)
    stop("'out.par' must be a single character string.")
  if (!out.par %in% c("alpha", "beta", "phi", "lambda"))
    stop("'out.par' must be one of: alpha, beta, phi, lambda.")
  if (!is.numeric(thresh) || length(thresh) != 1 || thresh <= 0)
    stop("'thresh' must be a single positive number.")
  if (is.null(object$trace))
    stop("'object' must be from optim_sample() containing a $trace element.")

  trace <- object$trace
  if (is.null(trace$track.N) || is.null(trace$track.res))
    stop("'object$trace' must contain $track.N and $track.res.")

  res_col <- paste0("res.temp.", out.par)
  if (!res_col %in% names(trace$track.res))
    stop("No results found for out.par = '", out.par, "' in trace.")

  ## Extract trace
  N_vec   <- trace$track.N$N.temp
  res_vec <- trace$track.res[[res_col]]

  if (length(N_vec) != length(res_vec))
    stop("Lengths of tracked N and result vectors do not match.")
  if (length(N_vec) < 3)
    stop("Cannot fit power curve. Optimization stopped early: ", object$N.min)

  keep <- N_vec > 0 & res_vec > 0
  if (sum(keep) < 3)
    stop("Fewer than 3 valid (positive) trace points; cannot fit power curve.")

  N_vec   <- N_vec[keep]
  res_vec <- res_vec[keep]

  ## Fit log-log model
  fit <- stats::lm(log(res_vec) ~ log(N_vec))
  a   <- as.numeric(stats::coef(fit)[1])
  b   <- as.numeric(stats::coef(fit)[2])
  r2  <- summary(fit)$r.squared

  N.min <- object$N.min

  ## Build plot data
  N_upper <- if (is.numeric(N.min)) max(max(N_vec), N.min * 1.1) else max(N_vec)
  N_grid  <- seq(min(N_vec), N_upper, length.out = 200)

  points_df <- rbind(
    data.frame(x = log(N_vec), y = log(res_vec), panel = "Log-log scale"),
    data.frame(x = N_vec,      y = res_vec,      panel = "Original scale")
  )

  line_df <- data.frame(
    x = N_grid, y = exp(a) * N_grid^b, panel = "Original scale"
  )

  abline_df <- data.frame(
    x = log(N_grid), y = a + b * log(N_grid), panel = "Log-log scale"
  )

  hline_df <- rbind(
    data.frame(yint = log(thresh), panel = "Log-log scale"),
    data.frame(yint = thresh,      panel = "Original scale")
  )

  pal        <- sspLNIRT_palette(2)
  point_col  <- pal[2]
  line_col   <- pal[1]

  ## Plot
  p <- ggplot2::ggplot(points_df, ggplot2::aes(x = .data[["x"]],
                                               y = .data[["y"]])) +
    ggplot2::geom_point(size = 1.8, alpha = 0.6, colour = point_col) +
    ggplot2::geom_line(data = abline_df, colour = line_col, linewidth = 0.7) +
    ggplot2::geom_line(data = line_df,   colour = line_col, linewidth = 0.7) +
    ggplot2::geom_hline(data = hline_df,
                        ggplot2::aes(yintercept = .data[["yint"]]),
                        linetype = "dashed", colour = "grey45", linewidth = 0.5) +
    ggplot2::facet_wrap(~ panel, scales = "free") +
    ggplot2::labs(
      x       = "Sample Size",
      y       = paste0("RMSE of ", out.par),
      caption = paste0("N.min = ", N.min, " | R.sq = ", round(r2, 3))
    ) +
    theme_sspLNIRT()

  if (is.numeric(N.min)) {
    p <- p + ggplot2::geom_vline(
      data = data.frame(xint = N.min, panel = "Original scale"),
      ggplot2::aes(xintercept = .data[["xint"]]),
      linetype = "dotted", colour = "grey45", linewidth = 0.5
    )
  }

  p
}

#' Plot Power Curve from Sample Size Optimization
#'
#' @description
#' Extracts the optimization trace from an [optim_sample()] result, fits a
#' log-log regression (\eqn{\log(\mathrm{RMSE}) \sim \log(N)}), and displays
#' the relationship on both log-log and original scales side by side. The
#' RMSE threshold and minimum \eqn{N} are overlaid as reference lines.
#'
#' This function is preserved for backward compatibility. New code should
#' prefer `plot(object, type = "power_curve", ...)`.
#'
#' @param object An object of class `"sspLNIRT"` containing a `$trace`
#'   element, as returned by [optim_sample()] or retrieved via
#'   [get_sspLNIRT()].
#' @param out.par Character or `NULL`. Which item parameter's RMSE trace to
#'   plot (one of `"alpha"`, `"beta"`, `"phi"`, `"lambda"`). When `NULL`
#'   (default), the first name of `object$res.best` is used; if that is also
#'   unavailable, an error is raised.
#' @param thresh Numeric. The RMSE threshold used in the optimization. Must
#'   be a single positive number.
#'
#' @return A [ggplot2::ggplot] object with two facets (log-log scale and
#'   original scale).
#'
#' @seealso [plot.sspLNIRT()] for the recommended interface;
#'   [optim_sample()] for producing the trace; [plot_estimation()] for
#'   visualizing estimation accuracy by parameter value.
#'
#' @examples
#' \dontrun{
#' result <- get_sspLNIRT(
#'   thresh = 0.10, out.par = "alpha",
#'   K = 30, mu.alpha = 1,
#'   meanlog.sigma2 = log(0.6), rho = 0.2
#' )
#' plot_power_curve(result$object, out.par = "alpha", thresh = 0.10)
#' # equivalent and preferred:
#' plot(result$object, type = "power_curve", out.par = "alpha", thresh = 0.10)
#' }
#'
#' @export
plot_power_curve <- function(object, out.par = NULL, thresh) {
  plot_power_curve_internal(object  = object,
                            out.par = out.par,
                            thresh  = thresh)
}

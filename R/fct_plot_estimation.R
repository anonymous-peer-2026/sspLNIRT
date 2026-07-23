#' Plot Estimation Metrics by Parameter Value (worker)
#'
#' @description
#' Internal worker that builds the estimation-accuracy plot. End users should
#' call [plot.sspLNIRT()] with `type = "estimation"`, or the back-compat
#' [plot_estimation()] wrapper.
#'
#' @inheritParams plot_estimation
#' @return A [ggplot2::ggplot] object.
#' @keywords internal
#' @noRd
plot_estimation_internal <- function(object,
                                     pars   = "item",
                                     y.val  = "rmse",
                                     n.bins = 30) {

  ## Input checks
  if (!inherits(object, "sspLNIRT"))
    stop("'object' must be an sspLNIRT object from comp_rmse() or optim_sample().")
  if (!is.numeric(n.bins) || length(n.bins) != 1 || n.bins < 2)
    stop("'n.bins' must be a single integer >= 2.")

  pars  <- match.arg(pars,  choices = c("item", "person"))
  y.val <- match.arg(y.val, choices = c("rmse", "bias"))

  ## Extract error data
  if (!is.null(object$comp.rmse)) {
    err.dat.list <- object$comp.rmse$err.dat
  } else if (!is.null(object$err.dat)) {
    err.dat.list <- object$err.dat
  } else {
    stop("'object' must contain either $comp.rmse or $err.dat.")
  }

  err.dat <- err.dat.list[[pars]]
  if (is.null(err.dat))
    stop("No error data found for pars = '", pars, "'.")

  ## Detect format and bin
  is_binned <- all(c("mean_sim", "mean_err", "mean_rmse") %in% names(err.dat))
  is_full   <- all(c("rep", "par", "sim.val", "err")      %in% names(err.dat))

  if (is_binned) {
    bin_means <- err.dat
  } else if (is_full) {
    bin_means <- do.call(rbind, lapply(
      split(err.dat, err.dat$par),
      function(d) {
        d$bin <- as.integer(cut(rank(d$sim.val, ties.method = "first"),
                                breaks = n.bins, labels = FALSE))
        do.call(rbind, lapply(split(d, d$bin), function(b) {
          data.frame(
            par       = b$par[1],
            bin       = b$bin[1],
            mean_sim  = mean(b$sim.val, na.rm = TRUE),
            mean_err  = mean(b$err,     na.rm = TRUE),
            mean_rmse = sqrt(mean(b$err^2, na.rm = TRUE))
          )
        }))
      }
    ))
    rownames(bin_means) <- NULL
  } else {
    stop("Unrecognized error data format. Expected either full or binned data.")
  }

  ## Filter sigma2 for item parameters
  if (pars == "item")
    bin_means <- bin_means[bin_means$par != "sigma2", ]

  ## y-axis
  if (y.val == "bias") {
    Y     <- "mean_err"
    y_lab <- "Mean Bias per Bin"
  } else {
    Y     <- "mean_rmse"
    y_lab <- "Mean RMSE per Bin"
  }

  n_bins_actual <- length(unique(bin_means$bin))
  point_colour  <- sspLNIRT_palette(2)[2]
  line_colour   <- sspLNIRT_palette(2)[1]

  ## Plot
  ggplot2::ggplot(bin_means, ggplot2::aes(x = .data[["mean_sim"]],
                                          y = .data[[Y]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_smooth(method = "loess", span = 0.8, formula = y ~ x,
                         se = FALSE, colour = line_colour, linewidth = 0.6, alpha = .5) +
    ggplot2::geom_point(size = 1.8, alpha = 0.7, colour = point_colour) +
    ggplot2::facet_wrap(
      ~ par,
      scales = "free_x",
      #labeller = ggplot2::labeller(par = ggplot2::label_parsed)
    ) +
    ggplot2::labs(
      x       = "Mean Simulated Value per Bin",
      y       = y_lab,
      caption = paste0("Bins = ", n_bins_actual)
    ) +
    theme_sspLNIRT()
}

#' Plot Estimation Metrics by Parameter Value
#'
#' @description
#' Plots the root mean squared error (RMSE) or bias of estimated parameters as
#' a function of their true (simulated) values, aggregated into bins. This
#' reveals how estimation accuracy varies across the parameter range (e.g.,
#' whether extreme item difficulties are estimated less precisely).
#'
#' Accepts output from [optim_sample()], [get_sspLNIRT()], or [comp_rmse()].
#' For [optim_sample()] output, the error data at the minimum \eqn{N} are
#' used. If the data are already binned (i.e., `keep.err.dat = FALSE` in
#' [comp_rmse()]), bins are plotted as-is; otherwise, raw errors are binned
#' on the fly using `n.bins`. For item parameters, \eqn{\sigma^2} is excluded.
#'
#' This function is preserved for backward compatibility. New code should
#' prefer `plot(object, type = "estimation", ...)`.
#'
#' @param object An object of class `"sspLNIRT"`, as returned by
#'   [optim_sample()], [get_sspLNIRT()], or [comp_rmse()].
#' @param pars Character. `"item"` or `"person"`. Which parameter set to plot.
#' @param y.val Character. `"rmse"` or `"bias"`. Metric for the y-axis.
#' @param n.bins Integer. Number of quantile bins for aggregation. Only used
#'   when the error data are in full (unbinned) format. Default is 30.
#'
#' @return A [ggplot2::ggplot] object, faceted by parameter.
#'
#' @seealso [plot.sspLNIRT()] for the recommended interface;
#'   [plot_power_curve()] for visualizing the optimization trace;
#'   [theme_sspLNIRT()].
#'
#' @examples
#' \dontrun{
#' plot_estimation(result, pars = "item", y.val = "rmse")
#' # equivalent and preferred:
#' plot(result, type = "estimation", pars = "item", y.val = "rmse")
#' }
#'
#' @export
plot_estimation <- function(object, pars = "item", y.val = "rmse", n.bins = 30) {
  plot_estimation_internal(object = object,
                           pars   = pars,
                           y.val  = y.val,
                           n.bins = n.bins)
}

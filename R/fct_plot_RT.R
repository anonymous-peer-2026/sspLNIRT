#' Plot Simulated Response Times
#'
#' @description
#' Simulates data under the Joint Hierarchical Model and plots the resulting
#' response time distribution. The plot type depends on `level`, `logRT` and
#' `overlay`:
#'
#' - **Person, seconds**: Density of mean response time per person (seconds),
#'   trimmed at the 99th percentile, with quantile reference lines.
#' - **Person, log**: Same density on the log-RT scale (untrimmed).
#' - **Item, seconds**: Per-item density of response times (seconds), trimmed
#'   at the 97.5th percentile per item, with the median marked.
#' - **Item, log**: Per-item density on the log-RT scale (untrimmed, fixed
#'   y-axis across items).
#' - **Item, `overlay = TRUE`**: All item densities are drawn as lines in a
#'   single panel instead of a facet grid.
#'
#' Data are generated via [sim.jhm.data()] with `scale = FALSE`. Response
#' times are on the log scale in the generative model; the seconds scale is
#' obtained via exponentiation.
#'
#' @inheritParams plot_RA
#' @param logRT Logical. If `TRUE`, response times are shown on the log
#'   scale; if `FALSE`, on the seconds scale.
#' @param overlay Logical. If `TRUE` and `level = "item"`, all item densities
#'   are drawn in one panel rather than faceted. Ignored when
#'   `level = "person"`. Default is `FALSE`. The item legend is suppressed
#'   for more than 12 items.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [plot_RA()]; [theme_sspLNIRT()].
#'
#' @examples
#' \dontrun{
#' plot_RT(level = "person", logRT = TRUE,  N = 500, K = 10)
#' plot_RT(level = "item",   logRT = TRUE, N = 1000, K = 5,
#'         mu.item = c(1, 0, 0.4, 1), sd.item = c(0.2, 1, 0.2, 0.5))
#'
#' # Many items: overlay instead of faceting
#' plot_RT(level = "item", logRT = TRUE, overlay = TRUE, N = 1000, K = 30)
#'
#' # Pass a design object retrieved from get_sspLNIRT() (or optim_sample()):
#' res <- get_sspLNIRT(thresh = 0.10, out.par = "alpha",
#'                     K = 30, mu.alpha = 1,
#'                     meanlog.sigma2 = log(0.6), rho = 0.4)
#' plot_RT(res$design, level = "item", logRT = FALSE)
#' }
#'
#' @export
plot_RT <- function(design = NULL,
                    level = "item",
                    logRT = FALSE,
                    overlay = FALSE,
                    N = 1e3,
                    K = 30,
                    mu.person = c(0, 0),
                    mu.item = c(1, 0, 0.5, 1),
                    meanlog.sigma2 = log(0.6),
                    cov.m.person = matrix(c(1,   0.4,
                                            0.4, 1), ncol = 2, byrow = TRUE),
                    cov.m.item = matrix(c(1, 0,   0,   0,
                                          0, 1,   0,   0.4,
                                          0, 0,   1,   0,
                                          0, 0.4, 0,   1), ncol = 4, byrow = TRUE),
                    sd.item = c(0.2, 1, 0.2, 0.5),
                    sdlog.sigma2 = 0,
                    item.pars.m = NULL,
                    cor2cov.item = FALSE) {

  ## Resolve design (if supplied). Fields present in `design` are used for
  ## arguments the caller did not pass explicitly; caller-supplied arguments
  ## always take precedence. Extra fields in `design` (e.g. thresh, out.par)
  ## are ignored.
  if (!is.null(design)) {
    if (!is.list(design))
      stop("'design' must be a list or 'sspLNIRT.design' object.")
    explicit <- names(match.call())[-1L]
    design_fields <- c("K", "mu.person", "mu.item", "meanlog.sigma2",
                       "cov.m.person", "cov.m.item", "sd.item",
                       "sdlog.sigma2", "item.pars.m", "cor2cov.item")
    for (f in intersect(design_fields, names(design))) {
      if (!(f %in% explicit))
        assign(f, design[[f]])
    }
  }

  ## Input checks

  level <- match.arg(level, choices = c("person", "item"))

  if (!is.logical(logRT))
    stop("'logRT' must be logical.")
  if (!is.logical(overlay) || length(overlay) != 1L || is.na(overlay))
    stop("'overlay' must be a single logical value.")
  if (!is.numeric(K) || K < 1)
    stop("'K' must be a positive integer.")
  if (length(mu.person) != 2)
    stop("'mu.person' must be length 2.")
  if (length(mu.item) != 4)
    stop("'mu.item' must be length 4.")
  if (!is.matrix(cov.m.person) || !all(dim(cov.m.person) == 2))
    stop("'cov.m.person' must be a 2x2 matrix.")
  if (!is.matrix(cov.m.item) || !all(dim(cov.m.item) == 4))
    stop("'cov.m.item' must be a 4x4 matrix.")
  if (!isSymmetric(cov.m.person))
    stop("'cov.m.person' must be symmetric.")
  if (!isSymmetric(cov.m.item))
    stop("'cov.m.item' must be symmetric.")
  if (cor2cov.item && is.null(sd.item))
    stop("'sd.item' must be provided when 'cor2cov.item = TRUE'.")
  if (!is.null(sd.item) && length(sd.item) != 4)
    stop("'sd.item' must be length 4.")
  if (!is.null(item.pars.m) && (!is.matrix(item.pars.m) || ncol(item.pars.m) != 4))
    stop("'item.pars.m' must be a matrix with 4 columns.")
  if (!is.null(item.pars.m) && nrow(item.pars.m) != K)
    stop("'item.pars.m' must have K rows.")

  ## Simulate data

  data <- sim.jhm.data(iter = 1,
                       N = N, K = K,
                       mu.person = mu.person,
                       mu.item = mu.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       cov.m.person = cov.m.person,
                       cov.m.item = cov.m.item,
                       sdlog.sigma2 = sdlog.sigma2,
                       item.pars.m = item.pars.m,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item,
                       scale = FALSE)

  logRT.data <- as.data.frame(data$time.data)
  RT.data    <- exp(logRT.data)

  fill_col <- sspLNIRT_palette(3)[3]
  outline  <- sspLNIRT_palette(3)[1]

  ## Person-level plots

  if (level == "person") {

    if (logRT) {
      dat   <- data.frame(val = rowMeans(logRT.data))
      lims  <- c(NA_real_, NA_real_)
      x_lab <- "Log Response Time"
    } else {
      dat   <- data.frame(val = rowMeans(RT.data))
      lims  <- c(0, stats::quantile(dat$val, 0.99))
      x_lab <- "Response Time in Seconds"
    }

    probs    <- c(0.01, 0.1, 0.5, 0.9, 0.99)
    quant_df <- data.frame(
      q        = as.numeric(stats::quantile(dat$val, probs)),
      quantile = factor(probs)
    )

    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data[["val"]])) +
        ggplot2::geom_density(fill = fill_col, colour = outline, alpha = 0.6) +
        ggplot2::geom_rug(alpha = 0.1, length = ggplot2::unit(0.03, "npc"),
                          colour = outline) +
        ggplot2::geom_vline(
          data = quant_df,
          ggplot2::aes(xintercept = .data[["q"]],
                       colour     = .data[["quantile"]]),
          linetype = "dashed", linewidth = 0.5
        ) +
        scale_colour_sspLNIRT() +
        ggplot2::coord_cartesian(xlim = lims) +
        ggplot2::labs(x = x_lab, y = "Density", colour = "Quantile") +
        theme_sspLNIRT()
    )
  }

  ## Item-level plots

  mat_sub  <- if (logRT) logRT.data else RT.data
  long_dat <- stats::setNames(utils::stack(mat_sub), c("RT", "item"))
  long_dat$item <- factor(long_dat$item)

  if (logRT) {
    x_lab      <- "Log Response Time"
    axis.scale <- "fixed"
  } else {
    x_lab      <- "Response Time in Seconds"
    axis.scale <- "free_y"
    long_dat <- do.call(rbind, lapply(split(long_dat, long_dat$item), function(d) {
      d[d$RT <= stats::quantile(d$RT, 0.975, na.rm = TRUE), ]
    }))
  }

  ## Overlay
  if (overlay) {

    n_items <- nlevels(droplevels(long_dat$item))

    cols <- if (n_items > 5) {
      sspLNIRT_palette(n_items + 2)[seq_len(n_items)]
    } else {
      sspLNIRT_palette(n_items)
    }

    p <- ggplot2::ggplot(long_dat,
                         ggplot2::aes(x      = .data[["RT"]],
                                      colour = .data[["item"]],
                                      group  = .data[["item"]])) +
      ggplot2::geom_density(fill = NA, linewidth = 0.4, trim = TRUE) +
      ggplot2::scale_colour_manual(values = cols) +
      ggplot2::labs(x = x_lab, y = "Density", colour = "Item") +
      theme_sspLNIRT()

    if (n_items > 12)
      p <- p + ggplot2::theme(legend.position = "none")

    return(p)
  }

  ## Pre-compute medians per item
  medians_df <- do.call(rbind, lapply(split(long_dat, long_dat$item), function(d) {
    data.frame(item = d$item[1],
               median_RT = stats::median(d$RT, na.rm = TRUE))
  }))

  ggplot2::ggplot(long_dat, ggplot2::aes(x = .data[["RT"]])) +
    ggplot2::geom_density(fill = fill_col, colour = outline,
                          alpha = 0.5, trim = TRUE) +
    ggplot2::geom_rug(alpha = 0.1, length = ggplot2::unit(0.03, "npc"),
                      colour = outline) +
    ggplot2::geom_vline(
      data = medians_df,
      ggplot2::aes(xintercept = .data[["median_RT"]]),
      linetype = "dashed", linewidth = 0.4, colour = "grey25"
    ) +
    ggplot2::facet_wrap(~ item, scales = axis.scale) +
    ggplot2::labs(x = x_lab, y = "Density") +
    theme_sspLNIRT()
}

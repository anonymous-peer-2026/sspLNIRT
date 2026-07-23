#' Plot Simulated Response Accuracy
#'
#' @description
#' Simulates data under the Joint Hierarchical Model and plots the resulting
#' response accuracy. The plot type depends on `level`, `by.theta` and
#' `overlay`:
#'
#' - **Person, by distribution**: Histogram of total correct scores across
#'   persons.
#' - **Person, by theta**: Mean total score as a function of ability
#'   \eqn{\theta}, shown as a bar chart binned by \eqn{\theta}.
#' - **Item, by distribution**: Density of \eqn{P(X = 1)} across persons for
#'   each item, with the item-wise median marked.
#' - **Item, by theta**: Item characteristic curves (ICCs), showing
#'   \eqn{P(X = 1)} as a function of \eqn{\theta} for each item, with a
#'   dashed line at the item difficulty \eqn{\beta}.
#' - **Item, `overlay = TRUE`**: All item curves are drawn in a single panel
#'   instead of a facet grid.
#'
#' Data are generated via [sim.jhm.data()] with `scale = FALSE`.
#'
#' @param design Optional list or `"sspLNIRT.design"` object holding the
#'   data-generating design. When supplied, its fields are used for any
#'   design-related argument the caller did not pass explicitly (`K`,
#'   `mu.person`, `mu.item`, `meanlog.sigma2`, `cov.m.person`, `cov.m.item`,
#'   `sd.item`, `sdlog.sigma2`, `item.pars.m`, `cor2cov.item`). Caller-
#'   supplied arguments always take precedence. Extra fields in `design`
#'   (e.g. `thresh`, `out.par`, `seed`) are ignored, so the object returned
#'   in `$design` by [get_sspLNIRT()] or [optim_sample()] can be passed
#'   directly. Default `NULL` (use the function defaults / caller arguments).
#' @param level Character. `"person"` for person-level aggregates or `"item"`
#'   for item-level curves / distributions.
#' @param by.theta Logical. If `TRUE`, the x-axis is \eqn{\theta}; if `FALSE`,
#'   a marginal distribution is shown.
#' @param overlay Logical. If `TRUE` and `level = "item"`, all item curves are
#'   drawn in one panel rather than faceted.
#' @param N Integer. Sample size (number of persons) for the simulated data.
#'   Default is 1000.
#' @param K Integer. Test length (number of items). Default is 10.
#' @param mu.person Numeric vector of length 2. Population means of
#'   \eqn{(\theta, \zeta)}.
#' @param mu.item Numeric vector of length 4. Population means of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}.
#' @param meanlog.sigma2 Numeric. Mean of the log-normal distribution for
#'   \eqn{\sigma^2} (on the log scale).
#' @param cov.m.person 2x2 symmetric matrix. Covariance matrix of
#'   \eqn{(\theta, \zeta)}.
#' @param cov.m.item 4x4 symmetric matrix. Covariance (or correlation) matrix
#'   of \eqn{(\alpha, \beta, \varphi, \lambda)}. See `cor2cov.item`.
#' @param sd.item Numeric vector of length 4. Standard deviations of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}. Required when
#'   `cor2cov.item = TRUE`.
#' @param sdlog.sigma2 Numeric. Standard deviation of the log-normal
#'   distribution for \eqn{\sigma^2}. Default is 0.
#' @param item.pars.m Matrix with 4 columns or `NULL`. If supplied, item
#'   parameters are held fixed instead of drawn from the truncated MVN.
#' @param cor2cov.item Logical. If `TRUE`, `cov.m.item` is treated as a
#'   correlation matrix and converted using `sd.item`.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [plot_RT()] for the response time counterpart;
#'   [theme_sspLNIRT()].
#'
#' @examples
#' \dontrun{
#' plot_RA(level = "person", by.theta = TRUE, N = 500, K = 20)
#' plot_RA(level = "item", by.theta = FALSE, N = 1000, K = 5,
#'         mu.item = c(1, 0, 0.5, 1), sd.item = c(0.2, 0.5, 0.2, 0.5))
#'
#' # Many items: overlay instead of faceting
#' plot_RA(level = "item", by.theta = TRUE, overlay = TRUE, N = 1000, K = 30)
#'
#' # Pass a design object retrieved from get_sspLNIRT() (or optim_sample()):
#' res <- get_sspLNIRT(thresh = 0.10, out.par = "alpha",
#'                     K = 30, mu.alpha = 1,
#'                     meanlog.sigma2 = log(0.6), rho = 0.4)
#' plot_RA(res$design, level = "item", by.theta = TRUE)
#' }
#'
#' @export
plot_RA <- function(design = NULL,
                    level = "item",
                    by.theta = TRUE,
                    overlay = FALSE,
                    N = 1e3,
                    K = 30,
                    mu.person = c(0, 0),
                    mu.item = c(1, 0, 0.5, 1),
                    meanlog.sigma2 = log(0.6),
                    cov.m.person = matrix(c(1,   0.4,
                                            0.4, 1), ncol = 2, byrow = FALSE),
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

  if (!is.logical(by.theta))
    stop("'by.theta' must be logical.")
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

  RA.data     <- as.data.frame(data$response.data[[1]])
  item.data   <- as.data.frame(data$item.par[[1]])
  person.data <- as.data.frame(data$person.par[[1]])

  fill_col   <- sspLNIRT_palette(3)[3]
  outline    <- sspLNIRT_palette(3)[1]
  line_col   <- sspLNIRT_palette(2)[1]

  ## Person-level plots
  if (level == "person") {

    if (!by.theta) {

      dat <- data.frame(RA = as.integer(rowSums(RA.data)))

      return(
        ggplot2::ggplot(dat, ggplot2::aes(x = .data[["RA"]])) +
          ggplot2::geom_histogram(
            binwidth = 1, boundary = -0.5, closed = "left",
            fill = fill_col, colour = outline, alpha = 0.7
          ) +
          ggplot2::scale_x_continuous(breaks = 0:K, limits = c(-0.5, K + 0.5)) +
          ggplot2::labs(x = "Total Correct Score", y = "Count") +
          theme_sspLNIRT()
      )
    }

    dat <- data.frame(
      theta = person.data$theta,
      RA    = rowSums(RA.data)
    )

    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data[["theta"]],
                                        y = .data[["RA"]])) +
        ggplot2::stat_summary_bin(fun = mean, bins = K,
                                  geom = "col", fill = fill_col,
                                  colour = outline, alpha = 0.7) +
        ggplot2::labs(x = expression(theta), y = "Total Correct Score") +
        theme_sspLNIRT()
    )
  }

  ## Item-level plots
  theta <- person.data$theta
  alpha <- item.data$alpha
  beta  <- item.data$beta

  eta      <- sweep(outer(theta, beta, FUN = "-"), 2, alpha, FUN = "*")
  prob_mat <- stats::pnorm(eta)

  prob_long <- data.frame(
    prob  = as.vector(prob_mat),
    item  = factor(paste0("Item", rep(seq_len(length(alpha)), each = length(theta))),
                   levels = paste0("Item", seq_len(K))),
    theta = rep(theta, K)
  )

  obs_long <- data.frame(
    response = as.vector(as.matrix(RA.data)),
    item     = factor(paste0("Item", rep(seq_len(K), each = N)),
                      levels = paste0("Item", seq_len(K))),
    theta    = rep(person.data$theta, K)
  )

  ## Colour ramp used when all items share a single panel
  if (overlay) {
    n_items      <- nlevels(droplevels(prob_long$item))
    overlay_cols <- if (n_items > 5) {
      sspLNIRT_palette(n_items + 2)[seq_len(n_items)]
    } else {
      sspLNIRT_palette(n_items)
    }
  }

  if (!by.theta) {

    ## Overlay: all item densities in a single panel
    if (overlay) {

      p <- ggplot2::ggplot(prob_long,
                           ggplot2::aes(x      = .data[["prob"]],
                                        colour = .data[["item"]],
                                        group  = .data[["item"]])) +
        ggplot2::geom_density(fill = NA, linewidth = 0.4, trim = TRUE) +
        ggplot2::scale_colour_manual(values = overlay_cols) +
        ggplot2::labs(x = expression(P(X == 1)), y = "Density",
                      colour = "Item") +
        theme_sspLNIRT()

      if (n_items > 12)
        p <- p + ggplot2::theme(legend.position = "none")

      return(p)
    }

    medians_df <- do.call(rbind, lapply(split(prob_long, prob_long$item), function(d) {
      data.frame(item = d$item[1],
                 median_prob = stats::median(d$prob, na.rm = TRUE))
    }))

    return(
      ggplot2::ggplot(prob_long, ggplot2::aes(x = .data[["prob"]])) +
        ggplot2::geom_density(fill = fill_col, colour = outline,
                              alpha = 0.5, trim = TRUE) +
        ggplot2::geom_rug(alpha = 0.1, length = ggplot2::unit(0.03, "npc"),
                          colour = outline) +
        ggplot2::geom_vline(
          data = medians_df,
          ggplot2::aes(xintercept = .data[["median_prob"]]),
          linetype = "dashed", linewidth = 0.4, colour = "grey25"
        ) +
        ggplot2::facet_wrap(~ item, scales = "free_y") +
        ggplot2::labs(x = expression(P(X == 1)), y = "Density") +
        theme_sspLNIRT()
    )
  }

  ## ICCs
  prob_long <- prob_long[order(prob_long$item, prob_long$theta), ]

  ## Overlay: all ICCs in a single panel
  if (overlay) {

    p <- ggplot2::ggplot(prob_long,
                         ggplot2::aes(x      = .data[["theta"]],
                                      y      = .data[["prob"]],
                                      colour = .data[["item"]],
                                      group  = .data[["item"]])) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::scale_colour_manual(values = overlay_cols) +
      ggplot2::labs(x = expression(theta), y = expression(P(X == 1)),
                    colour = "Item") +
      theme_sspLNIRT()

    if (n_items > 12)
      p <- p + ggplot2::theme(legend.position = "none")

    return(p)
  }

  beta_df <- data.frame(
    item     = factor(paste0("Item", seq_len(length(beta))),
                      levels = paste0("Item", seq_len(K))),
    beta_val = beta
  )

  ggplot2::ggplot(prob_long, ggplot2::aes(x = .data[["theta"]],
                                          y = .data[["prob"]])) +
    ggplot2::geom_jitter(
      data   = obs_long,
      ggplot2::aes(x = .data[["theta"]], y = .data[["response"]]),
      height = 0.03, width = 0,
      size   = 0.15, alpha = .5, colour = outline,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(colour = line_col, linewidth = 0.5) +
    ggplot2::geom_vline(
      data = beta_df,
      ggplot2::aes(xintercept = .data[["beta_val"]]),
      linetype = "dashed", linewidth = 0.4, colour = "grey25"
    ) +
    ggplot2::facet_wrap(~ item, scales = "fixed") +
    ggplot2::labs(x = expression(theta), y = expression(P(X == 1))) +
    theme_sspLNIRT()
}

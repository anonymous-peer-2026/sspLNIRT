#' Retrieve Precomputed Sample Size Results
#'
#' @description
#' Looks up precomputed [optim_sample()] results from the internal dataset
#' [sspLNIRT.data]. For each target parameter / threshold pair, the function
#' finds the matching configuration and returns the result whose minimum
#' \eqn{N} is the bottleneck (i.e., the largest required sample size across
#' all requested parameters).
#'
#' When multiple target parameters are supplied, each is matched independently
#' against the precomputed grid. The returned result corresponds to whichever
#' single-parameter configuration demanded the largest \eqn{N}, since that
#' \eqn{N} guarantees all other parameters also meet their thresholds
#' (RMSE is monotonically decreasing in \eqn{N}).
#'
#' @param thresh Numeric vector. Target RMSE threshold(s). Same length as
#'   `out.par`.
#' @param out.par Character vector. Item parameter(s): each one of `"alpha"`,
#'   `"beta"`, `"phi"`, `"lambda"`.
#' @param K Integer. Test length.
#' @param mu.alpha Numeric. Population mean of the discrimination parameter.
#' @param meanlog.sigma2 Numeric. Mean of the log-normal distribution for
#'   \eqn{\sigma^2} (on the log scale).
#' @param rho Numeric in \eqn{[-1, 1]}. Correlation between \eqn{\theta} and
#'   \eqn{\zeta}.
#'
#' @return A list with components:
#' \describe{
#'   \item{`object`}{An object of class `"sspLNIRT"` as returned by
#'     [optim_sample()].}
#'   \item{`design`}{A list with the full set of parameter values used for
#'     the precomputation.}
#' }
#'
#' @details
#' Searches [sspLNIRT.data] row by row using approximate matching
#' (`tol = 1e-3`) for numeric parameters. If no exact match is found, the
#' error message reports which parameters differed in the closest available
#' configuration.
#'
#' ## Bottleneck selection
#'
#' After matching each `out.par[j]` / `thresh[j]` pair independently, the
#' bottleneck result is selected by:
#' 1. If any `N.min == "res.ub > thresh"`, that result is returned.
#' 2. Otherwise, the result with the largest numeric `N.min` is returned.
#' 3. Otherwise (all `N.min == "res.lb < thresh"`), the first such result
#'    is returned.
#'
#' @seealso [optim_sample()]; [sspLNIRT.data]; [available_configs()].
#'
#' @examples
#' \dontrun{
#' result <- get_sspLNIRT(
#'   thresh         = 0.10,
#'   out.par        = "alpha",
#'   K              = 30,
#'   mu.alpha       = 1,
#'   meanlog.sigma2 = log(0.6),
#'   rho            = 0.2
#' )
#' summary(result$object)
#' }
#'
#' @export
get_sspLNIRT <- function(thresh,
                         out.par,
                         K,
                         mu.alpha,
                         meanlog.sigma2,
                         rho) {

  ## Input checks
  if (!is.numeric(thresh) || any(thresh <= 0))
    stop("'thresh' must be positive.")
  if (!is.character(out.par))
    stop("'out.par' must be a character vector.")
  if (length(thresh) != length(out.par))
    stop("'thresh' and 'out.par' must have the same length.")
  if (!all(out.par %in% c("alpha", "beta", "phi", "lambda")))
    stop("'out.par' must be a subset of: alpha, beta, phi, lambda.")
  if (!is.numeric(K) || length(K) != 1 || K < 1)
    stop("'K' must be a single positive integer.")
  if (!is.numeric(mu.alpha) || length(mu.alpha) != 1)
    stop("'mu.alpha' must be a single number.")
  if (!is.numeric(meanlog.sigma2) || length(meanlog.sigma2) != 1)
    stop("'meanlog.sigma2' must be a single number.")
  if (!is.numeric(rho) || length(rho) != 1 || abs(rho) > 1)
    stop("'rho' must be a single number in [-1, 1].")

  ## Helper: approximate numeric comparison
  values_match <- function(a, b, tol = 1e-3) {
    if (is.null(a) || is.null(b)) return(FALSE)
    abs(as.numeric(a) - as.numeric(b)) < tol
  }

  ## Look up each out.par / thresh pair
  matched <- vector("list", length(out.par))

  for (j in seq_along(out.par)) {

    match_idx  <- NULL
    mismatches <- list()

    for (i in seq_len(nrow(sspLNIRT.data))) {
      cfg <- sspLNIRT.data$cfg[[i]]

      all_match      <- TRUE
      row_mismatches <- character()

      if (!values_match(cfg$thresh, thresh[j])) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("thresh: requested %s, available %s",
                                    thresh[j], cfg$thresh))
      }
      if (!identical(cfg$out.par, out.par[j])) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("out.par: requested '%s', available '%s'",
                                    out.par[j], cfg$out.par))
      }
      if (!values_match(cfg$K, K)) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("K: requested %s, available %s",
                                    K, cfg$K))
      }
      if (!values_match(cfg$mu.item[1], mu.alpha)) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("mu.alpha: requested %s, available %s",
                                    mu.alpha, cfg$mu.item[1]))
      }
      if (!values_match(cfg$meanlog.sigma2, meanlog.sigma2)) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("meanlog.sigma2: requested %s, available %s",
                                    meanlog.sigma2, cfg$meanlog.sigma2))
      }
      if (!values_match(cfg$cov.m.person[1, 2], rho)) {
        all_match <- FALSE
        row_mismatches <- c(row_mismatches,
                            sprintf("rho: requested %s, available %s",
                                    rho, cfg$cov.m.person[1, 2]))
      }

      if (all_match) {
        match_idx <- i
        break
      } else {
        mismatches[[i]] <- row_mismatches
      }
    }

    if (!is.null(match_idx)) {
      res <- sspLNIRT.data$res[[match_idx]]
      cfg <- sspLNIRT.data$cfg[[match_idx]]
      class(res) <- unique(c("sspLNIRT", class(res)))
      matched[[j]] <- list(object = res, design = cfg)
    } else {
      n_mismatches <- vapply(mismatches, length, integer(1))
      best_row     <- which.min(n_mismatches)
      stop(paste0(
        "No matching configuration found for out.par = '", out.par[j],
        "', thresh = ", thresh[j], ".\n\n",
        "Closest match (row ", best_row, ") differs in ",
        n_mismatches[best_row], " parameter(s):\n",
        paste("  ", mismatches[[best_row]], collapse = "\n"),
        "\n\nUse `available_configs()` to see all available configurations."
      ))
    }
  }

  ## Select critical parameter result
  ub_idx   <- integer()
  lb_idx   <- integer()
  num_idx  <- integer()
  num_vals <- numeric()

  for (j in seq_along(matched)) {
    nm <- matched[[j]]$object$N.min
    if (identical(nm, "res.ub > thresh")) {
      ub_idx <- c(ub_idx, j)
    } else if (identical(nm, "res.lb < thresh")) {
      lb_idx <- c(lb_idx, j)
    } else {
      num_idx  <- c(num_idx, j)
      num_vals <- c(num_vals, nm)
    }
  }

  if (length(ub_idx) > 0)  return(matched[[ub_idx[1]]])
  if (length(num_idx) > 0) return(matched[[num_idx[which.max(num_vals)]]])
  matched[[lb_idx[1]]]
}

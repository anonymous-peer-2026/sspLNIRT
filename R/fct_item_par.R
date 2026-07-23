#' Generate Item Parameters
#'
#' @description
#' Generates a data frame of item parameters for `K` items. The four main
#' parameters \eqn{(\alpha, \beta, \varphi, \lambda)} are drawn from a
#' truncated multivariate normal distribution (via [tmvtnorm::rtmvnorm()])
#' with \eqn{\alpha > 0} and \eqn{\varphi > 0}. The residual variance
#' \eqn{\sigma^2} is drawn independently from a log-normal distribution.
#'
#' When `cor2cov.item = TRUE`, the supplied `cov.m.item` is treated as a
#' correlation matrix and converted to a covariance matrix using
#' \eqn{\Sigma = D \, R \, D}, where \eqn{D = \mathrm{diag}(\texttt{sd.item})}.
#'
#' @param K Integer. Number of items to generate.
#' @param mu.item Numeric vector of length 4. Means of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}.
#' @param cov.m.item 4x4 symmetric matrix. Covariance (or correlation) matrix
#'   of \eqn{(\alpha, \beta, \varphi, \lambda)}. See `cor2cov.item`.
#' @param meanlog.sigma2 Numeric. Mean of the log-normal distribution for
#'   \eqn{\sigma^2} (on the log scale).
#' @param sdlog.sigma2 Numeric. Standard deviation of the log-normal
#'   distribution for \eqn{\sigma^2}.
#' @param cor2cov.item Logical. If `TRUE`, `cov.m.item` is treated as a
#'   correlation matrix and converted using `sd.item`.
#' @param sd.item Numeric vector of length 4 or `NULL`. Standard deviations of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}. Required when
#'   `cor2cov.item = TRUE`.
#'
#' @return A data frame with `K` rows and 5 columns:
#' \describe{
#'   \item{`alpha`}{Item discrimination (\eqn{> 0}, truncated).}
#'   \item{`beta`}{Item difficulty (unbounded).}
#'   \item{`phi`}{Time discrimination (\eqn{> 0}, truncated).}
#'   \item{`lambda`}{Time intensity (unbounded).}
#'   \item{`sigma2`}{Residual variance (log-normal).}
#' }
#'
#' @seealso [sim.jhm.data()] which calls this function to generate item
#'   parameters for each replication.
#'
#' @noRd
item.par <- function(K,
                     mu.item,
                     cov.m.item,
                     meanlog.sigma2,
                     sdlog.sigma2,
                     cor2cov.item = FALSE,
                     sd.item = NULL) {


  sigma2 <- rlnorm(K, meanlog = meanlog.sigma2, sdlog = sdlog.sigma2)
  if (cor2cov.item) {
    D <- diag(sd.item)
    cov.m.item <- D %*% cov.m.item %*% D
  }

  item.pars <- tmvtnorm::rtmvnorm(
    n     = K,
    mean  = mu.item,
    sigma = cov.m.item,
    lower = c(0,    -Inf, 0,    -Inf),
    upper = c(Inf,   Inf, Inf,   Inf)
  )
  colnames(item.pars) <- c("alpha", "beta", "phi", "lambda")

  pars.out <- as.data.frame(cbind(item.pars, sigma2))
  return(pars.out)
}

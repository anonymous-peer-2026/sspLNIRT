#' List Available Precomputed Configurations
#'
#' @description
#' Returns a data frame summarizing all parameter combinations for which
#' precomputed [optim_sample()] results are available in [sspLNIRT.data].
#' Use this to check which values of `thresh`, `out.par`, `K`, `mu.alpha`,
#' `meanlog.sigma2`, and `rho` can be passed to [get_sspLNIRT()].
#'
#' @return A data frame with one row per precomputed configuration and the
#'   following columns:
#' \describe{
#'   \item{`thresh`}{RMSE threshold.}
#'   \item{`out.par`}{Target item parameter.}
#'   \item{`K`}{Test length.}
#'   \item{`mu.alpha`}{Mean of \eqn{\alpha} (item discrimination).}
#'   \item{`meanlog.sigma2`}{Mean of \eqn{\log(\sigma^2)}.}
#'   \item{`rho`}{Correlation between \eqn{\theta} and \eqn{\zeta}.}
#' }
#'
#' @seealso [get_sspLNIRT()] which looks up results by these parameters;
#'   [sspLNIRT.data] for the underlying dataset.
#'
#' @examples
#' configs <- available_configs()
#' head(configs)
#'
#' # unique test lengths
#' unique(configs$K)
#'
#' @export
available_configs <- function() {

  raw_meanlog <- sapply(sspLNIRT.data$cfg, `[[`, "meanlog.sigma2")

  meanlog_label <- paste0("log(", round(exp(raw_meanlog), 1), ")")

  data.frame(
    thresh         = sapply(sspLNIRT.data$cfg, `[[`, "thresh"),
    out.par        = sapply(sspLNIRT.data$cfg, `[[`, "out.par"),
    K              = sapply(sspLNIRT.data$cfg, `[[`, "K"),
    mu.alpha       = sapply(sspLNIRT.data$cfg, function(x) x$mu.item[1]),
    meanlog.sigma2 = meanlog_label,
    rho            = sapply(sspLNIRT.data$cfg, function(x) x$cov.m.person[1, 2])
  )
}

#' Generate Person Parameters
#'
#' @description
#' Generates a data frame of person parameters for `N` persons. The ability
#' parameter \eqn{\theta} and speed parameter \eqn{\zeta} are drawn jointly
#' from a bivariate normal distribution via [MASS::mvrnorm()].
#'
#' @param N Integer. Number of persons to generate.
#' @param mu.person Numeric vector of length 2. Population means of
#'   \eqn{(\theta, \zeta)}.
#' @param cov.m.person 2x2 symmetric matrix. Covariance matrix of
#'   \eqn{(\theta, \zeta)}.
#'
#' @return A data frame with `N` rows and 2 columns:
#' \describe{
#'   \item{`theta`}{Latent ability.}
#'   \item{`zeta`}{Latent speed.}
#' }
#'
#' @seealso [sim.jhm.data()] which calls this function to generate person
#'   parameters for each replication.
#'
#' @noRd
person.par <- function(N,
                       mu.person,
                       cov.m.person) {

  hyper.par <- as.data.frame(
    MASS::mvrnorm(N,
                  mu    = mu.person,
                  Sigma = cov.m.person)
  )
  colnames(hyper.par) <- c("theta", "zeta")

  return(hyper.par)
}

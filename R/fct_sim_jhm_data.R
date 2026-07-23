#' Simulate Data under the Joint Hierarchical Model
#'
#' @description
#' Generates simulated response accuracy and response time data under the
#' Joint Hierarchical Model (JHM). Response accuracy follows a two-parameter
#' normal ogive model; response times follow a log-normal model with item and
#' person parameters.
#'
#' Person parameters \eqn{(\theta, \zeta)} are drawn from a bivariate normal
#' distribution. Item parameters \eqn{(\alpha, \beta, \varphi, \lambda)} are
#' drawn from a truncated multivariate normal distribution (via [item.par()]),
#' and residual variances \eqn{\sigma^2} from a log-normal distribution.
#' Optionally, item parameters can be held fixed across replications via
#' `item.pars.m`.
#'
#' @param iter Integer. Number of independent data sets to generate.
#' @param N Integer. Sample size (number of persons) per data set.
#' @param K Integer. Test length (number of items) per data set.
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
#' @param sdlog.sigma2 Numeric. Standard deviation of the log-normal
#'   distribution for \eqn{\sigma^2}.
#' @param item.pars.m Matrix with 4 columns or `NULL`. If supplied, these item
#'   parameters are used for every replication instead of drawing new ones.
#' @param cor2cov.item Logical. If `TRUE`, `cov.m.item` is treated as a
#'   correlation matrix and converted using `sd.item`.
#' @param sd.item Numeric vector of length 4 or `NULL`. Standard deviations of
#'   item parameters. Required when `cor2cov.item = TRUE`.
#' @param scale Logical. If `TRUE` (default), item and person parameters are
#'   rescaled via [scale_M()] to the LNIRT identification constraints.
#'
#' @return A list with the following elements, each a list of length `iter`:
#' \describe{
#'   \item{`time.data`}{List of \eqn{N \times K} matrices. Simulated
#'     log-response times.}
#'   \item{`response.data`}{List of \eqn{N \times K} matrices. Simulated
#'     binary response accuracy (1 = correct, 0 = incorrect).}
#'   \item{`person.par`}{List of data frames with columns `theta` and `zeta`.}
#'   \item{`item.par`}{List of data frames with columns `alpha`, `beta`,
#'     `phi`, `lambda`, and `sigma2`.}
#'   \item{`scale.factor`}{List of data frames with columns `c.alpha` and
#'     `c.phi` (scaling constants; `NA` when `scale = FALSE`).}
#' }
#'
#' @seealso [person.par()] and [item.par()] for the parameter-generating
#'   functions; [scale_M()] for the rescaling step; [comp_rmse()] which calls
#'   this function at each Monte Carlo replication.
#'
#' @noRd
sim.jhm.data <- function(iter,
                         N,
                         K,
                         mu.person,
                         mu.item,
                         meanlog.sigma2,
                         cov.m.person,
                         cov.m.item,
                         sdlog.sigma2,
                         item.pars.m,
                         cor2cov.item,
                         sd.item,
                         scale = TRUE) {

  sim.time     <- vector("list", iter)
  sim.response <- vector("list", iter)
  person.par   <- vector("list", iter)
  item.par     <- vector("list", iter)
  scale.factor <- vector("list", iter)

  for (k in seq_len(iter)) {

    person <- person.par(N = N,
                         cov.m.person = cov.m.person,
                         mu.person = mu.person)

    if (is.null(item.pars.m)) {
      item <- item.par(K = K,
                       mu.item = mu.item,
                       cov.m.item = cov.m.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       sdlog.sigma2 = sdlog.sigma2,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item)
    } else {
      item <- as.data.frame(item.pars.m)
    }

    # rescale to identification constraints
    if (scale) {
      scaled.pars <- scale_M(item.pars = item,
                             person.pars = person)
      item    <- scaled.pars$items.pars.scaled
      person  <- scaled.pars$person.pars.scaled
      c.alpha <- scaled.pars$c.alpha
      c.phi   <- scaled.pars$c.phi
    } else {
      c.alpha <- NA
      c.phi   <- NA
    }

    time     <- matrix(nrow = N, ncol = K)
    response <- matrix(nrow = N, ncol = K)
    colnames(time) <- colnames(response) <- paste0("Item", seq_len(K))

    # 2-parameter normal ogive
    for (i in seq_len(K)) {
      response[, i] <- rbinom(N, 1,
                              prob = pnorm(item$alpha[i] * (person$theta - item$beta[i])))
    }

    # 3-parameter log-normal
    for (r in seq_len(K)) {
      time[, r] <- item$lambda[r] - item$phi[r] * person$zeta +
        rnorm(N, 0, sqrt(item$sigma2[r]))
    }

    sim.time[[k]]     <- time
    sim.response[[k]] <- response
    person.par[[k]]   <- person
    item.par[[k]]     <- item
    scale.factor[[k]] <- data.frame(c.alpha = c.alpha,
                                    c.phi   = c.phi)
  }

  return(list(
    time.data     = sim.time,
    response.data = sim.response,
    person.par    = person.par,
    item.par      = item.par,
    scale.factor  = scale.factor
  ))
}

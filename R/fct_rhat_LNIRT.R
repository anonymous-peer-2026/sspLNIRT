#' Rhat Convergence Diagnostics for LNIRT Objects
#'
#' @description
#' Computes \eqn{\hat{R}} convergence diagnostics (Vehtari et al., 2021) for
#' all model parameters from a list of fitted [LNIRT::LNIRT()] objects. Each
#' object is treated as one MCMC chain; the chains are combined and
#' \eqn{\hat{R}} is computed via [posterior::rhat()].
#'
#' Parameters are grouped into three blocks by dimensionality:
#' - **D1**: Scalar hyper-parameters (population means, variances, covariance).
#' - **D2**: Per-item and per-person parameters (discrimination, difficulty,
#'   time parameters, ability, speed, \eqn{\sigma^2}).
#' - **D3**: Item covariance matrix elements.
#'
#' @param object.list List of fitted LNIRT objects. Must contain at least
#'   `chains` elements.
#' @param chains Integer. Number of chains to use. Default is 4.
#' @param cutoff Numeric or `NULL`. \eqn{\hat{R}} threshold for convergence.
#'   If `NULL`, convergence counts and rates are not computed (only
#'   \eqn{\hat{R}} values are returned).
#'
#' @return A list containing:
#' \describe{
#'   \item{`value`}{List with \eqn{\hat{R}} values by block (`D1`, `D2.item`,
#'     `D2.person`, `D3`).}
#'   \item{`convergence`}{List with binary indicators (1 if \eqn{\hat{R}} <
#'     `cutoff`, else 0) by block. `NULL` when `cutoff = NULL`.}
#'   \item{`rate`}{List with mean convergence rate (proportion below `cutoff`)
#'     by block. `NULL` when `cutoff = NULL`.}
#' }
#'
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2021).
#' Rank-normalization, folding, and localization: An improved R ̂ for assessing
#' convergence of MCMC (with discussion). Bayesian analysis, 16(2), 667-718.
#' DOI: 10.1214/20-BA1221
#'
#' @noRd
rhat_LNIRT <- function(object.list,
                       chains = 4,
                       cutoff = 1.05) {

  stopifnot(length(object.list) >= 2)

  XG   <- object.list[[1]]$XG
  burn <- ceiling(object.list[[1]]$burnin / 100 * XG)

  D1 <- c(
    "Mu.Person.Ability",
    "Mu.Person.Speed",
    "Var.Person.Ability",
    "Var.Person.Speed",
    "Cov.Person.Ability.Speed",
    "Mu.Item.Discrimination",
    "Mu.Item.Difficulty",
    "Mu.Time.Discrimination",
    "Mu.Time.Intensity"
  )

  D2.item <- c(
    "Item.Discrimination",
    "Item.Difficulty",
    "Time.Discrimination",
    "Time.Intensity",
    "Sigma2"
  )

  D2.person <- c(
    "Person.Ability",
    "Person.Speed"
  )

  D3 <- c(
    "CovMat.Item"
  )

  mcmc.samples <- lapply(object.list, FUN = function(x) {
    x$MCMC.Samples
  })

  ## D1: scalar hyper-parameters
  D1.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D1]
  }))

  D1.mcmc <- lapply(D1.chains, function(x) {
    m <- do.call(cbind, x)
    m[burn:XG, ]
  })

  D1.r.hat <- lapply(D1.mcmc, posterior::rhat)

  ## D2: per-item parameters
  D2.item.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.item]
  }))

  D2.item.mcmc <- lapply(D2.item.chains, function(x) {
    do.call(rbind, x)
  })

  D2.item.r.hat <- lapply(D2.item.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })

  ## D2: per-person parameters
  D2.person.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.person]
  }))

  D2.person.mcmc <- lapply(D2.person.chains, function(x) {
    do.call(rbind, x)
  })

  D2.person.r.hat <- lapply(D2.person.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })

  ## D3: item covariance matrix
  D3.chains <- lapply(mcmc.samples, FUN = function(x) {
    x[D3]$CovMat.Item
  })

  D3.mcmc <- lapply(D3.chains, FUN = function(x) {
    temp <- x[, , 1]
    for (i in 2:4) {
      temp <- cbind(temp, x[, , i])
    }
    temp
  })

  D3.r.hat <- lapply(
    apply(do.call(rbind, D3.mcmc), 2, FUN = function(y) {
      matrix(y, ncol = chains)[burn:XG, ]
    }, simplify = FALSE),
    FUN = function(x) {
      posterior::rhat(x)
    }
  )

  ## Rhat values
  value <- list(
    D1        = D1.r.hat,
    D2.item   = unlist(D2.item.r.hat),
    D2.person = unlist(D2.person.r.hat),
    D3        = unlist(D3.r.hat)
  )

  ## Convergence counts and rates when cutoff is provided
  if (!is.null(cutoff)) {
    convergence <- list(
      D1        = ifelse(unlist(D1.r.hat) < cutoff, 1, 0),
      D2.item   = ifelse(value$D2.item < cutoff, 1, 0),
      D2.person = ifelse(value$D2.person < cutoff, 1, 0),
      D3        = ifelse(value$D3 < cutoff, 1, 0)
    )
    rate <- list(
      D1        = mean(convergence$D1),
      D2.item   = mean(convergence$D2.item),
      D2.person = mean(convergence$D2.person),
      D3        = mean(convergence$D3)
    )
  } else {
    convergence <- NULL
    rate        <- NULL
  }

  return(list(
    value       = value,
    convergence = convergence,
    rate        = rate
  ))
}

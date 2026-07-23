#' Scale Item and Person Parameters to LNIRT Identification Constraints
#'
#' @description
#' Rescales item and person parameter matrices so that the geometric means of
#' \eqn{\alpha} (discrimination) and \eqn{\varphi} (time discrimination) equal
#' 1, which is the identification constraint used by the [LNIRT][LNIRT::LNIRT]
#' package.
#'
#' When `re.scale = FALSE` (forward scaling), the function computes the
#' geometric means \eqn{c_\alpha} and \eqn{c_\varphi} from the supplied item
#' parameters and divides/multiplies accordingly:
#'
#' \deqn{\alpha^* = \alpha / c_\alpha, \quad \beta^* = \beta \cdot c_\alpha,
#' \quad \varphi^* = \varphi / c_\varphi}
#' \deqn{\theta^* = \theta \cdot c_\alpha, \quad \zeta^* = \zeta \cdot
#' c_\varphi}
#'
#' When `re.scale = TRUE` (inverse scaling), the supplied constants
#' `c.alpha` and `c.phi` are used to reverse the transformation back to the
#' original (generating) scale.
#'
#' @param item.pars Data frame with columns `alpha`, `beta`, `phi`, `lambda`,
#'   and `sigma2` (in that order). One row per item.
#' @param person.pars Data frame with columns `theta` and `zeta`. One row per
#'   person.
#' @param re.scale Logical. If `FALSE` (default), performs forward scaling
#'   (constraining geometric means to 1). If `TRUE`, inverts the scaling using
#'   `c.alpha` and `c.phi`.
#' @param c.alpha Numeric or `NULL`. Scaling constant for \eqn{\alpha}.
#'   Required when `re.scale = TRUE`; ignored when `re.scale = FALSE`.
#' @param c.phi Numeric or `NULL`. Scaling constant for \eqn{\varphi}.
#'   Required when `re.scale = TRUE`; ignored when `re.scale = FALSE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{`items.pars.scaled`}{Data frame. Scaled item parameters (same
#'     columns as `item.pars`).}
#'   \item{`person.pars.scaled`}{Data frame. Scaled person parameters (same
#'     columns as `person.pars`).}
#'   \item{`c.alpha`}{Numeric. The scaling constant for \eqn{\alpha}
#'     (geometric mean of the original \eqn{\alpha} values).}
#'   \item{`c.phi`}{Numeric. The scaling constant for \eqn{\varphi}
#'     (geometric mean of the original \eqn{\varphi} values).}
#' }
#'
#' @seealso [sim.jhm.data()] which calls this during data generation;
#'   [comp_rmse()] which calls this to map posterior estimates back to the
#'   generating scale.
#'
#' @noRd
scale_M <- function(item.pars,
                    person.pars,
                    re.scale = FALSE,
                    c.alpha = NULL,
                    c.phi = NULL) {

  I <- nrow(item.pars)
  N <- nrow(person.pars)

  if (re.scale) {

    ## Inverse scaling
    c.items   <- matrix(rep(c(c.alpha, 1 / c.alpha, c.phi, 1, 1), I),
                        ncol = 5, byrow = TRUE)
    c.persons <- matrix(rep(c(1 / c.alpha, 1 / c.phi), N),
                        ncol = 2, byrow = TRUE)

  } else {

    ## Forward scaling
    c.alpha <- prod(item.pars$alpha)^(1 / I)
    c.phi   <- prod(item.pars$phi)^(1 / I)

    c.items   <- matrix(rep(c(1 / c.alpha, c.alpha, 1 / c.phi, 1, 1), I),
                        ncol = 5, byrow = TRUE)
    c.persons <- matrix(rep(c(c.alpha, c.phi), N),
                        ncol = 2, byrow = TRUE)
  }

  ## Apply element-wise
  items.pars.scaled  <- as.data.frame(item.pars * c.items)
  person.pars.scaled <- as.data.frame(person.pars * c.persons)

  return(list(
    items.pars.scaled  = items.pars.scaled,
    person.pars.scaled = person.pars.scaled,
    c.alpha            = c.alpha,
    c.phi              = c.phi
  ))
}

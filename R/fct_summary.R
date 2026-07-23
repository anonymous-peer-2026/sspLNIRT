#' Summarise an sspLNIRT Object
#'
#' @description
#' Provides a structured summary of a `sspLNIRT` object. The method
#' auto-detects whether the object was produced by [optim_sample()] (contains
#' `N.min`) or by [comp_rmse()] (contains `person` and `item`), and returns
#' the relevant fields.
#'
#' @param object An object of class `"sspLNIRT"`, as returned by
#'   [optim_sample()] or [comp_rmse()].
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `"summary.sspLNIRT"`.
#'
#'   For [optim_sample()] output, the summary contains:
#'   \describe{
#'     \item{`N.min`}{Integer or character. Minimum sample size (or boundary
#'       message).}
#'     \item{`out.par`}{Character vector. Target parameter(s) tracked by the
#'       optimization, recovered from `track.res` columns.}
#'     \item{`res.best`}{Named numeric vector. RMSE at the optimal \eqn{N}.}
#'     \item{`comp.rmse`}{List. Full [comp_rmse()] output at the optimal
#'       \eqn{N}.}
#'     \item{`trace`}{List. Optimization diagnostics (steps, tracked results
#'       and \eqn{N} values, wall-clock time).}
#'   }
#'
#'   For [comp_rmse()] output, the summary contains:
#'   \describe{
#'     \item{`person`}{List with `rmse`, `mc.sd.rmse`, and `bias` for
#'       \eqn{\theta} and \eqn{\zeta}.}
#'     \item{`item`}{List with `rmse`, `mc.sd.rmse`, and `bias` for item
#'       parameters.}
#'     \item{`rhat.dat`}{Matrix of \eqn{\hat{R}} values or `NULL`.}
#'   }
#'
#' @seealso [optim_sample()], [comp_rmse()], [print.summary.sspLNIRT()].
#'
#' @method summary sspLNIRT
#' @exportS3Method summary sspLNIRT
summary.sspLNIRT <- function(object, ...) {

  if (!inherits(object, "sspLNIRT"))
    stop("Input must be an sspLNIRT object.")

  if (!is.null(object$N.min)) {

    par_cols <- grep("^res\\.temp\\.",
                     names(object$trace$track.res),
                     value = TRUE)
    out_pars <- sub("^res\\.temp\\.", "", par_cols)

    summary_obj <- list(
      N.min     = object$N.min,
      out.par   = out_pars,
      res.best  = object$res.best,
      comp.rmse = object$comp.rmse,
      trace     = object$trace
    )

  } else {

    summary_obj <- list(
      person   = object$person,
      item     = object$item,
      rhat.dat = object$rhat.dat
    )
  }

  class(summary_obj) <- "summary.sspLNIRT"
  summary_obj
}

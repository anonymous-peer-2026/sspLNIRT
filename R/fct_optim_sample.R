#' Optimize the Minimum Sample Size using Monte Carlo Simulations
#'
#' @description
#' Finds the minimum sample size \eqn{N} such that the root mean squared error
#' (RMSE) of one or more target item parameters falls below a specified
#' threshold. The search uses a bisection algorithm: at each step, simulated
#' data are generated and estimated via [comp_rmse()], and the search interval
#' is halved depending on whether the RMSE target is met.
#'
#' Data are simulated under the Joint Hierarchical Model (JHM) using a
#' two-parameter normal ogive model for response accuracy and a log-normal
#' model for response times.
#'
#' @param thresh Numeric vector. Target RMSE threshold(s) for the item
#'   parameter(s) named in `out.par`. Must be positive and the same length as
#'   `out.par`.
#' @param out.par Character vector. Name(s) of the target item parameter(s),
#'   each one of `"alpha"`, `"beta"`, `"phi"`, or `"lambda"`. Order must match
#'   `thresh`.
#' @param range Integer vector of length 2. Lower and upper bounds of the
#'   sample size search interval. Must satisfy `range[1] < range[2]` and
#'   `range[1] >= 2`.
#' @param iter Integer. Number of Monte Carlo replications per \eqn{N}
#'   evaluation. Default is 200.
#' @param K Integer. Test length (number of items). Default is 30.
#' @param mu.person Numeric vector of length 2. Population means of
#'   \eqn{(\theta, \zeta)}.
#' @param mu.item Numeric vector of length 4. Population means of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}.
#' @param meanlog.sigma2 Numeric. Mean of the log-normal distribution for the
#'   residual variance \eqn{\sigma^2}.
#' @param cov.m.person 2x2 symmetric matrix. Covariance matrix of
#'   \eqn{(\theta, \zeta)}.
#' @param cov.m.item 4x4 symmetric matrix. Covariance (or correlation) matrix
#'   of \eqn{(\alpha, \beta, \varphi, \lambda)}. See `cor2cov.item`.
#' @param sdlog.sigma2 Numeric. Standard deviation of the log-normal
#'   distribution for \eqn{\sigma^2}. Default is 0.
#' @param item.pars.m Matrix with 4 columns or `NULL`. If supplied, item
#'   parameters are held constant across replications.
#' @param cor2cov.item Logical. If `TRUE`, `cov.m.item` is treated as a
#'   correlation matrix and converted using `sd.item`.
#' @param sd.item Numeric vector of length 4 or `NULL`. Standard deviations of
#'   item parameters. Required when `cor2cov.item = TRUE`.
#' @param seed Integer, `TRUE`, or `NULL`. Passed to [comp_rmse()] for
#'   parallel-safe seeding.
#' @param XG Integer. Number of Gibbs sampler iterations per chain. Default
#'   5000.
#' @param burnin Integer. Burn-in percentage (0--99). Default is 20.
#' @param keep.err.dat Logical. Whether to retain the full error data in the
#'   [comp_rmse()] output at the optimal \eqn{N}.
#' @param keep.rhat.dat Logical. Whether to retain the full \eqn{\hat{R}}
#'   matrix in the [comp_rmse()] output at the optimal \eqn{N}.
#' @param verbose Logical. If `TRUE`, progress information is emitted via
#'   [message()] (which can be suppressed with [suppressMessages()]).
#'   Defaults to [interactive()].
#'
#' @return A list with S3 class `"sspLNIRT"` containing:
#' \describe{
#'   \item{`N.min`}{Integer: the minimum sample size that met all thresholds.
#'     If the lower bound already satisfies the threshold, the character
#'     string `"res.lb < thresh"` is returned. If the upper bound does not
#'     satisfy the threshold, `"res.ub > thresh"` is returned.}
#'   \item{`res.best`}{Named numeric vector. RMSE of the target parameter(s)
#'     at the optimal \eqn{N} (or at the boundary that triggered early
#'     stopping).}
#'   \item{`comp.rmse`}{List. Full [comp_rmse()] output at the optimal
#'     \eqn{N}.}
#'   \item{`trace`}{List with optimization diagnostics (`steps`,
#'     `track.res`, `track.N`, `time.taken`).}
#' }
#'
#' @seealso [comp_rmse()] for the per-\eqn{N} evaluation;
#'   [get_sspLNIRT()] for retrieving precomputed results.
#'
#' @examples
#' \dontrun{
#' future::plan(future::multisession, workers = 2)
#'
#' result <- optim_sample(
#'   thresh         = c(0.10, 0.15),
#'   out.par        = c("alpha", "beta"),
#'   range          = c(100, 500),
#'   iter           = 5,
#'   K              = 10,
#'   mu.person      = c(0, 0),
#'   mu.item        = c(1, 0, 0.5, 1),
#'   meanlog.sigma2 = log(0.3),
#'   cov.m.person   = matrix(c(1, 0.5, 0.5, 1), ncol = 2),
#'   cov.m.item     = matrix(c(1, 0,   0,   0,
#'                              0, 1,   0,   0.3,
#'                              0, 0,   1,   0,
#'                              0, 0.3, 0,   1), ncol = 4),
#'   sd.item        = c(0.2, 0.5, 0.2, 0.5),
#'   cor2cov.item   = TRUE,
#'   sdlog.sigma2   = 0.2,
#'   XG             = 500,
#'   seed           = 42
#' )
#'
#' summary(result)
#'
#' future::plan(future::sequential)
#' }
#'
#' @export
optim_sample <- function(out.par = "alpha",
                         thresh,
                         range = c(50, 2000),
                         iter = 200,
                         K = 30,
                         mu.person = c(0, 0),
                         mu.item = c(1, 0, 0.5, 1),
                         meanlog.sigma2 = log(0.6),
                         cov.m.person = matrix(c(1,   0.4,
                                                 0.4, 1), ncol = 2, byrow = TRUE),
                         cov.m.item = matrix(c(0.2, 0,   0,   0,
                                               0,   1,   0,   0.2,
                                               0,   0,   0.2, 0,
                                               0,   0.2, 0,   0.5), ncol = 4, byrow = TRUE),
                         sdlog.sigma2 = 0,
                         item.pars.m = NULL,
                         cor2cov.item = FALSE,
                         sd.item = NULL,
                         seed = NULL,
                         XG = 5000,
                         burnin = 20,
                         keep.err.dat = FALSE,
                         keep.rhat.dat = FALSE,
                         verbose = interactive()) {

  ## Input checks
  if (length(range) != 2 || range[1] >= range[2])
    stop("'range' must be a length-2 vector with range[1] < range[2].")
  if (range[1] < 2)
    stop("'range[1]' must be >= 2.")
  if (length(thresh) != length(out.par))
    stop("'thresh' and 'out.par' must have the same length.")
  if (any(thresh <= 0))
    stop("'thresh' must be positive.")
  if (!all(out.par %in% c("alpha", "beta", "phi", "lambda")))
    stop("'out.par' must be a subset of: alpha, beta, phi, lambda.")
  if (!is.numeric(iter) || iter < 1)
    stop("'iter' must be a positive integer.")
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
  if (!is.numeric(XG) || XG < 1)
    stop("'XG' must be a positive integer.")
  if (!is.numeric(burnin) || burnin < 0 || burnin >= 100)
    stop("'burnin' must be in [0, 100).")
  if (!is.logical(verbose) || length(verbose) != 1)
    stop("'verbose' must be TRUE or FALSE.")

  ## Helper: log progress only when verbose
  log_msg <- function(...) {
    if (isTRUE(verbose)) message(...)
  }

  ## Helper: evaluate comp_rmse at a given N
  compute_obj <- function(newN) {

    FUN.out <- comp_rmse(
      N              = newN,
      iter           = iter,
      K              = K,
      mu.person      = mu.person,
      mu.item        = mu.item,
      meanlog.sigma2 = meanlog.sigma2,
      cov.m.person   = cov.m.person,
      cov.m.item     = cov.m.item,
      sdlog.sigma2   = sdlog.sigma2,
      item.pars.m    = item.pars.m,
      cor2cov.item   = cor2cov.item,
      sd.item        = sd.item,
      keep.err.dat   = keep.err.dat,
      XG             = XG,
      burnin         = burnin,
      keep.rhat.dat  = keep.rhat.dat,
      seed           = seed
    )

    list(
      res       = FUN.out$item$rmse[out.par],
      mc.sd     = FUN.out$item$mc.sd.rmse[out.par],
      comp.rmse = FUN.out
    )
  }

  ## Initialise
  start.time <- Sys.time()
  lb <- range[1]
  ub <- range[2]
  n.par <- length(out.par)
  col.names <- c(paste0("res.lb.",   out.par),
                 paste0("res.ub.",   out.par),
                 paste0("res.temp.", out.par),
                 paste0("mc.sd.",    out.par))

  ## Evaluate lower bound
  res.lb <- compute_obj(newN = lb)
  log_msg("LB result is ", paste(format(res.lb$res), collapse = " "))

  if (all(res.lb$res < thresh)) {
    log_msg("stop due to res.lb < thresh with N = ", lb)

    track.res <- as.data.frame(matrix(NA, nrow = 1, ncol = 4 * n.par,
                                      dimnames = list(NULL, col.names)))
    track.res[1, paste0("res.lb.",   out.par)] <- res.lb$res
    track.res[1, paste0("res.temp.", out.par)] <- res.lb$res
    track.res[1, paste0("mc.sd.",    out.par)] <- res.lb$mc.sd

    output <- list(
      N.min     = "res.lb < thresh",
      res.best  = res.lb$res,
      comp.rmse = res.lb$comp.rmse,
      trace     = list(steps      = 1,
                       track.res  = track.res,
                       track.N    = data.frame(N.lb = lb, N.ub = ub, N.temp = lb),
                       time.taken = Sys.time() - start.time)
    )
    class(output) <- "sspLNIRT"
    return(output)
  }

  ## Evaluate upper bound
  res.ub <- compute_obj(newN = ub)
  log_msg("UB result is ", paste(format(res.ub$res), collapse = " "))

  if (all(res.ub$res > thresh)) {
    log_msg("stop due to res.ub > thresh with N = ", ub)

    track.res <- as.data.frame(matrix(NA, nrow = 2, ncol = 4 * n.par,
                                      dimnames = list(NULL, col.names)))
    track.res[1, paste0("res.lb.",   out.par)] <- res.lb$res
    track.res[1, paste0("res.temp.", out.par)] <- res.lb$res
    track.res[1, paste0("mc.sd.",    out.par)] <- res.lb$mc.sd

    track.res[2, paste0("res.lb.",   out.par)] <- res.lb$res
    track.res[2, paste0("res.ub.",   out.par)] <- res.ub$res
    track.res[2, paste0("res.temp.", out.par)] <- res.ub$res
    track.res[2, paste0("mc.sd.",    out.par)] <- res.ub$mc.sd

    output <- list(
      N.min     = "res.ub > thresh",
      res.best  = res.ub$res,
      comp.rmse = res.ub$comp.rmse,
      trace     = list(steps      = 2,
                       track.res  = track.res,
                       track.N    = data.frame(N.lb   = rep(lb, 2),
                                               N.ub   = rep(ub, 2),
                                               N.temp = c(lb, ub)),
                       time.taken = Sys.time() - start.time)
    )
    class(output) <- "sspLNIRT"
    return(output)
  }

  ## Bisection loop
  track.N <- data.frame(N.lb   = rep(lb, 2),
                        N.ub   = rep(ub, 2),
                        N.temp = c(lb, ub))

  track.res <- as.data.frame(matrix(NA, nrow = 2, ncol = 4 * n.par,
                                    dimnames = list(NULL, col.names)))

  track.res[1, paste0("res.lb.",   out.par)] <- res.lb$res
  track.res[1, paste0("res.temp.", out.par)] <- res.lb$res
  track.res[1, paste0("mc.sd.",    out.par)] <- res.lb$mc.sd

  track.res[2, paste0("res.lb.",   out.par)] <- res.lb$res
  track.res[2, paste0("res.ub.",   out.par)] <- res.ub$res
  track.res[2, paste0("res.temp.", out.par)] <- res.ub$res
  track.res[2, paste0("mc.sd.",    out.par)] <- res.ub$mc.sd

  res.temp <- res.lb
  N.lb     <- lb
  N.temp   <- ub
  N.ub     <- ub
  steps    <- 2

  repeat {

    inc <- (N.ub - N.lb) / 2
    if (inc < 1) {
      log_msg("stop due to inc ", inc, " with N = ", N.temp)
      break
    }

    if (all(res.temp$res < thresh)) {
      N.temp <- ceiling(N.lb + inc)
    } else {
      N.temp <- ceiling(N.ub - inc)
    }

    res.temp <- compute_obj(newN = N.temp)

    if (all(res.temp$res < thresh)) {
      N.ub   <- N.temp
      res.ub <- res.temp
    } else {
      N.lb   <- N.temp
      res.lb <- res.temp
    }

    steps <- steps + 1
    track.res[steps, paste0("res.lb.",   out.par)] <- res.lb$res
    track.res[steps, paste0("res.ub.",   out.par)] <- res.ub$res
    track.res[steps, paste0("res.temp.", out.par)] <- res.temp$res
    track.res[steps, paste0("mc.sd.",    out.par)] <- res.temp$mc.sd
    track.N[steps, ] <- c(N.lb, N.ub, N.temp)
    log_msg("New result is ", paste(format(c(res.lb$res, res.ub$res)),
                                    collapse = " "))
  }

  ## Assemble output
  N.min    <- N.ub
  res.best <- res.ub$res
  log_msg("Best result is ", paste(format(res.best), collapse = " "),
          " for threshold ", paste(thresh, collapse = " "))
  log_msg("Minimum N is ", N.min)

  output <- list(
    N.min     = N.min,
    res.best  = res.best,
    comp.rmse = res.ub$comp.rmse,
    trace     = list(steps      = steps,
                     track.res  = track.res,
                     track.N    = track.N,
                     time.taken = Sys.time() - start.time)
  )
  class(output) <- "sspLNIRT"
  output
}

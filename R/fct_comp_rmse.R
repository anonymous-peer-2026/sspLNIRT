#' Compute RMSE of Parameter Estimates using Monte-Carlo Simulations
#'
#' @description
#' Computes the root mean squared error (RMSE), Monte Carlo standard deviation,
#' and bias of estimated item and person parameters based on simulated data.
#' Data are generated under the Joint Hierarchical Model (JHM) using a
#' two-parameter normal ogive model for response accuracy and a log-normal
#' model for response times. Estimation is performed via MCMC (Gibbs sampling)
#' with four parallel chains per replication using [LNIRT::LNIRT()].
#'
#' @param N Integer. The sample size (number of persons). Must be >= 2.
#' @param iter Integer. The number of Monte Carlo replications (simulated data
#'   sets). Default is 200.
#' @param K Integer. The test length (number of items). Default is 30.
#' @param mu.person Numeric vector of length 2. Population means of the person
#'   parameters \eqn{(\theta, \zeta)}.
#' @param mu.item Numeric vector of length 4. Population means of the item
#'   parameters \eqn{(\alpha, \beta, \varphi, \lambda)}.
#' @param meanlog.sigma2 Numeric. Mean of the log-normal distribution for the
#'   residual variance \eqn{\sigma^2} of the response time model.
#' @param cov.m.person 2x2 symmetric matrix. Covariance (or correlation) matrix
#'   of \eqn{(\theta, \zeta)}.
#' @param cov.m.item 4x4 symmetric matrix. Covariance (or correlation) matrix
#'   of \eqn{(\alpha, \beta, \varphi, \lambda)}. See `cor2cov.item`.
#' @param sdlog.sigma2 Numeric. Standard deviation of the log-normal
#'   distribution for \eqn{\sigma^2}. Default is 0 (constant across items).
#' @param item.pars.m Matrix with 4 columns or `NULL`. If supplied, item
#'   parameters are held constant across replications (rows = items, columns =
#'   \eqn{\alpha, \beta, \varphi, \lambda}).
#' @param cor2cov.item Logical. If `TRUE`, `cov.m.item` is treated as a
#'   correlation matrix and converted to a covariance matrix using `sd.item`.
#' @param sd.item Numeric vector of length 4 or `NULL`. Standard deviations of
#'   \eqn{(\alpha, \beta, \varphi, \lambda)}. Required when `cor2cov.item =
#'   TRUE`.
#' @param XG Integer. Number of Gibbs sampler iterations per chain. Default is
#'   5000.
#' @param burnin Integer. Burn-in percentage (0--99). Default is 20.
#' @param seed Integer or `TRUE`. Random seed passed to
#'   [future.apply::future_lapply()] via `future.seed`. An integer gives a
#'   reproducible L'Ecuyer-CMRG seed sequence; `TRUE` (the default when `NULL`
#'   is supplied) generates a random parallel-safe seed. See **Note**.
#' @param keep.err.dat Logical. If `TRUE`, the full per-replication error data
#'   are returned. If `FALSE` (default), errors are binned into `K` quantile
#'   bins per parameter.
#' @param keep.rhat.dat Logical. If `TRUE`, the full \eqn{\hat{R}} matrix
#'   (items x replications) is returned. If `FALSE` (default), it is discarded.
#'
#' @return A list with S3 class `"sspLNIRT"` containing:
#' \describe{
#'   \item{`person`}{List with named vectors `rmse`, `mc.sd.rmse`, and `bias`
#'     for \eqn{\theta} and \eqn{\zeta}.}
#'   \item{`item`}{List with named vectors `rmse`, `mc.sd.rmse`, and `bias`
#'     for \eqn{\alpha}, \eqn{\beta}, \eqn{\varphi}, \eqn{\lambda}, and
#'     \eqn{\sigma^2}.}
#'   \item{`rhat.dat`}{Matrix (items x replications) of \eqn{\hat{R}} values
#'     if `keep.rhat.dat = TRUE`, otherwise `NULL`.}
#'   \item{`err.dat`}{List with data frames `person` and `item`. If
#'     `keep.err.dat = FALSE`, errors are binned (columns: `par`, `bin`,
#'     `mean_sim`, `mean_err`, `mean_rmse`). If `TRUE`, raw per-replication
#'     errors (columns: `rep`, `par`, `sim.val`, `err`).}
#' }
#'
#' @note
#' When `seed = NULL` is passed, it is internally converted to `TRUE`, which
#' triggers automatic parallel-safe seeding via [future.apply::future_lapply()].
#' Pass an explicit integer for full reproducibility.
#'
#' Computation is parallelized over replications using the
#' \pkg{future}/\pkg{future.apply} framework. Set a parallel backend (e.g.,
#' `future::plan(future::multisession)`) before calling this function.
#'
#' @seealso [optim_sample()] which calls this function at each bisection step.
#'
#' @examples
#' \dontrun{
#' # Minimal example
#' future::plan(future::multisession, workers = 2)
#'
#' result <- comp_rmse(
#'   N              = 100,
#'   iter           = 3,
#'   K              = 10,
#'   mu.person      = c(0, 0),
#'   mu.item        = c(1, 0, 0.5, 1),
#'   meanlog.sigma2 = log(0.6),
#'   cov.m.person   = matrix(c(1, 0.4, 0.4, 1), ncol = 2),
#'   cov.m.item     = diag(4),
#'   sd.item        = c(0.2, 1, 0.2, 0.5),
#'   cor2cov.item   = TRUE,
#'   sdlog.sigma2   = 0,
#'   XG             = 500,
#'   burnin         = 20,
#'   seed           = 42,
#'   keep.err.dat   = FALSE,
#'   keep.rhat.dat  = TRUE
#' )
#'
#' result$item$rmse
#' result$person$rmse
#'
#' future::plan(future::sequential)
#' }
#'
#' @export
comp_rmse <- function(N,
                      iter = 200,
                      K = 30,
                      mu.person = c(0, 0),
                      mu.item = c(1, 0, 0.5, 1),
                      meanlog.sigma2 = log(0.6),
                      cov.m.person = matrix(c(1, 0.4,
                                              0.4, 1), ncol = 2, byrow = TRUE),
                      cov.m.item = matrix(c(0.2, 0,   0,   0,
                                            0,   1,   0,   0.2,
                                            0,   0,   0.2, 0,
                                            0,   0.2, 0,   0.5), ncol = 4, byrow = TRUE),
                      sdlog.sigma2 = 0,
                      item.pars.m = NULL,
                      cor2cov.item = FALSE,
                      sd.item = NULL,
                      XG = 5000,
                      burnin = 20,
                      seed = NULL,
                      keep.err.dat = FALSE,
                      keep.rhat.dat = FALSE) {

  # input checks
  if (!is.numeric(N) || length(N) != 1 || N < 2)
    stop("'N' must be a single integer >= 2.")
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
  if (is.null(seed)) {seed <- TRUE}

  # set progressr
  handler <- list(progressr::handler_txtprogressbar())

  # run in parallel
  out <- progressr::with_progress({
    p <- progressr::progressor(steps = iter)

    future.apply::future_lapply(
      X = seq_len(iter),
      FUN = function(i) {

        # simulate data
        data <- sim.jhm.data(iter = 1,
                             N = N,
                             K = K,
                             mu.person = mu.person,
                             mu.item = mu.item,
                             meanlog.sigma2 = meanlog.sigma2,
                             cov.m.person = cov.m.person,
                             cov.m.item = cov.m.item,
                             sdlog.sigma2 = sdlog.sigma2,
                             item.pars.m = item.pars.m,
                             cor2cov.item = cor2cov.item,
                             sd.item = sd.item)

        RT <- data$time.data[[1]]
        Y  <- data$response.data[[1]]
        item.par   <- data$item.par[[1]]
        person.par <- data$person.par[[1]]
        scale.factor <- data$scale.factor[[1]]

        rm(data); gc()

        # LNIRT with 4 chains
        fit.list <- vector("list", 4)
        for (f in 1:4) {
          fit.list[[f]] <- LNIRT::LNIRT(
            RT = RT,
            Y = Y,
            XG = XG,
            burnin = burnin,
            residual = FALSE,
            par1 = TRUE
          )
        }

        # compute r.hat
        r.hat.out <- rhat_LNIRT(fit.list, chains = 4, cutoff = NULL)
        rhat.dat <- c(unlist(r.hat.out$value$D2.item))

        # compute posterior means
        XGburnin <- ceiling(fit.list[[1]]$XG * fit.list[[1]]$burnin / 100)
        idx <- XGburnin:fit.list[[1]]$XG
        nchains <- length(fit.list)

        post.theta <- Reduce(`+`, lapply(fit.list, \(x) colMeans(x$MCMC.Samples$Person.Ability[idx, , drop = FALSE]))) / nchains
        post.zeta  <- Reduce(`+`, lapply(fit.list, \(x) colMeans(x$MCMC.Samples$Person.Speed[idx, , drop = FALSE]))) / nchains

        post.alpha  <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Item.Discrimination)) / nchains
        post.beta   <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Item.Difficulty)) / nchains
        post.phi    <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Time.Discrimination)) / nchains
        post.lambda <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Time.Intensity)) / nchains
        post.sigma2 <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Sigma2)) / nchains

        # drop objects in workers
        for (f in seq_along(fit.list)) fit.list[[f]]$MCMC.Samples <- NULL
        rm(fit.list, RT, Y)
        gc()

        # re-scale to input scale
        post.item.pars <- cbind(post.alpha, post.beta, post.phi, post.lambda, post.sigma2)
        post.person.pars <- cbind(post.theta, post.zeta)
        re.scaled.post <- scale_M(item.pars = post.item.pars,
                                  person.pars = post.person.pars,
                                  re.scale = TRUE,
                                  c.alpha = scale.factor$c.alpha,
                                  c.phi = scale.factor$c.phi)
        re.scaled.data <- scale_M(item.pars = item.par,
                                  person.pars = person.par,
                                  re.scale = TRUE,
                                  c.alpha = scale.factor$c.alpha,
                                  c.phi = scale.factor$c.phi)

        # calculate errors on input scale
        res <- list(
          err.theta  = (re.scaled.post$person.pars.scaled$post.theta  - re.scaled.data$person.pars.scaled$theta),
          err.zeta   = (re.scaled.post$person.pars.scaled$post.zeta   - re.scaled.data$person.pars.scaled$zeta),
          err.alpha  = (re.scaled.post$items.pars.scaled$post.alpha   - re.scaled.data$items.pars.scaled$alpha),
          err.beta   = (re.scaled.post$items.pars.scaled$post.beta    - re.scaled.data$items.pars.scaled$beta),
          err.phi    = (re.scaled.post$items.pars.scaled$post.phi     - re.scaled.data$items.pars.scaled$phi),
          err.lambda = (re.scaled.post$items.pars.scaled$post.lambda  - re.scaled.data$items.pars.scaled$lambda),
          err.sigma2 = (re.scaled.post$items.pars.scaled$post.sigma2  - re.scaled.data$items.pars.scaled$sigma2),
          rhat.dat   = rhat.dat
        )

        # true values
        res$sim.alpha  <- re.scaled.data$items.pars.scaled$alpha
        res$sim.beta   <- re.scaled.data$items.pars.scaled$beta
        res$sim.phi    <- re.scaled.data$items.pars.scaled$phi
        res$sim.lambda <- re.scaled.data$items.pars.scaled$lambda
        res$sim.sigma2 <- re.scaled.data$items.pars.scaled$sigma2
        res$sim.theta  <- re.scaled.data$person.pars.scaled$theta
        res$sim.zeta   <- re.scaled.data$person.pars.scaled$zeta

        # empty memory
        rm(post.theta, post.zeta, post.alpha, post.beta, post.phi, post.lambda, post.sigma2,
           post.item.pars, post.person.pars, re.scaled.post, re.scaled.data,
           item.par, person.par, scale.factor)
        gc()

        # track and return
        p()
        return(res)
      },
      future.seed = seed,
      future.stdout = FALSE,
      future.packages = c("LNIRT"),
      future.globals = structure(
        TRUE,
        add = c(
          "sim.jhm.data",
          "scale_M",
          "rhat_LNIRT",
          "person.par",
          "item.par"
        )
      )
    )
  },
  handlers = handler)

  ## Aggregate error array: person x parameter x replication
  err.person <- simplify2array(lapply(out, function(x) {
    cbind(
      theta = x$err.theta,
      zeta  = x$err.zeta
    )
  }))

  bias.person     <- apply(err.person, 2, mean, na.rm = TRUE)
  rmse.rep.person <- sqrt(apply(err.person^2, c(3, 2), mean, na.rm = TRUE))
  rmse.person     <- apply(rmse.rep.person, 2, mean, na.rm = TRUE)
  mc.sd.person    <- apply(rmse.rep.person, 2, sd, na.rm = TRUE)

  person <- list(
    rmse       = rmse.person,
    mc.sd.rmse = mc.sd.person,
    bias       = bias.person
  )

  ## Aggregate error array: item x parameter x replication
  err.item <- simplify2array(lapply(out, function(x) {
    cbind(
      alpha  = x$err.alpha,
      beta   = x$err.beta,
      phi    = x$err.phi,
      lambda = x$err.lambda,
      sigma2 = x$err.sigma2
    )
  }))

  bias.item     <- apply(err.item, 2, mean, na.rm = TRUE)
  rmse.rep.item <- sqrt(apply(err.item^2, c(3, 2), mean, na.rm = TRUE))
  rmse.item     <- apply(rmse.rep.item, 2, mean, na.rm = TRUE)
  mc.sd.item    <- apply(rmse.rep.item, 2, sd, na.rm = TRUE)

  item <- list(
    rmse       = rmse.item,
    mc.sd.rmse = mc.sd.item,
    bias       = bias.item
  )

  ## Full error data
  err.item <- do.call(rbind, Map(function(x, r) {
    data.frame(
      rep     = r,
      par     = rep(c("alpha", "beta", "phi", "lambda", "sigma2"),
                    times = c(length(x$err.alpha),
                              length(x$err.beta),
                              length(x$err.phi),
                              length(x$err.lambda),
                              length(x$err.sigma2))),
      sim.val = c(x$sim.alpha, x$sim.beta, x$sim.phi, x$sim.lambda, x$sim.sigma2),
      err     = c(x$err.alpha, x$err.beta, x$err.phi, x$err.lambda, x$err.sigma2),
      stringsAsFactors = FALSE
    )
  }, out, seq_along(out)))

  err.person <- do.call(rbind, Map(function(x, r) {
    data.frame(
      rep     = r,
      par     = rep(c("theta", "zeta"),
                    times = c(length(x$err.theta),
                              length(x$err.zeta))),
      sim.val = c(x$sim.theta, x$sim.zeta),
      err     = c(x$err.theta, x$err.zeta),
      stringsAsFactors = FALSE
    )
  }, out, seq_along(out)))

  ## Bin error data
  if (!keep.err.dat) {
    n.bins <- K

    ntile_base <- function(x, n) {
      floor((rank(x, ties.method = "first") - 1) * n / length(x)) + 1L
    }

    bin_summarise <- function(df, n.bins) {
      do.call(rbind, lapply(split(df, df$par), function(d) {
        d$bin <- ntile_base(d$sim.val, n.bins)
        do.call(rbind, lapply(split(d, d$bin), function(b) {
          data.frame(
            par       = b$par[1],
            bin       = b$bin[1],
            mean_sim  = mean(b$sim.val, na.rm = TRUE),
            mean_err  = mean(b$err, na.rm = TRUE),
            mean_rmse = sqrt(mean(b$err^2, na.rm = TRUE))
          )
        }))
      }))
    }

    err.item   <- bin_summarise(err.item, n.bins)
    err.person <- bin_summarise(err.person, n.bins)
    rownames(err.item)   <- NULL
    rownames(err.person) <- NULL
  }

  ## Convergence diagnostics
  rhat.dat <- sapply(out, \(x) x$rhat.dat)
  if (!keep.rhat.dat) {
    rhat.dat <- NULL
  }

  rm(out)
  gc()

  ## Return
  output <- list(
    person   = person,
    item     = item,
    rhat.dat = rhat.dat,
    err.dat  = list(person = err.person,
                    item   = err.item)
  )
  class(output) <- "sspLNIRT"
  return(output)
}

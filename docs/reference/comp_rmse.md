# Compute RMSE of Parameter Estimates using Monte-Carlo Simulations

Computes the root mean squared error (RMSE), Monte Carlo standard
deviation, and bias of estimated item and person parameters based on
simulated data. Data are generated under the Joint Hierarchical Model
(JHM) using a two-parameter normal ogive model for response accuracy and
a log-normal model for response times. Estimation is performed via MCMC
(Gibbs sampling) with four parallel chains per replication using
\[LNIRT::LNIRT()\].

## Usage

``` r
comp_rmse(
  N,
  iter = 200,
  K = 30,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 0.5, 1),
  meanlog.sigma2 = log(0.6),
  cov.m.person = matrix(c(1, 0.4, 0.4, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(0.2, 0, 0, 0, 0, 1, 0, 0.2, 0, 0, 0.2, 0, 0, 0.2, 0, 0.5), ncol =
    4, byrow = TRUE),
  sdlog.sigma2 = 0,
  item.pars.m = NULL,
  cor2cov.item = FALSE,
  sd.item = NULL,
  XG = 5000,
  burnin = 20,
  seed = NULL,
  keep.err.dat = FALSE,
  keep.rhat.dat = FALSE
)
```

## Arguments

- N:

  Integer. The sample size (number of persons). Must be \>= 2.

- iter:

  Integer. The number of Monte Carlo replications (simulated data sets).
  Default is 200.

- K:

  Integer. The test length (number of items). Default is 30.

- mu.person:

  Numeric vector of length 2. Population means of the person parameters
  \\(\theta, \zeta)\\.

- mu.item:

  Numeric vector of length 4. Population means of the item parameters
  \\(\alpha, \beta, \varphi, \lambda)\\.

- meanlog.sigma2:

  Numeric. Mean of the log-normal distribution for the residual variance
  \\\sigma^2\\ of the response time model.

- cov.m.person:

  2x2 symmetric matrix. Covariance (or correlation) matrix of \\(\theta,
  \zeta)\\.

- cov.m.item:

  4x4 symmetric matrix. Covariance (or correlation) matrix of \\(\alpha,
  \beta, \varphi, \lambda)\\. See \`cor2cov.item\`.

- sdlog.sigma2:

  Numeric. Standard deviation of the log-normal distribution for
  \\\sigma^2\\. Default is 0 (constant across items).

- item.pars.m:

  Matrix with 4 columns or \`NULL\`. If supplied, item parameters are
  held constant across replications (rows = items, columns = \\\alpha,
  \beta, \varphi, \lambda\\).

- cor2cov.item:

  Logical. If \`TRUE\`, \`cov.m.item\` is treated as a correlation
  matrix and converted to a covariance matrix using \`sd.item\`.

- sd.item:

  Numeric vector of length 4 or \`NULL\`. Standard deviations of
  \\(\alpha, \beta, \varphi, \lambda)\\. Required when \`cor2cov.item =
  TRUE\`.

- XG:

  Integer. Number of Gibbs sampler iterations per chain. Default is
  5000.

- burnin:

  Integer. Burn-in percentage (0–99). Default is 20.

- seed:

  Integer or \`TRUE\`. Random seed passed to
  \[future.apply::future_lapply()\] via \`future.seed\`. An integer
  gives a reproducible L'Ecuyer-CMRG seed sequence; \`TRUE\` (the
  default when \`NULL\` is supplied) generates a random parallel-safe
  seed. See \*\*Note\*\*.

- keep.err.dat:

  Logical. If \`TRUE\`, the full per-replication error data are
  returned. If \`FALSE\` (default), errors are binned into \`K\`
  quantile bins per parameter.

- keep.rhat.dat:

  Logical. If \`TRUE\`, the full \\\hat{R}\\ matrix (items x
  replications) is returned. If \`FALSE\` (default), it is discarded.

## Value

A list with S3 class \`"sspLNIRT"\` containing:

- \`person\`:

  List with named vectors \`rmse\`, \`mc.sd.rmse\`, and \`bias\` for
  \\\theta\\ and \\\zeta\\.

- \`item\`:

  List with named vectors \`rmse\`, \`mc.sd.rmse\`, and \`bias\` for
  \\\alpha\\, \\\beta\\, \\\varphi\\, \\\lambda\\, and \\\sigma^2\\.

- \`rhat.dat\`:

  Matrix (items x replications) of \\\hat{R}\\ values if \`keep.rhat.dat
  = TRUE\`, otherwise \`NULL\`.

- \`err.dat\`:

  List with data frames \`person\` and \`item\`. If \`keep.err.dat =
  FALSE\`, errors are binned (columns: \`par\`, \`bin\`, \`mean_sim\`,
  \`mean_err\`, \`mean_rmse\`). If \`TRUE\`, raw per-replication errors
  (columns: \`rep\`, \`par\`, \`sim.val\`, \`err\`).

## Note

When \`seed = NULL\` is passed, it is internally converted to \`TRUE\`,
which triggers automatic parallel-safe seeding via
\[future.apply::future_lapply()\]. Pass an explicit integer for full
reproducibility.

Computation is parallelized over replications using the
future/future.apply framework. Set a parallel backend (e.g.,
\`future::plan(future::multisession)\`) before calling this function.

## See also

\[optim_sample()\] which calls this function at each bisection step.

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal example
future::plan(future::multisession, workers = 2)

result <- comp_rmse(
  N              = 100,
  iter           = 3,
  K              = 10,
  mu.person      = c(0, 0),
  mu.item        = c(1, 0, 0.5, 1),
  meanlog.sigma2 = log(0.6),
  cov.m.person   = matrix(c(1, 0.4, 0.4, 1), ncol = 2),
  cov.m.item     = diag(4),
  sd.item        = c(0.2, 1, 0.2, 0.5),
  cor2cov.item   = TRUE,
  sdlog.sigma2   = 0,
  XG             = 500,
  burnin         = 20,
  seed           = 42,
  keep.err.dat   = FALSE,
  keep.rhat.dat  = TRUE
)

result$item$rmse
result$person$rmse

future::plan(future::sequential)
} # }
```

# Optimize the Minimum Sample Size using Monte Carlo Simulations

Finds the minimum sample size \\N\\ such that the root mean squared
error (RMSE) of one or more target item parameters falls below a
specified threshold. The search uses a bisection algorithm: at each
step, simulated data are generated and estimated via \[comp_rmse()\],
and the search interval is halved depending on whether the RMSE target
is met.

Data are simulated under the Joint Hierarchical Model (JHM) using a
two-parameter normal ogive model for response accuracy and a log-normal
model for response times.

## Usage

``` r
optim_sample(
  out.par = "alpha",
  thresh,
  range = c(50, 2000),
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
  seed = NULL,
  XG = 5000,
  burnin = 20,
  keep.err.dat = FALSE,
  keep.rhat.dat = FALSE,
  verbose = interactive()
)
```

## Arguments

- out.par:

  Character vector. Name(s) of the target item parameter(s), each one of
  \`"alpha"\`, \`"beta"\`, \`"phi"\`, or \`"lambda"\`. Order must match
  \`thresh\`.

- thresh:

  Numeric vector. Target RMSE threshold(s) for the item parameter(s)
  named in \`out.par\`. Must be positive and the same length as
  \`out.par\`.

- range:

  Integer vector of length 2. Lower and upper bounds of the sample size
  search interval. Must satisfy \`range\[1\] \< range\[2\]\` and
  \`range\[1\] \>= 2\`.

- iter:

  Integer. Number of Monte Carlo replications per \\N\\ evaluation.
  Default is 200.

- K:

  Integer. Test length (number of items). Default is 30.

- mu.person:

  Numeric vector of length 2. Population means of \\(\theta, \zeta)\\.

- mu.item:

  Numeric vector of length 4. Population means of \\(\alpha, \beta,
  \varphi, \lambda)\\.

- meanlog.sigma2:

  Numeric. Mean of the log-normal distribution for the residual variance
  \\\sigma^2\\.

- cov.m.person:

  2x2 symmetric matrix. Covariance matrix of \\(\theta, \zeta)\\.

- cov.m.item:

  4x4 symmetric matrix. Covariance (or correlation) matrix of \\(\alpha,
  \beta, \varphi, \lambda)\\. See \`cor2cov.item\`.

- sdlog.sigma2:

  Numeric. Standard deviation of the log-normal distribution for
  \\\sigma^2\\. Default is 0.

- item.pars.m:

  Matrix with 4 columns or \`NULL\`. If supplied, item parameters are
  held constant across replications.

- cor2cov.item:

  Logical. If \`TRUE\`, \`cov.m.item\` is treated as a correlation
  matrix and converted using \`sd.item\`.

- sd.item:

  Numeric vector of length 4 or \`NULL\`. Standard deviations of item
  parameters. Required when \`cor2cov.item = TRUE\`.

- seed:

  Integer, \`TRUE\`, or \`NULL\`. Passed to \[comp_rmse()\] for
  parallel-safe seeding.

- XG:

  Integer. Number of Gibbs sampler iterations per chain. Default 5000.

- burnin:

  Integer. Burn-in percentage (0–99). Default is 20.

- keep.err.dat:

  Logical. Whether to retain the full error data in the \[comp_rmse()\]
  output at the optimal \\N\\.

- keep.rhat.dat:

  Logical. Whether to retain the full \\\hat{R}\\ matrix in the
  \[comp_rmse()\] output at the optimal \\N\\.

- verbose:

  Logical. If \`TRUE\`, progress information is emitted via
  \[message()\] (which can be suppressed with \[suppressMessages()\]).
  Defaults to \[interactive()\].

## Value

A list with S3 class \`"sspLNIRT"\` containing:

- \`N.min\`:

  Integer: the minimum sample size that met all thresholds. If the lower
  bound already satisfies the threshold, the character string \`"res.lb
  \< thresh"\` is returned. If the upper bound does not satisfy the
  threshold, \`"res.ub \> thresh"\` is returned.

- \`res.best\`:

  Named numeric vector. RMSE of the target parameter(s) at the optimal
  \\N\\ (or at the boundary that triggered early stopping).

- \`comp.rmse\`:

  List. Full \[comp_rmse()\] output at the optimal \\N\\.

- \`trace\`:

  List with optimization diagnostics (\`steps\`, \`track.res\`,
  \`track.N\`, \`time.taken\`).

## See also

\[comp_rmse()\] for the per-\\N\\ evaluation; \[get_sspLNIRT()\] for
retrieving precomputed results.

## Examples

``` r
if (FALSE) { # \dontrun{
future::plan(future::multisession, workers = 2)

result <- optim_sample(
  thresh         = c(0.10, 0.15),
  out.par        = c("alpha", "beta"),
  range          = c(100, 500),
  iter           = 5,
  K              = 10,
  mu.person      = c(0, 0),
  mu.item        = c(1, 0, 0.5, 1),
  meanlog.sigma2 = log(0.3),
  cov.m.person   = matrix(c(1, 0.5, 0.5, 1), ncol = 2),
  cov.m.item     = matrix(c(1, 0,   0,   0,
                             0, 1,   0,   0.3,
                             0, 0,   1,   0,
                             0, 0.3, 0,   1), ncol = 4),
  sd.item        = c(0.2, 0.5, 0.2, 0.5),
  cor2cov.item   = TRUE,
  sdlog.sigma2   = 0.2,
  XG             = 500,
  seed           = 42
)

summary(result)

future::plan(future::sequential)
} # }
```

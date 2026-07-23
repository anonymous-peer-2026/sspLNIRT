# Plot Simulated Response Accuracy

Simulates data under the Joint Hierarchical Model and plots the
resulting response accuracy. The plot type depends on \`level\`,
\`by.theta\` and \`overlay\`:

\- \*\*Person, by distribution\*\*: Histogram of total correct scores
across persons. - \*\*Person, by theta\*\*: Mean total score as a
function of ability \\\theta\\, shown as a bar chart binned by
\\\theta\\. - \*\*Item, by distribution\*\*: Density of \\P(X = 1)\\
across persons for each item, with the item-wise median marked. -
\*\*Item, by theta\*\*: Item characteristic curves (ICCs), showing \\P(X
= 1)\\ as a function of \\\theta\\ for each item, with a dashed line at
the item difficulty \\\beta\\. - \*\*Item, \`overlay = TRUE\`\*\*: All
item curves are drawn in a single panel instead of a facet grid.

Data are generated via \[sim.jhm.data()\] with \`scale = FALSE\`.

## Usage

``` r
plot_RA(
  design = NULL,
  level = "item",
  by.theta = TRUE,
  overlay = FALSE,
  N = 1000,
  K = 30,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 0.5, 1),
  meanlog.sigma2 = log(0.6),
  cov.m.person = matrix(c(1, 0.4, 0.4, 1), ncol = 2, byrow = FALSE),
  cov.m.item = matrix(c(1, 0, 0, 0, 0, 1, 0, 0.4, 0, 0, 1, 0, 0, 0.4, 0, 1), ncol = 4,
    byrow = TRUE),
  sd.item = c(0.2, 1, 0.2, 0.5),
  sdlog.sigma2 = 0,
  item.pars.m = NULL,
  cor2cov.item = FALSE
)
```

## Arguments

- design:

  Optional list or \`"sspLNIRT.design"\` object holding the
  data-generating design. When supplied, its fields are used for any
  design-related argument the caller did not pass explicitly (\`K\`,
  \`mu.person\`, \`mu.item\`, \`meanlog.sigma2\`, \`cov.m.person\`,
  \`cov.m.item\`, \`sd.item\`, \`sdlog.sigma2\`, \`item.pars.m\`,
  \`cor2cov.item\`). Caller- supplied arguments always take precedence.
  Extra fields in \`design\` (e.g. \`thresh\`, \`out.par\`, \`seed\`)
  are ignored, so the object returned in \`\$design\` by
  \[get_sspLNIRT()\] or \[optim_sample()\] can be passed directly.
  Default \`NULL\` (use the function defaults / caller arguments).

- level:

  Character. \`"person"\` for person-level aggregates or \`"item"\` for
  item-level curves / distributions.

- by.theta:

  Logical. If \`TRUE\`, the x-axis is \\\theta\\; if \`FALSE\`, a
  marginal distribution is shown.

- overlay:

  Logical. If \`TRUE\` and \`level = "item"\`, all item curves are drawn
  in one panel rather than faceted.

- N:

  Integer. Sample size (number of persons) for the simulated data.
  Default is 1000.

- K:

  Integer. Test length (number of items). Default is 10.

- mu.person:

  Numeric vector of length 2. Population means of \\(\theta, \zeta)\\.

- mu.item:

  Numeric vector of length 4. Population means of \\(\alpha, \beta,
  \varphi, \lambda)\\.

- meanlog.sigma2:

  Numeric. Mean of the log-normal distribution for \\\sigma^2\\ (on the
  log scale).

- cov.m.person:

  2x2 symmetric matrix. Covariance matrix of \\(\theta, \zeta)\\.

- cov.m.item:

  4x4 symmetric matrix. Covariance (or correlation) matrix of \\(\alpha,
  \beta, \varphi, \lambda)\\. See \`cor2cov.item\`.

- sd.item:

  Numeric vector of length 4. Standard deviations of \\(\alpha, \beta,
  \varphi, \lambda)\\. Required when \`cor2cov.item = TRUE\`.

- sdlog.sigma2:

  Numeric. Standard deviation of the log-normal distribution for
  \\\sigma^2\\. Default is 0.

- item.pars.m:

  Matrix with 4 columns or \`NULL\`. If supplied, item parameters are
  held fixed instead of drawn from the truncated MVN.

- cor2cov.item:

  Logical. If \`TRUE\`, \`cov.m.item\` is treated as a correlation
  matrix and converted using \`sd.item\`.

## Value

A \[ggplot2::ggplot\] object.

## See also

\[plot_RT()\] for the response time counterpart; \[theme_sspLNIRT()\].

## Examples

``` r
if (FALSE) { # \dontrun{
plot_RA(level = "person", by.theta = TRUE, N = 500, K = 20)
plot_RA(level = "item", by.theta = FALSE, N = 1000, K = 5,
        mu.item = c(1, 0, 0.5, 1), sd.item = c(0.2, 0.5, 0.2, 0.5))

# Many items: overlay instead of faceting
plot_RA(level = "item", by.theta = TRUE, overlay = TRUE, N = 1000, K = 30)

# Pass a design object retrieved from get_sspLNIRT() (or optim_sample()):
res <- get_sspLNIRT(thresh = 0.10, out.par = "alpha",
                    K = 30, mu.alpha = 1,
                    meanlog.sigma2 = log(0.6), rho = 0.4)
plot_RA(res$design, level = "item", by.theta = TRUE)
} # }
```

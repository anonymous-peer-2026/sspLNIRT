# Retrieve Precomputed Sample Size Results

Looks up precomputed \[optim_sample()\] results from the internal
dataset \[sspLNIRT.data\]. For each target parameter / threshold pair,
the function finds the matching configuration and returns the result
whose minimum \\N\\ is the bottleneck (i.e., the largest required sample
size across all requested parameters).

When multiple target parameters are supplied, each is matched
independently against the precomputed grid. The returned result
corresponds to whichever single-parameter configuration demanded the
largest \\N\\, since that \\N\\ guarantees all other parameters also
meet their thresholds (RMSE is monotonically decreasing in \\N\\).

## Usage

``` r
get_sspLNIRT(thresh, out.par, K, mu.alpha, meanlog.sigma2, rho)
```

## Arguments

- thresh:

  Numeric vector. Target RMSE threshold(s). Same length as \`out.par\`.

- out.par:

  Character vector. Item parameter(s): each one of \`"alpha"\`,
  \`"beta"\`, \`"phi"\`, \`"lambda"\`.

- K:

  Integer. Test length.

- mu.alpha:

  Numeric. Population mean of the discrimination parameter.

- meanlog.sigma2:

  Numeric. Mean of the log-normal distribution for \\\sigma^2\\ (on the
  log scale).

- rho:

  Numeric in \\\[-1, 1\]\\. Correlation between \\\theta\\ and
  \\\zeta\\.

## Value

A list with components:

- \`object\`:

  An object of class \`"sspLNIRT"\` as returned by \[optim_sample()\].

- \`design\`:

  A list with the full set of parameter values used for the
  precomputation.

## Details

Searches \[sspLNIRT.data\] row by row using approximate matching (\`tol
= 1e-3\`) for numeric parameters. If no exact match is found, the error
message reports which parameters differed in the closest available
configuration.

\## Bottleneck selection

After matching each \`out.par\[j\]\` / \`thresh\[j\]\` pair
independently, the bottleneck result is selected by: 1. If any \`N.min
== "res.ub \> thresh"\`, that result is returned. 2. Otherwise, the
result with the largest numeric \`N.min\` is returned. 3. Otherwise (all
\`N.min == "res.lb \< thresh"\`), the first such result is returned.

## See also

\[optim_sample()\]; \[sspLNIRT.data\]; \[available_configs()\].

## Examples

``` r
if (FALSE) { # \dontrun{
result <- get_sspLNIRT(
  thresh         = 0.10,
  out.par        = "alpha",
  K              = 30,
  mu.alpha       = 1,
  meanlog.sigma2 = log(0.6),
  rho            = 0.2
)
summary(result$object)
} # }
```

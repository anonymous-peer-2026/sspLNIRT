# List Available Precomputed Configurations

Returns a data frame summarizing all parameter combinations for which
precomputed \[optim_sample()\] results are available in
\[sspLNIRT.data\]. Use this to check which values of \`thresh\`,
\`out.par\`, \`K\`, \`mu.alpha\`, \`meanlog.sigma2\`, and \`rho\` can be
passed to \[get_sspLNIRT()\].

## Usage

``` r
available_configs()
```

## Value

A data frame with one row per precomputed configuration and the
following columns:

- \`thresh\`:

  RMSE threshold.

- \`out.par\`:

  Target item parameter.

- \`K\`:

  Test length.

- \`mu.alpha\`:

  Mean of \\\alpha\\ (item discrimination).

- \`meanlog.sigma2\`:

  Mean of \\\log(\sigma^2)\\.

- \`rho\`:

  Correlation between \\\theta\\ and \\\zeta\\.

## See also

\[get_sspLNIRT()\] which looks up results by these parameters;
\[sspLNIRT.data\] for the underlying dataset.

## Examples

``` r
configs <- available_configs()
head(configs)
#>   thresh out.par  K mu.alpha meanlog.sigma2 rho
#> 1   0.20     phi 50      1.4         log(1) 0.4
#> 2   0.10   alpha 50      1.0         log(1) 0.6
#> 3   0.10    beta 30      0.8       log(0.2) 0.6
#> 4   0.15   alpha 50      1.4       log(0.2) 0.2
#> 5   0.15    beta 50      0.6         log(1) 0.2
#> 6   0.05    beta 50      1.0       log(0.2) 0.2

# unique test lengths
unique(configs$K)
#> [1] 50 30
```

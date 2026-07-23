# Plot Power Curve from Sample Size Optimization

Extracts the optimization trace from an \[optim_sample()\] result, fits
a log-log regression (\\\log(\mathrm{RMSE}) \sim \log(N)\\), and
displays the relationship on both log-log and original scales side by
side. The RMSE threshold and minimum \\N\\ are overlaid as reference
lines.

This function is preserved for backward compatibility. New code should
prefer \`plot(object, type = "power_curve", ...)\`.

## Usage

``` r
plot_power_curve(object, out.par = NULL, thresh)
```

## Arguments

- object:

  An object of class \`"sspLNIRT"\` containing a \`\$trace\` element, as
  returned by \[optim_sample()\] or retrieved via \[get_sspLNIRT()\].

- out.par:

  Character or \`NULL\`. Which item parameter's RMSE trace to plot (one
  of \`"alpha"\`, \`"beta"\`, \`"phi"\`, \`"lambda"\`). When \`NULL\`
  (default), the first name of \`object\$res.best\` is used; if that is
  also unavailable, an error is raised.

- thresh:

  Numeric. The RMSE threshold used in the optimization. Must be a single
  positive number.

## Value

A \[ggplot2::ggplot\] object with two facets (log-log scale and original
scale).

## See also

\[plot.sspLNIRT()\] for the recommended interface; \[optim_sample()\]
for producing the trace; \[plot_estimation()\] for visualizing
estimation accuracy by parameter value.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- get_sspLNIRT(
  thresh = 0.10, out.par = "alpha",
  K = 30, mu.alpha = 1,
  meanlog.sigma2 = log(0.6), rho = 0.2
)
plot_power_curve(result$object, out.par = "alpha", thresh = 0.10)
# equivalent and preferred:
plot(result$object, type = "power_curve", out.par = "alpha", thresh = 0.10)
} # }
```

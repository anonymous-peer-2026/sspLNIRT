# Plot Method for sspLNIRT Objects

S3 method for the \[base::plot()\] generic, dispatched on objects of
class \`"sspLNIRT"\` (returned by \[optim_sample()\], \[comp_rmse()\],
or \[get_sspLNIRT()\]). The \`type\` argument selects the visualization:

- \`"estimation"\` (default):

  Per-parameter RMSE or bias as a function of the true (simulated)
  parameter value, faceted by parameter. See \[plot_estimation()\].

- \`"power_curve"\`:

  Optimization trace as a log-log power curve plus the original-scale
  view, with the RMSE threshold and minimum \\N\\ overlaid. Requires an
  \[optim_sample()\] result. See \[plot_power_curve()\].

Arguments specific to each \`type\` are passed through \`...\`.

## Usage

``` r
# S3 method for class 'sspLNIRT'
plot(x, y = NULL, type = c("estimation", "power_curve"), ...)
```

## Arguments

- x:

  An object of class \`"sspLNIRT"\`.

- y:

  Unused (required by the generic). Ignored with a warning if supplied.

- type:

  Character. One of \`"estimation"\` or \`"power_curve"\`.

- ...:

  Additional arguments passed to the underlying plot helper. For \`type
  = "estimation"\`: \`pars\` (\`"item"\` / \`"person"\`), \`y.val\`
  (\`"rmse"\` / \`"bias"\`), \`n.bins\`. For \`type = "power_curve"\`:
  \`out.par\`, \`thresh\`.

## Value

A \[ggplot2::ggplot\] object.

## See also

\[plot_estimation()\], \[plot_power_curve()\], \[theme_sspLNIRT()\].

## Examples

``` r
if (FALSE) { # \dontrun{
result <- get_sspLNIRT(
  thresh = 0.10, out.par = "alpha",
  K = 30, mu.alpha = 1,
  meanlog.sigma2 = log(0.6), rho = 0.2
)

# estimation accuracy
plot(result$object, type = "estimation", pars = "item", y.val = "rmse")

# power curve from the optimization trace
plot(result$object, type = "power_curve",
     out.par = "alpha", thresh = 0.10)
} # }
```

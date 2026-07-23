# Plot Estimation Metrics by Parameter Value

Plots the root mean squared error (RMSE) or bias of estimated parameters
as a function of their true (simulated) values, aggregated into bins.
This reveals how estimation accuracy varies across the parameter range
(e.g., whether extreme item difficulties are estimated less precisely).

Accepts output from \[optim_sample()\], \[get_sspLNIRT()\], or
\[comp_rmse()\]. For \[optim_sample()\] output, the error data at the
minimum \\N\\ are used. If the data are already binned (i.e.,
\`keep.err.dat = FALSE\` in \[comp_rmse()\]), bins are plotted as-is;
otherwise, raw errors are binned on the fly using \`n.bins\`. For item
parameters, \\\sigma^2\\ is excluded.

This function is preserved for backward compatibility. New code should
prefer \`plot(object, type = "estimation", ...)\`.

## Usage

``` r
plot_estimation(object, pars = "item", y.val = "rmse", n.bins = 30)
```

## Arguments

- object:

  An object of class \`"sspLNIRT"\`, as returned by \[optim_sample()\],
  \[get_sspLNIRT()\], or \[comp_rmse()\].

- pars:

  Character. \`"item"\` or \`"person"\`. Which parameter set to plot.

- y.val:

  Character. \`"rmse"\` or \`"bias"\`. Metric for the y-axis.

- n.bins:

  Integer. Number of quantile bins for aggregation. Only used when the
  error data are in full (unbinned) format. Default is 30.

## Value

A \[ggplot2::ggplot\] object, faceted by parameter.

## See also

\[plot.sspLNIRT()\] for the recommended interface;
\[plot_power_curve()\] for visualizing the optimization trace;
\[theme_sspLNIRT()\].

## Examples

``` r
if (FALSE) { # \dontrun{
plot_estimation(result, pars = "item", y.val = "rmse")
# equivalent and preferred:
plot(result, type = "estimation", pars = "item", y.val = "rmse")
} # }
```

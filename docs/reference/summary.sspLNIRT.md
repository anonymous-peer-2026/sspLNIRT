# Summarise an sspLNIRT Object

Provides a structured summary of a \`sspLNIRT\` object. The method
auto-detects whether the object was produced by \[optim_sample()\]
(contains \`N.min\`) or by \[comp_rmse()\] (contains \`person\` and
\`item\`), and returns the relevant fields.

## Usage

``` r
# S3 method for class 'sspLNIRT'
summary(object, ...)
```

## Arguments

- object:

  An object of class \`"sspLNIRT"\`, as returned by \[optim_sample()\]
  or \[comp_rmse()\].

- ...:

  Additional arguments (currently unused).

## Value

An object of class \`"summary.sspLNIRT"\`.

For \[optim_sample()\] output, the summary contains:

- \`N.min\`:

  Integer or character. Minimum sample size (or boundary message).

- \`out.par\`:

  Character vector. Target parameter(s) tracked by the optimization,
  recovered from \`track.res\` columns.

- \`res.best\`:

  Named numeric vector. RMSE at the optimal \\N\\.

- \`comp.rmse\`:

  List. Full \[comp_rmse()\] output at the optimal \\N\\.

- \`trace\`:

  List. Optimization diagnostics (steps, tracked results and \\N\\
  values, wall-clock time).

For \[comp_rmse()\] output, the summary contains:

- \`person\`:

  List with \`rmse\`, \`mc.sd.rmse\`, and \`bias\` for \\\theta\\ and
  \\\zeta\\.

- \`item\`:

  List with \`rmse\`, \`mc.sd.rmse\`, and \`bias\` for item parameters.

- \`rhat.dat\`:

  Matrix of \\\hat{R}\\ values or \`NULL\`.

## See also

\[optim_sample()\], \[comp_rmse()\], \[print.summary.sspLNIRT()\].

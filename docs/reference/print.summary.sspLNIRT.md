# Print Method for sspLNIRT Summary Objects

Formats and prints a \`"summary.sspLNIRT"\` object to the console. The
output adapts to the source: \[optim_sample()\] results show the minimum
sample size, optimization diagnostics, and parameter accuracy tables;
\[comp_rmse()\] results show item and person parameter accuracy tables.

## Usage

``` r
# S3 method for class 'summary.sspLNIRT'
print(x, digits = 4, ...)
```

## Arguments

- x:

  An object of class \`"summary.sspLNIRT"\`, as returned by
  \[summary.sspLNIRT()\].

- digits:

  Integer. Number of decimal places shown in tables. Default 4.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns \`x\`.

## See also

\[summary.sspLNIRT()\].

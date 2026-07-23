# sspLNIRT Plot Theme

A consistent ggplot2 theme used across all plotting functions in the
package. Extends \[ggplot2::theme_minimal()\] with tighter panel grids,
consistent strip styling for facets, and slightly larger axis text.

Use as the final layer of any ggplot, e.g. \`p + theme_sspLNIRT()\`.

## Usage

``` r
theme_sspLNIRT(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Numeric. Base font size in points. Default \`11\`.

- base_family:

  Character. Base font family. Default \`""\` (system).

## Value

A \[ggplot2::theme\] object.

## See also

\[scale_colour_sspLNIRT()\], \[scale_fill_sspLNIRT()\] for the matching
discrete palette helpers; \[plot.sspLNIRT()\] for the main plotting
entry point.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_sspLNIRT()
} # }
```

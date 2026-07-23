# Discrete colour / fill scales for sspLNIRT plots

Discrete ggplot2 scales matching \[theme_sspLNIRT()\]. The palette is a
grey-to-slate-blue ramp.

## Usage

``` r
scale_colour_sspLNIRT(...)

scale_fill_sspLNIRT(...)
```

## Arguments

- ...:

  Passed to \[ggplot2::discrete_scale()\].

## Value

A ggplot2 discrete scale.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point() +
  scale_colour_sspLNIRT() +
  theme_sspLNIRT()
} # }
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" alt="sspLNIRT logo" style="float: right; width: 120px; margin: 0 0 15px 15px;" />

# sspLNIRT

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/anonymous-peer-2026/sspLNIRT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/anonymous-peer-2026/sspLNIRT/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/anonymous-peer-2026/sspLNIRT/graph/badge.svg)](https://app.codecov.io/gh/anonymous-peer-2026/sspLNIRT)
<!-- badges: end -->

`sspLNIRT` is a sample size planning tool for item calibration with the
Joint Hierarchical Model (JHM) of response accuracy and response time.
It estimates the minimum sample size required to achieve a target
accuracy (RMSE) of item parameter estimates under a specified
data-generating process.

The package provides:

- **Precomputed results** for various design conditions, accessible
  instantly via a Shiny app and/or R package.
- **Custom Sample Size Estimation** via `optim_sample()` for design
  conditions outside the precomputed data.
- **Diagnostic functions** for inspecting parameter accuracy or bias,
  power curves, and implied response time and response accuracy
  distributions.

## Usage

The tool is available as an R package and as an interactive Shiny app.

### Web App

Use the app at
[anonymous-peer-2026.shinyapps.io/sspLNIRT](https://anonymous-peer-2026.shinyapps.io/sspLNIRT/).

### Installation

You can install the latest version of the R package from GitHub:

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("anonymous-peer-2026/sspLNIRT")
```

### System Requirements

The `sspLNIRT` package was built under R version 4.4.3 using Apple clang
version 16.0.0 (clang-1600.0.26.6) and GNU Fortran (GCC) 14.2.0. To
compile R packages from source, install the appropriate toolchain:

- macOS: see <https://mac.r-project.org/tools/>
- Windows: see <https://cran.r-project.org/bin/windows/Rtools/>

### Run

Launch the Shiny app locally with:

``` r
sspLNIRT::run_app()
```

## Documentation

Vignettes and full function documentation are available at
[github.com/anonymous-peer-2026/sspLNIRT](https://github.com/anonymous-peer-2026/sspLNIRT/).

## Citation

Please cite `sspLNIRT` if you use it. To cite the software, use:

Author A (2026). *sspLNIRT: Sample Size Planning for Item Calibration
using the Joint Hierarchical Model*. R package version 0.0.0.9000,
<https://github.com/anonymous-peer-2026/sspLNIRT>.

Or copy the reference information to your BibTeX file:

``` bibtex
@Manual{,
    title = {sspLNIRT: Sample Size Planning for Item Calibration using the Joint Hierarchical Model},
    author = {Anonymous Author},
    year = {2026},
    note = {R package version 0.0.0.9000},
    url = {https://github.com/anonymous-peer-2026/sspLNIRT},
  }
```

## Code of Conduct

I am open to feedback and new ideas. Please mind the Contributor Code of
Conduct.

## About

You are reading the doc about version: 0.0.0.9000

This README has been compiled on 2026-07-23 16:27:28.

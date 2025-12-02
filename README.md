
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nalanda

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nalanda)](https://CRAN.R-project.org/package=nalanda)
[![R-CMD-check](https://github.com/centerconflictcooperation/nalanda/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/centerconflictcooperation/nalanda/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/centerconflictcooperation/nalanda/graph/badge.svg)](https://app.codecov.io/gh/centerconflictcooperation/nalanda)
<!-- badges: end -->

## About the Name

The package is named after [Nalanda
Mahavihara](https://en.wikipedia.org/wiki/Nalanda), one of the most
renowned centers of learning in ancient India. Founded in the 5th
century CE, Nalanda was a Buddhist monastic university that attracted
scholars from across Asia and became a symbol of knowledge, wisdom, and
the pursuit of learning through texts and collaboration.

This name is particularly fitting for a package related to the study of
books and prosociality, reflecting the historical significance of
Nalanda as a center for both scholarly texts and the cooperative
exchange of ideas. The connection resonates with contemporary research
on how books and shared learning can foster prosocial behavior and
cooperation.

Learn more about related research on books, learning, and prosociality:
[Mind and Life Europe - 2024 EVA Recipients &
Projects](https://mindandlife-europe.org/2024-eva-recipients-projects/)

## Overview

The **nalanda** package provides tools and utilities for analyzing
research data related to books, reading, and prosocial behavior. The
package aims to facilitate the exploration of how books and shared
learning experiences can foster prosociality and cooperation in
communities.

## Installation

You can install the development version of nalanda from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("centerconflictcooperation/nalanda")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nalanda)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

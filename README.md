
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maya

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/maya)](https://cran.r-project.org/package=maya)
[![Build
Status](https://travis-ci.org/edgararuiz/maya.svg?branch=master)](https://travis-ci.org/edgararuiz/maya)
[![Coverage
status](https://codecov.io/gh/edgararuiz/maya/branch/master/graph/badge.svg)](https://codecov.io/github/edgararuiz/maya?branch=master)
<!-- badges: end -->

Provides functions to convert between the Mayan calendar dates and
Gregorian dates.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/maya")
```

## Example

``` r
library(maya)

mayan_to_gregorian("13.0.0.0.0")
#> [1] "December 21, 2012 CE"
```

``` r
gregorian_to_mayan("August 11, 3114 BCE")
#> [1] "0.0.0.0.0"
```

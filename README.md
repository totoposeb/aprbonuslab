
# aprbonuslab

<!-- badges: start -->
[![R-CMD-check](https://github.com/totoposeb/aprbonuslab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/totoposeb/aprbonuslab/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an implementation of the multiple linear regression algorithm in R, using QR decomposition and a reference class (RC): `linreg()`

## Installation

You can install the development version of aprbonuslab from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("totoposeb/aprbonuslab")
```

## Example

This is a basic example which shows you how to fit a linear regression model with `linreg`:

``` r
library(aprbonuslab)
m <- linreg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
m$print()
m$coef()
m$plot()
```


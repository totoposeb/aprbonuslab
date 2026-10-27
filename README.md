
# aprbonuslab

<!-- badges: start -->
[![R-CMD-check](https://github.com/totoposeb/aprbonuslab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/totoposeb/aprbonuslab/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an R package with two regression models, implemented as reference classes (RC) and fitted with the QR decomposition:

- `linreg`: multiple linear regression.
- `ridgereg`: ridge regression.

It also includes `visualize_airport_delays()`, which plots the mean arrival delay of flights by destination airport from the [nycflights13](https://cran.r-project.org/package=nycflights13) data.

## Installation

You can install the development version of aprbonuslab from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("totoposeb/aprbonuslab")
```

## Examples

Fit a linear regression model with `linreg`:

``` r
library(aprbonuslab)
m <- linreg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
m$print()
m$coef()
m$plot()
```

Fit a ridge regression model with `ridgereg` and predict new data:

``` r
m <- ridgereg$new(
  Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1
)
m$print()
m$coef()
m$predict(iris[1:5, ])
```

Plot the mean flight delay by airport:

``` r
visualize_airport_delays()
```


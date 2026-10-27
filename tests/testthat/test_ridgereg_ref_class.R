data("iris")

test_that("ridgereg rejects bad input", {
  expect_error(ridgereg_mod <- ridgereg$new(
    formula = Petal.Length ~ Sepdsal.Width + Sepal.Length, data = iris,
    lambda = 1
  ))
  expect_error(ridgereg_mod <- ridgereg$new(
    formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = irfsfdis,
    lambda = 1
  ))
  expect_error(ridgereg_mod <- ridgereg$new(
    formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
    lambda = -1
  ))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(
    Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1
  )

  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(
    Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1
  )

  expect_output(
    ridgereg_mod$print(),
    "ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 1\\)"
  )
  expect_output(
    ridgereg_mod$print(),
    "( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length"
  )
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(
    Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 0
  )
  lm_mod <- lm(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  newdata <- iris[c(1, 5, 7), c("Sepal.Width", "Sepal.Length")]

  # With lambda = 0, ridge regression gives the same predictions as lm()
  expect_equal(unname(ridgereg_mod$predict()), unname(fitted(lm_mod)))
  expect_equal(
    unname(ridgereg_mod$predict(newdata)), unname(predict(lm_mod, newdata))
  )
})

test_that("coef() method works", {
  ridgereg_mod <- ridgereg$new(
    Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 0
  )
  lm_mod <- lm(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # With lambda = 0, ridge regression gives the same coefficients as lm()
  expect_equal(ridgereg_mod$coef(), coef(lm_mod))
})

test_that("coefficients are similar to MASS::lm.ridge()", {
  # lm.ridge() divides by n instead of n - 1 when normalizing, hence the
  # tolerance
  for (lambda in c(1, 10)) {
    ridgereg_mod <- ridgereg$new(
      Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = lambda
    )
    lm_ridge_mod <- MASS::lm.ridge(
      Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = lambda
    )

    expect_equal(
      unname(ridgereg_mod$coef()), unname(coef(lm_ridge_mod)),
      tolerance = 1e-2
    )
  }
})

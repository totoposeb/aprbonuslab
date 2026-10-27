#' Ridge regression using the QR decomposition
#'
#' @description
#' `ridgereg` is a reference class (RC) for ridge regression. Calling
#' `ridgereg$new(formula, data, lambda)` fits the model and stores the results
#' in the object.
#'
#' @details
#' `ridgereg$new()` takes three arguments:
#' * `formula`: a formula such as `y ~ x1 + x2`.
#' * `data`: a data frame with every variable in `formula`.
#' * `lambda`: the ridge penalty, a single number of 0 or more.
#'
#' The covariates are normalized before fitting, and the intercept is not
#' penalized. The penalty is added to the data as extra rows, which turns
#' ridge regression into a least squares problem that is solved with the QR
#' decomposition.
#'
#' @section Methods:
#' * `print()`: prints the call and the coefficients.
#' * `predict(newdata)`: returns predictions for `newdata`, or the fitted
#'   values if `newdata` is not given.
#' * `coef()`: returns the coefficients.
#'
#' @field formula The model formula.
#' @field data_name Name of the data, used by `print()`.
#' @field X Design matrix.
#' @field y Values of the dependent variable.
#' @field beta Coefficients, including the intercept.
#' @field y_hat Fitted values.
#' @field lambda The ridge penalty.
#' @field model_terms Model terms, used by `predict()`.
#' @field xlevels Factor levels, used by `predict()`.
#'
#' @examples
#' m <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris,
#'                   lambda = 1)
#'
#' @importFrom methods new setRefClass
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <- setRefClass(
  "ridgereg",
  fields = list(
    formula = "formula", data_name = "character", X = "matrix", y = "numeric",
    beta = "numeric", y_hat = "numeric", lambda = "numeric",
    model_terms = "ANY", xlevels = "list"
  ),
  methods = list(
    initialize = function(formula, data, lambda) {
      # Validates inputs
      stopifnot(
        "`formula` is not a formula!" = inherits(formula, "formula"),
        "`data` is not a data.frame!" = is.data.frame(data),
        "`lambda` must be a single non-negative number!" = is.numeric(lambda) &&
          length(lambda) == 1 && lambda >= 0
      )

      formula <<- formula
      data_name <<- deparse(substitute(data))
      lambda <<- lambda

      # Gets data
      mf <- model.frame(formula, data = data)
      model_terms <<- attr(mf, "terms")
      xlevels <<- .getXlevels(model_terms, mf)
      X <<- model.matrix(model_terms, mf)
      y <<- model.response(mf)

      # Separates the covariates from the intercept, which is not penalized
      X_cov <- X[, colnames(X) != "(Intercept)", drop = FALSE]
      p <- ncol(X_cov)

      # Normalizes the covariates: X_norm = (X_cov - x_mean) / x_sd
      x_mean <- apply(X_cov, 2, mean)
      x_sd <- apply(X_cov, 2, sd)
      X_norm <- scale(X_cov, center = x_mean, scale = x_sd)

      # Centers the response: the intercept on the normalized scale is mean(y)
      y_mean <- mean(y)
      y_c <- y - y_mean # centered values for y

      # Solves for the beta coefficients (normalized scale) with QR
      X_aug <- rbind(X_norm, diag(sqrt(lambda), p))
      y_aug <- c(y_c, rep(0, p))
      QR <- qr(X_aug)
      Q <- qr.Q(QR)
      R <- qr.R(QR)
      beta_norm <- drop(backsolve(r = R, x = crossprod(Q, y_aug)))

      # Transforms the coefficients back to the original scale
      slopes <- beta_norm / x_sd
      intercept <- y_mean - sum(slopes * x_mean)
      beta <<- c("(Intercept)" = intercept, slopes)

      # Calculates fitted values
      y_hat <<- drop(X %*% beta)
    },
    print = function() {
      cat("Call:\n")
      cat(
        "ridgereg(formula = ", deparse1(formula), ", data = ", data_name,
        ", lambda = ", lambda, ")\n\n",
        sep = ""
      )
      cat("Coefficients:\n")
      base::print(beta)
    },
    predict = function(newdata = NULL) {
      # Returns the fitted values when no newdata is given
      if (is.null(newdata)) return(y_hat)

      # When newdata is given, calculates the new X_new matrix
      X_new <- model.matrix(
        delete.response(model_terms), newdata, xlev = xlevels
      )

      # Returns the new predictions with the fitted betas
      drop(X_new %*% beta)
    },
    coef = function() {
      beta
    },
    show = function() {
      print()
    }
  )
)

#' Constructor for logitreg results
#'
#' @param design design matrix with n rows and q columns
#' @param response response vector of length n
#' @param coefs optimal coefficients
#'
#' @return list of
#' `coefficients`: the regression coefficients,
#' `fitted`: the estimated probabilities
#' `data`: the original data (`design` & `response`) used for the fit.
new_logitreg <- function(design, response, coefs) {

  fitted <- design %*% as.matrix(coefs, ncol = 1)

  return_obj <- list(
    "coefficients" = coefs,
    "fitted" = fitted,
    "predictions" = logistic(fitted),
    data = list(
      "design" = design,
      "response" = response
    )
  )

  class(return_obj) <- append(class(return_obj), "logitreg")
  return_obj
}

validate_logitreg <- function(x) {
  checkmate::assert_class(x, "logitreg")
  x
}

#' @export
predict.logitreg <- function(object, ...) {
  object[["predictions"]]
}

#' @export
fitted.logitreg <- function(object, ...) {
  object[["fitted"]]
}

#' @export
coef.logitreg <- function(object, ...) {
  object[["coefficients"]]
}

#' @importFrom stats predict
#' @importFrom graphics plot
#' @export
plot.logitreg <- function(x, ...) {
  predictions <- ROCR::prediction(predict(x), x[["data"]][["response"]])
  perf <- ROCR::performance(predictions, "tpr", "fpr")
  plot(perf)
}

#' Fit logistic regression by minimising negative log likelihood.
#'
#' @param x formula or design matrix
#' @param y design matrix or response
#' @param design design matrix with n rows and q columns
#' @param response response vector of length n
#' @param init_coefs optional numeric vector of initial coefficient values
#' @param max_iter maximal number of allowed iterations
#' @param ... optional [optim()] arguments
#'
#' @return logitreg object, see [new_logitreg()]
#' @importFrom stats rnorm optim
#' @export
#' @md
logitreg <- function(x, y, init_coefs, max_iter, ...) {
  UseMethod("logitreg")
}

#' @rdname logitreg
#'
#' @importFrom stats model.frame model.matrix
#'
#' @examples
#' formula <- y ~ x
#' data <- data.frame(x = c(0, 1), y = c(0, 1))
#' logitreg(formula, data)
#' @export
#' @md
logitreg.formula <- function(x, y, ...) {

  formula <- x
  data <- y

  checkmate::assert_data_frame(data)

  data <- model.frame(formula, data)
  design <- model.matrix(formula, data)
  # drop regressors, keeping only response
  response <- data[ , !(names(data) %in% colnames(design)[-1])]
  logitreg.default(design, response, ...)
}

#' @rdname logitreg
#'
#' @examples
#' design <- matrix(c(1, 1, 0, 1), nrow = 2)
#' response <- c(0, 1)
#' logitreg(design, response)
#' @export
#' @md
logitreg.default <- function(x, y, init_coefs = rnorm(ncol(x)), max_iter = 1000, ...) {

  design <- x
  response <- y
  # invalid dimensions
  stopifnot(nrow(design) == length(response))

  # remove Inf and NA from design and response
  valid_row_mask <- !(is.na(rowSums(design)) | is.infinite(rowSums(design)) | is.na(response) | is.infinite(response))
  design <- design[valid_row_mask, ]
  response <- response[valid_row_mask]

  # validate design and response
  checkmate::assert_matrix(design, mode = "numeric", any.missing = FALSE)
  checkmate::assert_integerish(response, lower = 0, upper = 1, any.missing = FALSE)

  # validate initial coefficients
  checkmate::check_numeric(init_coefs, any.missing = FALSE, len = ncol(design))

  # validate max_iter
  checkmate::assert_count(max_iter, positive = TRUE)

  fit_logitreg(design, response, init_coefs, max_iter, ...)

  }

#' @rdname logitreg
fit_logitreg <- function(design, response, init_coefs, max_iter, ...) {
  optimal_coefs <- optim(
    par = init_coefs,
    fn = neg_loglik,
    gr = neg_loglik_deriv,
    design = design,
    response = response,
    method = "BFGS",
    control = list("maxit" = max_iter),
    ...
  )[["par"]]

  new_logitreg(design, response, optimal_coefs)
}


#' Squash score predictions between 0 and 1 using the sigmoid function.
#'
#' @param x scores in real numbers
#'
#' @return probability predictions
#' @importFrom stats plogis
logistic <- function(x) plogis(x)

#' Compute the negative log likelihood of logistic regression.
#'
#' @param coefs vector of regression coefficients
#' @param design design matrix with n rows and q columns
#' @param response response vector of length n
#'
#' @return negative log likelihood
#' @export
#'
#' @examples
#' coefs <- matrix(c(-23, 47), nrow = 2)
#' design <- matrix(c(1, 1, 0, 1), nrow = 2)
#' response <- c(0, 1)
#' neg_loglik(coefs, design, response)
#' @importFrom stats plogis
neg_loglik <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  -sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}

#' Compute the derivative of negative log likelihood of logistic regression.
#'
#' @rdname neg_loglik
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  -t(response - probabilities) %*% design
}

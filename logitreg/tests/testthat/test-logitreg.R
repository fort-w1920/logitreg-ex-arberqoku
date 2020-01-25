library(testthat)

context("logitreg")

test_that("basic implementation for loglik works", {
  coefs <- matrix(c(-23, 47), nrow = 2)
  design <- matrix(c(1, 1, 0, 1), nrow = 2)
  response <- c(0, 1)
  expect_equivalent(neg_loglik(coefs, design, response), 0, tolerance = 1e-6)
  expect_equivalent(neg_loglik_deriv(coefs, design, response), matrix(c(0, 0), ncol = 2), tolerance = 1e-6)
})

test_that("optimisation works", {
  n <- 100
  numerics <- 3
  factors <- 2
  seed <- 123

  data <- sim_data(n, numerics, factors, seed = seed)
  optimal_glm_coefs <- glm.fit(data[["design"]], data[["response"]],
                               family = binomial(link = "logit"))[["coefficients"]]
  logit_model <- logitreg(data[["design"]], data[["response"]])
  expect_equivalent(signif(logit_model[["coefficients"]], 2), signif(optimal_glm_coefs, 2))

  # test Inf and NA in design and/or response
  incomplete_design <- data[["design"]]
  incomplete_design[1, 2] <- NA
  incomplete_response <- data[["response"]]
  incomplete_response[[3]] <- Inf
  logit_model <- logitreg(incomplete_design, incomplete_response)
  expect_equivalent(length(logit_model[["fitted"]]), n - 2)

  # test invalid dimensions
  bad_dim_response <- data[["response"]]
  bad_dim_response <- c(0, bad_dim_response)
  expect_error(logitreg(data[["design"]], bad_dim_response))
  expect_error(logitreg(data[["design"]], data[["response"]], init_coefs = c(0)))

  # queer response
  non_binary_response <- data[["response"]]
  non_binary_response[[1]] <- 2
  expect_error(logitreg(data[["design"]], non_binary_response))

  # formula generic
  data <- sim_data(n, numerics, factors, seed = seed, dataframe = TRUE)
  formula <- response ~ .
  optimal_glm_coefs <- glm(formula, data, family = binomial(link = "logit"))[["coefficients"]]
  logit_model <- logitreg(formula, data)
  expect_equivalent(signif(logit_model[["coefficients"]], 2), signif(optimal_glm_coefs, 2))

})


test_that("external generics work", {
  n <- 100
  numerics <- 3
  factors <- 2
  seed <- 123

  data <- sim_data(n, numerics, factors, seed = seed)
  optimal_glm_coefs <- data[["coefs"]]
  logit_model <- logitreg(data[["design"]], data[["response"]])

  expect_s3_class(logit_model, "logitreg")
  expect_equal(fitted(logit_model), logit_model[["fitted"]])
  expect_equal(predict(logit_model), logit_model[["predictions"]])
  expect_equal(coef(logit_model), logit_model[["coefficients"]])

})


context("Test Auxiliary Functions")

test_that("checking to see if mean works", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(10, 0.5), 5)
  expect_equal(aux_mean(5, 0.2), 1)
  expect_length(aux_mean(10, 0.3), 1)
})

test_that("checking to see if variance works", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_length(aux_variance(10, 0.3), 1)
  expect_equal(aux_variance(100, 0.3), 21)
})

test_that("checking to see if mode works", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_length(aux_mode(10, 0.3), 1)
  expect_equal(aux_mode(5, 0.5), 3)
})

test_that("checking to see if skewness works", {
  expect_lt(aux_skewness(10, 0.3), 0.3)
  expect_length(aux_skewness(10, 0.3), 1)
  expect_lt(aux_skewness(10, 0.3), 10)
})

test_that("checking to see if kurtosis works", {
  expect_lt(aux_kurtosis(10, 0.3), 0.3)
  expect_length(aux_kurtosis(10, 0.3), 1)
  expect_lt(aux_kurtosis(15, 0.5), 0)
})

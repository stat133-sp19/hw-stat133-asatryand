prob1 <- 0.5
prob2 <- -0.01
prob3 <- 1.01
prob4 <- 0.7

trials1 <- 4
trials2 <- -5
trials3 <- 1000000
trials4 <- 0.1


context("Test Checker Functions")

test_that("test to see if check_prob works as expected", {
  expect_true(check_prob(0.5))
  expect_equal(check_prob(prob4), TRUE)
  expect_equal(check_prob(prob4), check_prob(prob1))
  expect_error(check_prob(prob2))
  expect_error(check_prob(prob3))
})

test_that("test to see if check_trials works as expected", {
  expect_true(check_trials(trials1))
  expect_equal(check_trials(trials3), TRUE)
  expect_equal(check_trials(trials3), check_trials(trials1))
  expect_error(check_trials(trials2))
  expect_error(check_trials(trials4))
})

test_that("test to see if check_success works as expected", {
  expect_true(check_success(1:5, 10))
  expect_true(check_success(5, 10))
  expect_true(check_success(1000, 10000))
  expect_error(check_success(1:6, 4))
  expect_error(check_success(5, 4))
})



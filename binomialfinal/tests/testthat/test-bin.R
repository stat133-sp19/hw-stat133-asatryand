context("Test Binomial Functions")

test_that("test to see if bin_choose works as expected", {
  expect_equal(bin_choose(n = 10, k = 4), 210)
  expect_length(bin_choose(10, 4), 1)
  expect_equal(bin_choose(n = 5, k = 2), 10)
})

test_that("test to see if bin_probability works as expected", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_length(bin_probability(success = 0:2, trials = 5, prob = 0.5), 3)
  expect_error(bin_probabiloty(success = 0:6), trials = 3, prob = 0.4)
})

test_that("test to see if bin_distribution works as expected", {
  expect_equal(class(bin_distribution(5, 0.5)), c("bindis", "data.frame"))
  expect_error(bin_distribution(5, -1))
  expect_error(bin_distribution(4.5, 0.3))
})

test_that("test to see if bin_cumulative works as expected", {
  expect_equal(class(bin_cumulative(5, 0.2)), c("bincum", "data.frame"))
  expect_error(bin_cumulative(4.5, 0.3))
  expect_error(bin_cumulative(4, -0.9))
})

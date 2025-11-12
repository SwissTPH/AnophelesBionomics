library(testthat)

test_that("Basic functionality with numerator and denominator", {
  df <- data.frame(a = c(1, 2, NA), b = c(3, NA, 2), c = c(10, 20, 30))
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"))
  expect_equal(result$x.num, c(4, NA, NA))
  expect_equal(result$x.den, c(10, 20, 30))
})

test_that("Zeros in denominator are turned to NA", {
  df <- data.frame(a = c(1, 2), b = c(3, 4), c = c(0, 10))
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"))
  expect_equal(result$x.den, c(NA, 10))
})

test_that("Missing numerator values replaced using percentage", {
  df <- data.frame(a = c(NA, NA), b = c(NA, NA), c = c(100, 200), p = c(10, 50))
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"), percentage_variables = c("p"))
  expect_equal(result$x.num, c(10, 100))
  expect_equal(result$x.den, c(100, 200))
})

test_that("No replacement when percentage is NA", {
  df <- data.frame(a = c(NA, NA), b = c(NA, NA), c = c(100, 200), p = c(NA, NA))
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"), percentage_variables = c("p"))
  expect_true(all(is.na(result$x.num)))
  expect_equal(result$x.den, c(100, 200))
})

test_that("Row with missing denominator returns NA in den column", {
  df <- data.frame(a = c(1), b = c(2), c = NA)
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"))
  expect_true(is.na(result$x.den))
  expect_equal(result$x.num, 3)
})

test_that("Multiple percentage columns are averaged", {
  df <- data.frame(a = NA, b = NA, c = 100, p1 = 20, p2 = 40)
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"), percentage_variables = c("p1", "p2"))
  expect_equal(result$x.num, 30)
  expect_equal(result$x.den, 100)
})

test_that("Only numerator without percentage keeps NA if missing", {
  df <- data.frame(a = NA, b = NA, c = 100)
  result <- augment_withProportion_modif(df, "x", denominator_variables = c("c"), numerator_variables = c("a", "b"))
  expect_true(is.na(result$x.num))
  expect_equal(result$x.den, 100)
})

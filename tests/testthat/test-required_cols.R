test_that("required_cols passes when all expected columns are present", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_silent(required_cols(df, c("a", "b"), "test_df"))
})

test_that("required_cols throws error when columns are missing", {
  df <- data.frame(a = 1:3)
  expect_error(required_cols(df, c("a", "b"), "test_df"),
               regexp = "Missing required columns in test_df: b")
})

test_that("required_cols handles multiple missing columns", {
  df <- data.frame(a = 1:3)
  expect_error(required_cols(df, c("a", "b", "c"), "input_data"),
               regexp = "Missing required columns in input_data: b, c")
})

test_that("required_cols handles empty expected list", {
  df <- data.frame(a = 1:3)
  expect_silent(required_cols(df, character(0), "test_df"))
})

test_that("required_cols handles data frame with no columns", {
  df <- data.frame()
  expect_error(required_cols(df, c("x", "y"), "empty_df"),
               regexp = "Missing required columns in empty_df: x, y")
})

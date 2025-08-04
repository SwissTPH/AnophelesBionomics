test_that("Function filters correctly when remove_unknown_insecticide = TRUE", {
  df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    insecticide_control = c("f", " t", "unknown"),
    endophily.num = c(5, 10, 3),
    endophily.den = c(10, 20, 6),
    stringsAsFactors = FALSE
  )

  result <- create_datareq(df, varname = "endophily", remove_unknown_insecticide = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$intervened), "f")
})

test_that("Function filters correctly when remove_unknown_insecticide = FALSE", {
  df <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    insecticide_control = c("f", "t", "unknown"),
    endophily.num = c(5, 10, 3),
    endophily.den = c(10, 20, 6),
    stringsAsFactors = FALSE
  )

  result <- create_datareq(df, varname = "endophily", remove_unknown_insecticide = FALSE)
  expect_equal(nrow(result), 2)
  expect_false("t" %in% result$intervened)
})

test_that("species column is renamed to survey", {
  df <- data.frame(
    species = c("a", "b"),
    insecticide_control = c("f", "f"),
    endophily.num = c(1, 2),
    endophily.den = c(3, 4),
    stringsAsFactors = FALSE
  )

  result <- create_datareq(df, varname = "endophily")
  expect_true("survey" %in% colnames(result))
  expect_false("species" %in% colnames(result))
})

test_that("Rows with missing denominators are removed", {
  df <- data.frame(
    species = c("a", "b"),
    insecticide_control = c("f", "f"),
    endophily.num = c(1, 2),
    endophily.den = c(NA, 4),
    stringsAsFactors = FALSE
  )

  result <- create_datareq(df, varname = "endophily")
  expect_equal(nrow(result), 1)
  expect_equal(result$endophily.den, 4)
})

test_that("Warning is issued when numerator > denominator", {
  df <- data.frame(
    species = c("a", "b"),
    insecticide_control = c("f", "f"),
    endophily.num = c(5, 2),
    endophily.den = c(3, 5),
    stringsAsFactors = FALSE
  )

  expect_output(
    create_datareq(df, varname = "endophily"),
    regexp = "denominator is smaller than numerator in 1 rows"
  )
})

test_that("Total number of valid observations is printed", {
  df <- data.frame(
    species = c("a", "b", "c"),
    insecticide_control = c("f", "f", "f"),
    endophily.num = c(1, 2, NA),
    endophily.den = c(3, 4, 5),
    stringsAsFactors = FALSE
  )

  expect_output(
    create_datareq(df, varname = "endophily"),
    regexp = "Total observations: 2"
  )
})

test_that("create_datareq returns data.frame with expected columns and structure", {
  df <- data.frame(
    species = c("sp1", "sp2", "sp3", "sp4", "sp5"),
    insecticide_control = c("f", "f", "t", NA, "f"),
    endophily.num = c(10, 5, 7, 3, 8),
    endophily.den = c(20, 10, 7, 4, NA),
    stringsAsFactors = FALSE
  )

  res <- create_datareq(df, varname = "endophily", remove_unknown_insecticide = TRUE)

  expect_s3_class(res, "data.frame")
  expect_true(all(!grepl(" ", res$intervened)))
})

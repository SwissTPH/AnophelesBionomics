test_that("augment_with_taxonomy returns list with expected names", {
  data <- data.frame(survey = c("s1", "s2"), stringsAsFactors = FALSE)
  compat <- data.frame(
    survey = c("s1", "s2"),
    species = c("sp1_compat", "sp2_compat"),
    complex = c("comp1_compat", "comp2_compat"),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(data, compat)

  expect_type(result, "list")
  expect_named(result, c("data", "species_complex"))
})

test_that("Returned data has correct structure and columns", {
  data <- data.frame(survey = c("s1", "s2"), stringsAsFactors = FALSE)
  compat <- data.frame(
    survey = c("s1", "s2"),
    species = c("sp1_compat", "sp2_compat"),
    complex = c("comp1_compat", "comp2_compat"),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(data, compat)

  expect_s3_class(result$data, "data.frame")
  expect_true(all(c("survey", "species", "complex", "speciesNb", "complexNb") %in% colnames(result$data)))
})

test_that("species_complex has correct structure and columns", {
  data <- data.frame(survey = c("s1", "s2"), stringsAsFactors = FALSE)
  compat <- data.frame(
    survey = c("s1", "s2"),
    species = c("sp1_compat", "sp2_compat"),
    complex = c("comp1_compat", "comp2_compat"),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(data, compat)

  expect_s3_class(result$species_complex, "data.frame")
  expect_true(all(c("species", "speciesNb", "complex", "complexNb") %in% colnames(result$species_complex)))
})

test_that("Mixed species and complex data", {
  test_data <- data.frame(
    survey = c(1, 2, 3, 4),
    value = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  test_compat <- data.frame(
    survey = c(1, 2, 3, 4),
    species = c("A", "B", "", ""),
    complex = c("", "", "X", "Y"),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(test_data, test_compat)

  expect_equal(sum(grepl("unlabeled ", result$data$species)), 2)
  expect_equal(result$data$complex[3], "X")
  expect_equal(result$data$species[4], "unlabeled Y")
})

test_that("Duplicate species names", {
  test_data <- data.frame(
    survey = c(1, 2, 3),
    value = c(5, 5, 5),
    stringsAsFactors = FALSE
  )

  test_compat <- data.frame(
    survey = c(1, 2, 3),
    species = c("A", "A", "B"),
    complex = c("", "X", ""),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(test_data, test_compat)

  expect_equal(result$data$speciesNb[1], result$data$speciesNb[2])
  expect_equal(result$data$complex[1], "A_complex")
  expect_equal(result$data$complex[2], "X")
})

test_that("Complex-only data", {
  test_data <- data.frame(
    survey = c(1, 2),
    value = c(7, 8),
    stringsAsFactors = FALSE
  )

  test_compat <- data.frame(
    survey = c(1, 2),
    species = c("", ""),
    complex = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  result <- augment_with_taxonomy(test_data, test_compat)

  expect_true(all(grepl("unlabeled ", result$data$species)))
  expect_equal(result$data$species, c("unlabeled X", "unlabeled Y"))
  expect_equal(result$data$complex, c("X", "Y"))
})

test_that("Missing required columns throws error", {
  test_data <- data.frame(
    survey = c(1, 2),
    wrong_column = c(9, 9),
    stringsAsFactors = FALSE
  )

  test_compat <- data.frame(
    wrong_id = c(1, 2),
    species = c("A", "B"),
    complex = c("", ""),
    stringsAsFactors = FALSE
  )

  expect_error(augment_with_taxonomy(test_data, test_compat))
})

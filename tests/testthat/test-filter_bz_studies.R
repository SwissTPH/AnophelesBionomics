test_that("filter_bz_studies correctly filters Endophagy studies", {
  input_data <- data.frame(
    indoor_biting_sampling = c("MBI", "Other"),
    outdoor_biting_sampling = c("MBO", "MBO"),
    HLC_same = c(TRUE, FALSE),
    dummy = 1:2,
    stringsAsFactors = FALSE
  )

  filtered <- filter_bz_studies(input_data, varname = "endophagy", nice_varname = "Endophagy")

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$indoor_biting_sampling, "MBI")
  expect_equal(filtered$outdoor_biting_sampling, "MBO")
  expect_true(filtered$HLC_same)
})

test_that("filter_bz_studies correctly filters Endophily studies", {
  input_data <- data.frame(
    indoor_resting_sampling = c("HRI", "Other"),
    outdoor_resting_sampling = c("WinExit", "WinExit"),
    WinExit_in_HRI_houses = c("t", "f"),
    dummy = 1:2,
    stringsAsFactors = FALSE
  )

  filtered <- filter_bz_studies(input_data, varname = "endophily", nice_varname = "Endophily")

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$indoor_resting_sampling, "HRI")
  expect_equal(filtered$outdoor_resting_sampling, "WinExit")
  expect_equal(filtered$WinExit_in_HRI_houses, "t")
})

test_that("filter_bz_studies filters out host_unit == 'AI' for HBI variables", {
  input_data <- data.frame(
    host_unit = c("AI", "HI"),
    host_sampling = c("RO", "Other"),
    dummy = 1:2,
    stringsAsFactors = FALSE
  )

  filtered <- filter_bz_studies(input_data, varname = "indoor_HBI", nice_varname = "HBI")

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$host_unit, "HI")
})

test_that("filter_bz_studies applies correct host_sampling filter for outdoor_HBI", {
  input_data <- data.frame(
    host_unit = c("HI", "HI", "HI", "HI", "AI"),
    host_sampling = c("RO", "RO (shelter)", "RO (pit)", "Other", "RO"),
    dummy = 1:5,
    stringsAsFactors = FALSE
  )

  filtered <- filter_bz_studies(input_data, varname = "outdoor_HBI", nice_varname = "HBI")

  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$host_sampling %in% c("RO", "RO (shelter)", "RO (pit)")))
  expect_true(all(filtered$host_unit == "HI"))  # AI should be removed
})

test_that("filter_bz_studies returns data.frame", {
  datareq <- data.frame(
    indoor_biting_sampling = c("MBI", "Other"),
    outdoor_biting_sampling = c("MBO", "Other"),
    HLC_same = c(TRUE, FALSE),
    indoor_resting_sampling = c("HRI", "Other"),
    outdoor_resting_sampling = c("WinExit", "Other"),
    WinExit_in_HRI_houses = c("t", "f"),
    host_unit = c("AI", "Other"),
    host_sampling = c("RO", "Other"),
    stringsAsFactors = FALSE
  )

  res <- filter_bz_studies(datareq, varname = "endophagy", nice_varname = "Endophagy")
  expect_s3_class(res, "data.frame")
})

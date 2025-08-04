test_that("prepare_stan_data returns correct structure and values", {
  data_test <- data.frame(
    speciesNb = c(1, 1, 2, 2, 3),
    complexNb = c(1, 1, 2, 2, 1),
    HBI.num = c(5, 3, 10, 4, 7),
    HBI.den = c(10, 5, 20, 10, 15),
    stringsAsFactors = FALSE
  )

  result <- prepare_stan_data(data_test, "HBI")

  expect_type(result, "list")
  expect_named(result, c("N_species_obs", "N_species", "N_complexes",
                         "r_species", "N_species_", "species_id", "species_complex"))
  expect_equal(result$N_species_obs, nrow(data_test))
  expect_equal(result$N_species, max(data_test$speciesNb))
  expect_equal(result$N_complexes, max(data_test$complexNb))
  expect_equal(result$r_species, data_test$HBI.num)
  expect_equal(result$N_species_, data_test$HBI.den)
  expect_equal(result$species_id, data_test$speciesNb)

  expected_complex <- data_test[!duplicated(data_test$speciesNb), ]
  expected_complex <- expected_complex[order(expected_complex$speciesNb), "complexNb"]
  expect_equal(result$species_complex, expected_complex)
})

test_that("prepare_stan_data handles multiple variables correctly", {
  data_test <- data.frame(
    speciesNb = c(1, 2),
    complexNb = c(1, 2),
    endophagy.num = c(2, 3),
    endophagy.den = c(5, 6),
    HBI.num = c(1, 4),
    HBI.den = c(3, 7),
    stringsAsFactors = FALSE
  )

  result_endophagy <- prepare_stan_data(data_test, "endophagy")
  result_HBI <- prepare_stan_data(data_test, "HBI")

  expect_equal(result_endophagy$r_species, data_test$endophagy.num)
  expect_equal(result_endophagy$N_species_, data_test$endophagy.den)
  expect_equal(result_HBI$r_species, data_test$HBI.num)
  expect_equal(result_HBI$N_species_, data_test$HBI.den)
})

test_that("prepare_stan_data handles non-sequential speciesNb and complexNb", {
  data_test <- data.frame(
    speciesNb = c(2, 4, 2, 4),
    complexNb = c(3, 5, 3, 5),
    trait.num = c(1, 2, 3, 4),
    trait.den = c(5, 5, 6, 6),
    stringsAsFactors = FALSE
  )

  result <- prepare_stan_data(data_test, "trait")

  expect_equal(result$N_species_obs, nrow(data_test))
  expect_equal(result$N_species, max(data_test$speciesNb))
  expect_equal(result$N_complexes, max(data_test$complexNb))
  expect_equal(result$r_species, data_test$trait.num)
  expect_equal(result$N_species_, data_test$trait.den)

  expected_complex <- data_test[!duplicated(data_test$speciesNb), ]
  expected_complex <- expected_complex[order(expected_complex$speciesNb), "complexNb"]
  expect_equal(result$species_complex, expected_complex)
})

test_that("prepare_stan_data handles single species and complex", {
  data_test <- data.frame(
    speciesNb = rep(1, 3),
    complexNb = rep(1, 3),
    var.num = c(2, 3, 1),
    var.den = c(5, 6, 3),
    stringsAsFactors = FALSE
  )

  result <- prepare_stan_data(data_test, "var")

  expect_equal(result$N_species_obs, 3)
  expect_equal(result$N_species, 1)
  expect_equal(result$N_complexes, 1)
  expect_equal(result$species_complex, 1)
})

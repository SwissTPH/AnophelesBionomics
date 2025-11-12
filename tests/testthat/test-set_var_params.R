test_that("set_var_params handles all supported variables correctly", {

  repo <- data.frame(
    citation = c("Qui1997", "Marrama2004", "OtherCitation"),
    parity_total = c(100, 200, 150),
    parity_n = c(80, 150, 100),
    parity_percent = c(80, 75, 67),
    indoor_biting_total = c(100, 200, NA),
    indoor_biting_n = c(70, NA, 80),
    indoor_biting = c(0.7, 0.75, 0.8),
    indoor_outdoor_biting_units = c("I:O", "I:O", "I:O"),
    outdoor_biting_sampling = c("MBO", "Other", "MBO"),
    outdoor_biting_n = c(30, 50, 20),
    HLC_same = c(TRUE, FALSE, TRUE),
    resting_unit = c("%", "count", "%"),
    indoor_total = c(100, 0, 50),
    indoor_fed = c(30, 10, 25),
    indoor_unfed = c(20, 5, 15),
    indoor_gravid = c(50, 5, 10),
    outdoor_total = c(100, 50, NA),
    outdoor_fed = c(40, 20, NA),
    outdoor_unfed = c(30, 15, NA),
    outdoor_gravid = c(30, 15, NA),
    parous_with_sac = c(50, 100, 75),
    parous = c(80, 150, 100),
    sac_rate_percent = c(62.5, 66.7, 75),
    indoor_host_sampling = c("HRI", "ILT", "WinExit"),
    indoor_host_n = c(10, 20, 30),
    indoor_host_total = c(50, 100, 150),
    indoor_host = c(20, 20, 20),
    outdoor_host_sampling = c("RO", "RO (shelter)", "RO (pit)"),
    outdoor_host_n = c(5, 15, 25),
    outdoor_host_total = c(20, 50, 100),
    outdoor_host = c(25, 30, 25),
    combined_host_sampling_1 = c("HRI", "RO", ""),
    combined_host_sampling_2 = c("", "RO (shelter)", ""),
    combined_host_sampling_3 = c("", "", ""),
    combined_host_sampling_n = c("t", "t", "t"),
    combined_host_n = c(15, 35, NA),
    combined_host_total = c(30, 70, NA),
    combined_host = c(50, 50, NA),
    stringsAsFactors = FALSE
  )


  # Test parous_rate
  result <- set_var_params("parous_rate", repo)
  expect_equal(result$nice_varname, "Parous rate")
  expect_equal(result$denominator_variables, "parity_total")
  expect_equal(result$numerator_variables, "parity_n")
  expect_equal(result$percentage_variables, "parity_percent")
  expect_true(result$remove_unknown_insecticide)

  # Test endophagy
  result <- set_var_params("endophagy", repo)
  expect_equal(result$nice_varname, "Endophagy")
  expect_equal(result$denominator_variables, "indoor_biting_total")
  expect_equal(result$numerator_variables, "indoor_biting_n")
  expect_equal(result$percentage_variables, "indoor_biting")
  expect_true(result$remove_unknown_insecticide)

  # Test endophily
  result <- set_var_params("endophily", repo)
  expect_equal(result$nice_varname, "Endophily")
  expect_equal(result$denominator_variables, "total_fed")
  expect_equal(result$numerator_variables, "indoor_fed")
  expect_null(result$percentage_variables)
  expect_false(result$remove_unknown_insecticide)

  # Test sac_rate
  result <- set_var_params("sac_rate", repo)
  expect_equal(result$nice_varname, "Sac rate")
  expect_equal(result$denominator_variables, "parous")
  expect_equal(result$numerator_variables, "parous_with_sac")
  expect_equal(result$percentage_variables, "sac_rate_percent")
  expect_false(result$remove_unknown_insecticide)

  # Test indoor_HBI
  result <- set_var_params("indoor_HBI", repo)
  expect_equal(result$nice_varname, "Indoor HBI")
  expect_equal(result$denominator_variables, "host_total")
  expect_equal(result$numerator_variables, "host_n")
  expect_equal(result$percentage_variables, "host_perc")
  expect_true(result$remove_unknown_insecticide)

  # Test outdoor_HBI
  result <- set_var_params("outdoor_HBI", repo)
  expect_equal(result$nice_varname, "Outdoor HBI")

  # Test error for invalid variable
  expect_error(
    set_var_params("invalid_var", repo),
    "Unrecognized variable name. Supported variables: parous_rate, endophagy, endophily, outdoor_HBI, indoor_HBI, HBI, sac_rate."
  )

})



repo_mock <- data.frame(
  citation = c("Qui 1997", "Qui 1997", "Marrama 2004", "Marrama 2004", "Other 2020", "Marrama 2004"),
  outdoor_biting_sampling = c("X", "X", "Y", "Y", "Z", "Z"),
  HLC_same = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  indoor_biting_total = c(NA, NA, NA, 15, 10, NA),
  indoor_biting_n = c(5, 7, NA, 10, 6, NA),
  outdoor_biting_n = c(5, 6, 5, 5, 5, 5),
  indoor_outdoor_biting_units = c("I:O", "I:O", "I:O", "I", "I", "I:O"),
  indoor_biting = c(0.6, 0.8, 0.7, 0.9, 0.8, 0.5),

  resting_unit = c("%", "%", "count", "count", "%", "%"),
  indoor_total = c(0, 0, NA, NA, 100, NA),
  outdoor_total = c(NA, NA, 20, 20, 100, 20),
  indoor_fed = c(10, 20, 15, 16, 50, 12),
  outdoor_fed = c(5, 6, 10, 11, 40, 13),
  indoor_unfed = c(0, 0, 3, 3, 5, 3),
  indoor_gravid = c(0, 0, 2, 2, 5, 2),
  outdoor_unfed = c(0, 0, 4, 4, 5, 4),
  outdoor_gravid = c(0, 0, 3, 3, 5, 3),

  indoor_host_sampling = c("HRI", "ILT", "ILT", "WinExit", NA, "ILT"),
  indoor_host_n = c(10, 20, 20, 15, NA, 5),
  indoor_host_total = c(20, 40, 40, 30, NA, 10),
  indoor_host = c(50, 50, 50, 50, NA, 50),
  outdoor_host_sampling = c("RO", "RO (pit)", "WinExit", "RO (ani-shelter)", NA, "RO"),
  outdoor_host_n = c(30, 30, 40, 50, NA, 15),
  outdoor_host_total = c(60, 60, 80, 100, NA, 30),
  outdoor_host = c(50, 50, 50, 50, NA, 50),

  combined_host_sampling_1 = c("", "HRI", "RO (shelter)", "", "HRI", "RO (ani-shelter)"),
  combined_host_sampling_2 = c("", "", "", "", "", ""),
  combined_host_sampling_3 = c("", "", "", "", "", ""),
  combined_host_sampling_n = c("t", "", "", "t", "", ""),
  combined_host_n = c(NA, 15, 35, NA, 10, 12),
  combined_host_total = c(NA, 30, 70, NA, 20, 24),
  combined_host = c(NA, 50, 50, NA, 50, 50),
  stringsAsFactors = FALSE
)


test_that("endophagy transformations with multiple observations", {
  res <- set_var_params("endophagy", repo_mock)
  new_repo <- res$repo

  expect_true(all(new_repo$outdoor_biting_sampling[grepl("Qui|Marrama", new_repo$citation)] == "MBO"))

  expect_true(all(new_repo$HLC_same[new_repo$citation == "Marrama 2004"] == FALSE))
  expect_true(all(new_repo$HLC_same[new_repo$citation != "Marrama 2004"] == TRUE))

  idx_na <- which(is.na(repo_mock$indoor_biting_total) & repo_mock$indoor_outdoor_biting_units == "I:O")
  expected_totals <- repo_mock$indoor_biting_n[idx_na] + repo_mock$outdoor_biting_n[idx_na]
  expect_equal(new_repo$indoor_biting_total[idx_na], expected_totals)

  idx_na_n <- which(is.na(repo_mock$indoor_biting_n) & !is.na(new_repo$indoor_biting_total) & repo_mock$indoor_outdoor_biting_units == "I:O")
  expected_n <- round(new_repo$indoor_biting_total[idx_na_n] * repo_mock$indoor_biting[idx_na_n] / (1 + repo_mock$indoor_biting[idx_na_n]))
  expect_equal(new_repo$indoor_biting_n[idx_na_n], expected_n)
})

test_that("endophily transformations with multiple observations", {
  res      <- set_var_params("endophily", repo_mock)
  new_repo <- res$repo

  idx0 <- which(repo_mock$resting_unit == "%" & repo_mock$indoor_total == 0)
  expect_true(all(new_repo$indoor_fed[idx0] == 0))

  idx_na_total_in <- which(is.na(repo_mock$indoor_total) &
                             repo_mock$resting_unit == "count")
  expected_indoor_total <- repo_mock$indoor_unfed[idx_na_total_in] +
    repo_mock$indoor_fed[idx_na_total_in]  +
    repo_mock$indoor_gravid[idx_na_total_in]
  expect_equal(new_repo$indoor_total[idx_na_total_in], expected_indoor_total)

  idx_na_total_out <- which(is.na(repo_mock$outdoor_total) &
                              repo_mock$resting_unit == "count")
  expected_outdoor_total <- repo_mock$outdoor_unfed[idx_na_total_out] +
    repo_mock$outdoor_fed[idx_na_total_out]  +
    repo_mock$outdoor_gravid[idx_na_total_out]
  expect_equal(new_repo$outdoor_total[idx_na_total_out], expected_outdoor_total)

  idx_pct <- which(repo_mock$resting_unit == "%")
  expected_counts_in  <- round(repo_mock$indoor_fed[idx_pct]  / 100 *
                                 new_repo$indoor_total[idx_pct])
  expected_counts_out <- round(repo_mock$outdoor_fed[idx_pct] / 100 *
                                 new_repo$outdoor_total[idx_pct])

  expect_equal(new_repo$indoor_fed[idx_pct],  expected_counts_in)
  expect_equal(new_repo$outdoor_fed[idx_pct], expected_counts_out)

  expect_equal(new_repo$total_fed,
               new_repo$indoor_fed + new_repo$outdoor_fed)
})

test_that("indoor_HBI transformation extracts correct indoor observations", {
  res <- set_var_params("indoor_HBI", repo_mock)
  new_repo <- res$repo

  expect_true(all(new_repo$location == "indoors"))

  r1 <- new_repo %>% dplyr::filter(citation == "Qui 1997", host_sampling == "HRI")
  expect_true(any(r1$host_n == 10))
  expect_true(any(r1$host_total == 20))
  expect_true(any(r1$host_perc == 50))

  r2 <- new_repo %>% dplyr::filter(citation == "Qui 1997", host_sampling == "ILT")
  expect_true(any(r2$host_n == 20))
  expect_true(any(r2$host_total == 40))

  r4 <- new_repo %>% dplyr::filter(citation == "Marrama 2004", host_sampling == "WinExit", location == "indoors")
  expect_true(any(r4$host_n == 15))

  r5 <- new_repo %>% dplyr::filter(citation == "Other 2020", host_sampling == "HRI")
  expect_true(any(r5$host_n == 10))
  expect_true(any(r5$host_total == 20))
})

test_that("outdoor_HBI transformation extracts correct outdoor observations", {
  res <- set_var_params("outdoor_HBI", repo_mock)
  new_repo <- res$repo

  expect_true(all(new_repo$location == "outdoors"))

  r1 <- new_repo %>% dplyr::filter(citation == "Qui 1997", host_sampling == "RO")
  expect_true(any(r1$host_n == 30))
  expect_true(any(r1$host_total == 60))

  r2 <- new_repo %>% dplyr::filter(citation == "Qui 1997", host_sampling == "RO (pit)")
  expect_true(any(r2$host_n == 30))

  r3 <- new_repo %>% dplyr::filter(citation == "Marrama 2004", host_sampling == "RO (shelter)")
  expect_true(any(r3$host_n == 35))
  expect_true(any(r3$host_total == 70))

  r6 <- new_repo %>% dplyr::filter(citation == "Marrama 2004", host_sampling == "RO (ani-shelter)")
  expect_true(any(r6$host_n == 12))
  expect_true(any(r6$host_total == 24))
})




test_that("indoor_HBI transformation dplyr::filters correctly and renames", {
  res <- set_var_params("indoor_HBI", repo_mock)
  new_repo <- res$repo

  expect_true(all(new_repo$location == "indoors"))
  expect_true(all(new_repo$host_sampling %in% c("HRI", "ILT", "WinExit")))
  expect_false(any(grepl("outdoor_host", colnames(new_repo))))
  expect_false(any(grepl("combined_host_sampling_2|combined_host_sampling_3", colnames(new_repo))))
})

test_that("outdoor_HBI transformation dplyr::filters correctly and renames", {
  res <- set_var_params("outdoor_HBI", repo_mock)
  new_repo <- res$repo

  expect_true(all(new_repo$location == "outdoors"))
  expect_true(all(new_repo$host_sampling %in% c("RO", "RO (shelter)", "RO (pit)", "RO (ani-shelter)", "WinExit")))
  expect_false(any(grepl("indoor_host", colnames(new_repo))))
})





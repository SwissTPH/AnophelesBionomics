test_that("create_repo() returns a data.frame", {
  repo <- create_repo()
  expect_s3_class(repo, "data.frame")
})

test_that("create_repo() output contains required columns", {
  repo <- create_repo()
  expected_cols <- c("species", "source_id", "citation", "pubmed_id", "country", "taxon")
  expect_true(all(expected_cols %in% names(repo)))
})

test_that("create_repo() doesn't create a massive number of duplicated rows", {
  repo <- create_repo()
  dupes <- duplicated(repo)
  expect_lt(sum(dupes), nrow(repo) * 0.1)
})

test_that("create_repo() contains multiple species and countries", {
  repo <- create_repo()
  expect_gt(length(unique(repo$species)), 5)
  expect_gt(length(unique(repo$country)), 2)
})

test_that("left join with endophily data does not reduce number of rows", {
  base_repo <- create_repo()
  endo_data <- read_data_file("endophily_data_WinExit_HRI.csv", sep = ";")
  required_cols(endo_data, c("source_id", "citation", "pubmed_id"), "endophily_data_WinExit_HRI.csv")

  joined_repo <- dplyr::left_join(base_repo, endo_data, by = c("source_id", "citation", "pubmed_id"))
  expect_gte(nrow(joined_repo), nrow(base_repo))
})


# Sample repository data
repo <- data.frame(
  country = c("The Gambia", "Tanzania (United Republic of)", "Venezuela", "Philippines", "Brazil"),
  year_start = c(2000, 1995, 2010, 2020, 2005),
  stringsAsFactors = FALSE
)

test_that("Function filters correctly by region only", {
  result <- region_period_filter(repo, geo = c("Africa-E", "Americas"))
  expect_true(all(result$region %in% c("Africa-E", "Americas")))
  expect_equal(nrow(result), 3)  # Kenya, USA, Brazil
})

test_that("Function filters correctly by year only", {
  result <- region_period_filter(repo, geo = c("Europe", "Africa-E", "Americas", "Asia-Pacific"),
                                 year_min = 2005, year_max = 2020)
  expect_true(all(result$year_start >= 2005 & result$year_start <= 2020))
  expect_equal(nrow(result), 3)
})

test_that("Function filters correctly by region and year", {
  result <- region_period_filter(repo, geo = c("Americas"), year_min = 2006, year_max = 2020)
  expect_equal(nrow(result), 1)  # Only USA
  expect_equal(result$country, "Venezuela")
})

test_that("Function handles missing year filters correctly", {
  result <- region_period_filter(repo, geo = c("Asia-Pacific"))
  expect_equal(nrow(result), 1)
  expect_equal(result$country, "Philippines")
})

test_that("Function returns empty tibble if no match", {
  result <- region_period_filter(repo, geo = c("Africa-W"), year_min = 2100)
  expect_equal(nrow(result), 0)
})

test_that("Function handles NA in geo vector", {
  result <- region_period_filter(repo, geo = c(NA))
  expect_true(all(is.na(result$region)))
})

test_that("region_period_filter returns data.frame with expected columns", {

  res <- region_period_filter(repo)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("country", "region", "year_start") %in% colnames(res)))
})

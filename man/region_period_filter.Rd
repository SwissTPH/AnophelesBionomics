% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{region_period_filter}
\alias{region_period_filter}
\title{Filter Dataset by Geographic Region and Time Period}
\usage{
region_period_filter(
  repo,
  geo = c("Africa-E", "Africa-W", "Americas", "Asia-Pacific", NA),
  year_min = -Inf,
  year_max = +Inf
)
}
\arguments{
\item{repo}{A `data.frame` or tibble containing the input dataset. Must include a `country` column.}

\item{geo}{Character vector of region names to retain. Defaults to common WHO-defined macro-regions:
`"Africa-E"`, `"Africa-W"`, `"Americas"`, `"Asia-Pacific"`, and `NA`.}

\item{year_min}{Integer or numeric. Minimum year to include (inclusive). Defaults to `-Inf` (no lower bound).}

\item{year_max}{Integer or numeric. Maximum year to include (inclusive). Defaults to `Inf` (no upper bound).}
}
\value{
A filtered dataframe including only rows matching the selected region(s) and within the defined time period.
}
\description{
Enhances and filters a repository dataset based on specified geographic regions
and a defined time window. Regional information is added by merging with an
external country-to-region mapping file.
}
\details{
The function uses a file named `"countries_continent_region.csv"` (semicolon-separated)
  which must include at least two columns: `country` and `region`.
}

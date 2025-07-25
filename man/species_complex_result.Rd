% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{species_complex_result}
\alias{species_complex_result}
\title{Extract Summary Statistics from Stan Model Results}
\usage{
species_complex_result(results, ci_level = 0.95, all = TRUE, output_dir = NULL)
}
\arguments{
\item{results}{A list containing the Stan fit results and species metadata. Must include:
- `fit`: The fitted Stan model object (class `stanfit`),
- `species_complex`: A data frame linking species and complexes with indices.}

\item{ci_level}{Numeric value between 0 and 1 specifying the confidence interval level (default is 0.95).}

\item{all}{Logical indicating whether to include all species and complexes from an external compatibility repository (default is TRUE).}
}
\value{
A data.frame with summarized posterior statistics including:
  - `name`: Taxonomic unit name (genus, complex, or species),
  - `level`: Taxonomic level ("genus", "complex", or "species"),
  - `estimate_ci`: Formatted estimate with confidence interval as a percentage string,
  - `variance`: Variance of the estimate,
  - `ci_lower`: Lower bound of the confidence interval,
  - `ci_upper`: Upper bound of the confidence interval,
  - additional metadata columns when `all = TRUE`.
}
\description{
This function extracts and summarizes posterior estimates from a fitted Stan model,
organizing the results by taxonomic levels: genus, species complex, and species.
It calculates means, variances, and confidence intervals, formats estimates with confidence intervals,
and optionally includes all species and complexes from a compatibility repository.
The output is a tidy data.frame suitable for further analysis or reporting.
}
\details{
The function first extracts summary statistics from the Stan fit object, including means,
standard deviations, and confidence intervals for parameters representing genus, complex, and species levels.
It then maps these parameters to species and complex names using the `species_complex` data frame.
Unlabelled species are handled according to their complex membership.
When `all = TRUE`, missing species and complexes are supplemented based on a compatibility repository to ensure completeness.
}
\examples{
\dontrun{
# Assuming `stan_results` is a list with the required components:
summary_df <- extract_stan_summary(
  results = stan_results,
  ci_level = 0.95,
  all = TRUE
)
}

}

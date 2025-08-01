% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{create_datareq}
\alias{create_datareq}
\title{Filter and Prepare Dataset for Proportion-Based Analysis}
\usage{
create_datareq(data.req, varname, remove_unknown_insecticide = TRUE)
}
\arguments{
\item{data.req}{A `data.frame` containing the dataset.}

\item{varname}{Character string; prefix used to identify the computed proportion columns (e.g., `"endophily"`, `"parous_rate"`).}

\item{remove_unknown_insecticide}{Logical; if `TRUE` (default), removes rows where `insecticide_control` is `"t"` or unknown.}
}
\value{
A filtered and cleaned dataframe, excluding rows with missing denominators and (optionally) unknown insecticide interventions.
}
\description{
Prepares a dataset for proportion-based analysis by:
\itemize{
  \item Removing rows with unknown insecticide intervention (if requested),
  \item Renaming and standardizing column names,
  \item Filtering out rows with missing numerators or denominators,
  \item Warning when the numerator exceeds the denominator.
}
}
\details{
This function assumes that proportions have already been computed using
\code{\link{augment_withProportion_modif}}, resulting in columns like `<varname>.num` and `<varname>.den`.


Additional processing steps:
\itemize{
  \item The `species` column is renamed to `survey`.
  \item Whitespace is removed from `insecticide_control` to create a new column `intervened`.
  \item A warning is issued if any rows have `varname.num > varname.den`.
  \item The total number of valid observations is printed.
}
}

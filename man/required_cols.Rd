% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary_functions.R
\name{required_cols}
\alias{required_cols}
\title{Ensure Required Columns Are Present in a Data Frame}
\usage{
required_cols(df, expected, name)
}
\arguments{
\item{df}{A data frame to be checked.}

\item{expected}{A character vector of expected column names.}

\item{name}{A character string indicating the name or source of the data frame (used in the error message).}
}
\value{
The function does not return anything. It throws an error if required columns are missing.
}
\description{
This utility function checks whether all expected columns are present in a given data frame.
It is particularly useful for validating user input or internal data structures within a package,
helping to prevent downstream errors caused by missing variables.
}
\examples{
required_cols(data.frame(a = 1, b = 2), c("a", "b"), "example_df") # OK
\dontrun{
required_cols(data.frame(a = 1), c("a", "b"), "example_df") # Error
}

}

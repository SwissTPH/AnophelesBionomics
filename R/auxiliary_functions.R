#' Read a CSV File from the Package's Internal Data Directory
#'
#' This function reads a CSV or similarly formatted data file located in the package's
#' `inst/extdata/` directory. It safely constructs the file path and returns the content
#' as a data frame.
#'
#' @param filename Character string. The name of the file to read, including extension (e.g., `"mydata.csv"`).
#' @param sep Character string. The field separator used in the file. Defaults to `","`.
#'
#' @return A data frame containing the data read from the specified file.
#'
#' @details
#' This function is useful for accessing example datasets or reference tables shipped within
#' the package without requiring users to manually specify full file paths.
#' It verifies the file exists inside the package before attempting to read it.
#'
#' @examples
#' \dontrun{
#' df <- read_data_file("countries_continent_region.csv", sep = ";")
#' }
read_data_file <- function(filename, sep = ",") {
  stopifnot(is.character(filename), length(filename) == 1)
  stopifnot(is.character(sep), length(sep) == 1)

  path <- system.file("extdata", filename, package = "AnophelesBionomics")
  if (path == "") {
    stop(sprintf("The file '%s' was not found in the package.", filename), call. = FALSE)
  }

  data <- read.table(file = path,
                     header = TRUE,
                     sep = sep,
                     stringsAsFactors = FALSE,
                     quote = "\"",
                     fill = TRUE,
                     comment.char = "")

  return(data)
}


#' Ensure Required Columns Are Present in a Data Frame
#'
#' This utility function checks whether all expected columns are present in a given data frame.
#' It is particularly useful for validating user input or internal data structures within a package,
#' helping to prevent downstream errors caused by missing variables.
#'
#' @param df A data frame to be checked.
#' @param expected A character vector of expected column names.
#' @param name A character string indicating the name or source of the data frame (used in the error message).
#'
#' @return The function does not return anything. It throws an error if required columns are missing.
#'
#' @examples
#' required_cols(data.frame(a = 1, b = 2), c("a", "b"), "example_df") # OK
#' \dontrun{
#' required_cols(data.frame(a = 1), c("a", "b"), "example_df") # Error
#' }
required_cols <- function(df, expected, name) {
  missing <- setdiff(expected, colnames(df))
  if (length(missing) > 0) {
    stop(paste0("Missing required columns in ", name, ": ", paste(missing, collapse = ", ")))
  }
}



























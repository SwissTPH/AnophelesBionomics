#' Create and Load the Combined Repository Dataset
#'
#' Loads, merges, and enriches multiple bionomics datasets into a single comprehensive repository.
#' This function consolidates regional datasets, maps species to taxonomic information, integrates
#' additional data sources (e.g., endophily, supplemental studies, and sac rate data), and removes
#' entries lacking taxonomic classification. Stops execution if required columns are missing.
#'
#' @return A `data.frame` containing the harmonized bionomics dataset from multiple sources.
create_repo <- function() {

  files <- c(
    "Bionomics_Africa.csv",
    "Bionomics_Americas.csv",
    "Bionomics_Asia-Pacific.csv",
    "Bionomics_Asia-Pacific_bz_20200516.csv"
  )

  data_list <- lapply(files, function(file) {
    df <- read_data_file(file)
    required_cols(df,
                  c("species", "source_id", "citation", "pubmed_id", "country"),
                  file)
    return(df)
  })

  repo <- do.call(rbind, data_list)

  species_mapping <- read_data_file("species_mapping.csv", sep = ",")
  required_cols(species_mapping, c("species", "Taxon_no"), "species_mapping.csv")

  taxonomy <- read_data_file("taxonomy.csv", sep = ";")
  required_cols(taxonomy, c("Taxon_no", "taxon"), "taxonomy.csv")

  repo$Taxon_no <- species_mapping$Taxon_no[match(repo$species, species_mapping$species)]
  repo$taxon <- taxonomy$taxon[match(repo$Taxon_no, taxonomy$Taxon_no)]
  repo$taxon[repo$species == "Homo sapiens"] <- "Homo sapiens"
  repo <- repo[!is.na(repo$taxon), ]

  endophily_papers <- read_data_file("endophily_data_WinExit_HRI.csv", sep = ";")
  required_cols(endophily_papers, c("source_id", "citation", "pubmed_id"), "endophily_data_WinExit_HRI.csv")

  repo <- dplyr::left_join(repo, endophily_papers, by = c("source_id", "citation", "pubmed_id"))

  additional_Briet_data <- read_data_file("additional_bionomics_Briet_2019.csv", sep = ";")
  required_cols(additional_Briet_data, c("species"), "additional_bionomics_Briet_2019.csv")

  sac_rate_data <- read_data_file("sac_rate_numbers.csv", sep = ";")
  required_cols(sac_rate_data, c("species"), "sac_rate_numbers.csv")

  repo <- dplyr::bind_rows(repo, additional_Briet_data)
  repo <- dplyr::bind_rows(repo, sac_rate_data)

  return(repo)
}


#' Add External Data to a Repository with Validation
#'
#' This function checks whether external data includes the required columns
#' for a specific indicator type (`varname`) and appends it to an existing
#' dataset repository (`repo`) if validation passes.
#'
#' @param varname A character string indicating the type of indicator to validate.
#'        Supported values include: `"parous_rate"`, `"endophagy"`, `"endophily"`,
#'        `"sac_rate"`, `"indoor_HBI"`, `"outdoor_HBI"`, and `"resting_duration"`.
#' @param repo A data frame representing the current dataset repository to which
#'        external data will be appended.
#' @param extern_data A data frame containing the external data to validate and append.
#'        This data must contain all required columns for the specified `varname`.
#'
#' @return A data frame combining the original `repo` and the validated `extern_data`.
#'         If required columns are missing, the function stops with an informative error.
#'
#' @details
#' Each `varname` has its own set of required columns in addition to core fields
#' like `species`, `insecticide_control`, `country`, and `year_start`.
#' both `"HBI_indoor"` and `"HBI_outdoor"`.
#'
#' @examples
#' repo <- data.frame(
#'   species = character(),
#'   insecticide_control = character(),
#'   country = character(),
#'   year_start = integer(),
#'   parity_n = integer(),
#'   parity_total = integer(),
#'   parity_percent = numeric(),
#'   stringsAsFactors = FALSE
#' )
#'
#' extern_data <- data.frame(
#'   species = c("Anopheles arabiensis", "Anopheles arabiensis"),
#'   insecticide_control = c("f", "f"),
#'   country = c("Kenya", "Ethiopia"),
#'   year_start = c(2021, 2022),
#'   parity_n = c(45, 30),
#'   parity_total = c(100, 60),
#'   parity_percent = c(45, 50),
#'   stringsAsFactors = FALSE
#' )
#'
#' repo <- adding_data_extern("parous_rate", repo, extern_data)
#'
adding_data_extern <- function(varname, repo, extern_data) {
  base_required <- c("species", "insecticide_control", "country", "year_start")

  rules <- list(
    parous_rate = c("parity_n", "parity_total", "parity_percent", "insecticide_control"),
    endophagy = c("indoor_biting_n", "indoor_biting_total", "indoor_biting",
                  "indoor_biting_sampling", "outdoor_biting_sampling",
                  "HLC_same", "insecticide_control"),
    endophily = c("indoor_fed", "outdoor_fed", "resting_unit",
                  "indoor_resting_sampling", "outdoor_resting_sampling",
                  "WinExit_in_HRI_houses", "insecticide_control"),
    sac_rate = c("parous_with_sac", "parous", "sac_rate_percent", "insecticide_control"),
    indoor_HBI = c("indoor_host_sampling", "indoor_host_n", "indoor_host_total",
                   "indoor_host", "host_unit", "insecticide_control"),
    outdoor_HBI = c("outdoor_host_sampling", "outdoor_host_n", "outdoor_host_total",
                    "outdoor_host", "host_unit", "insecticide_control"),
    HBI = c(
      "combined_host_sampling_1", "combined_host_n", "combined_host_total",
      "combined_host", "combined_host_sampling_n", "insecticide_control",
      "indoor_host_sampling", "indoor_host_n", "indoor_host_total",
      "indoor_host", "host_unit",
      "outdoor_host_sampling", "outdoor_host_n", "outdoor_host_total",
      "outdoor_host"
    )
  )

  if (!varname %in% names(rules)) {
    warning(paste("Unknown indicator type:", varname, "- no data added."))
    return(repo)
  }

  required_cols <- unique(c(base_required, rules[[varname]]))

  missing_cols <- setdiff(required_cols, colnames(extern_data))
  if (length(missing_cols) > 0) {
    message(paste0(
      "extern_data is missing required columns for '", varname, "'.\n",
      "Missing columns: ", paste(missing_cols, collapse = ", "), "\n",
    ))
  }

  repo <- dplyr::bind_rows(repo, extern_data)

  return(repo)

}


#' Set Parameters and Transform Data for a Given Variable
#'
#' @description
#' Configures variable-specific parameters and applies necessary transformations to the dataset
#' based on the variable of interest. This includes defining numerator/denominator columns,
#' computing derived metrics when needed, filtering, and restructuring the repository.
#'
#' @param varname A string specifying the variable to analyze.
#'   Supported values: `"parous_rate"`, `"endophagy"`, `"endophily"`, `"indoor_HBI"`, `"outdoor_HBI"`, `"sac_rate"`, `"HBI"`.
#' @param repo A `data.frame` containing the repository data to process.
#'
#' @return A named list containing:
#' \describe{
#'   \item{nice_varname}{Human-readable name for the variable}
#'   \item{denominator_variables}{Name of the denominator column}
#'   \item{numerator_variables}{Name of the numerator column}
#'   \item{percentage_variables}{Name of the percentage column (or \code{NULL} if not applicable)}
#'   \item{remove_unknown_insecticide}{Logical flag indicating whether to exclude unknown insecticide data}
#'   \item{repo}{(Optional) Transformed repository \code{data.frame} for HBI and other derived variables}
#' }
#'
set_var_params <- function(varname, repo) {

  if (varname == "parous_rate") {
    nice_varname <- "Parous rate"
    denominator_variables <- "parity_total"
    numerator_variables <- "parity_n"
    percentage_variables <- "parity_percent"
    remove_unknown_insecticide <- TRUE

  } else if (varname == "endophagy") {
    nice_varname <- "Endophagy"
    denominator_variables <- "indoor_biting_total"
    numerator_variables <- "indoor_biting_n"
    percentage_variables <- "indoor_biting"
    remove_unknown_insecticide <- TRUE

    repo <- dplyr::mutate(repo,
                          outdoor_biting_sampling = ifelse((grepl("Qui", citation) & grepl("1997", citation)) |
                                                             (grepl("Marrama", citation) & grepl("2004", citation)),
                                                           "MBO", outdoor_biting_sampling),
                          HLC_same = ifelse(grepl("Marrama", citation) & grepl("2004", citation), FALSE, TRUE),
                          indoor_biting_total = ifelse(is.na(indoor_biting_total) & indoor_outdoor_biting_units == "I:O",
                                                       indoor_biting_n + outdoor_biting_n,
                                                       indoor_biting_total),
                          indoor_biting_n = ifelse(is.na(indoor_biting_n) & !is.na(indoor_biting_total) & indoor_outdoor_biting_units == "I:O",
                                                   round((indoor_biting_total * indoor_biting) / (1 + indoor_biting), digits = 0),
                                                   indoor_biting_n)
    )

  } else if (varname == "endophily") {
    nice_varname <- "Endophily"
    denominator_variables <- "total_fed"
    numerator_variables <- "indoor_fed"
    percentage_variables <- NULL
    remove_unknown_insecticide <- FALSE

    repo <- dplyr::mutate(repo,
                          indoor_fed = ifelse(resting_unit == "%" & indoor_total == 0, 0, indoor_fed),
                          indoor_total = ifelse(is.na(indoor_total) & resting_unit == "count",
                                                indoor_unfed + indoor_fed + indoor_gravid, indoor_total),
                          outdoor_total = ifelse(is.na(outdoor_total) & resting_unit == "count",
                                                 outdoor_unfed + outdoor_fed + outdoor_gravid, outdoor_total),
                          indoor_fed = ifelse(resting_unit == "%", round(indoor_fed / 100 * indoor_total), indoor_fed),
                          outdoor_fed = ifelse(resting_unit == "%", round(outdoor_fed / 100 * outdoor_total), outdoor_fed),
                          total_fed = indoor_fed + outdoor_fed
    )

  } else if (varname == "sac_rate") {
    nice_varname <- "Sac rate"
    varname <- "sac_rate"
    numerator_variables <- "parous_with_sac"
    denominator_variables <- "parous"
    percentage_variables <- "sac_rate_percent"
    remove_unknown_insecticide <- FALSE

  } else if (grepl("HBI", varname)) {
    denominator_variables <- "host_total"
    numerator_variables <- "host_n"
    percentage_variables <- "host_perc"
    remove_unknown_insecticide <- TRUE

    indoor_methods <- c("HRI", "ILT", "WinExit")
    outdoor_methods <- c("RO", "RO (shelter)", "RO (pit)", "RO (ani-shelter)")

    HBI_indoors <- repo |>
      dplyr::filter(indoor_host_sampling %in% indoor_methods) |>
      dplyr::select(-c(outdoor_host_sampling, outdoor_host_n, outdoor_host_total, outdoor_host,
                       paste0("combined_host_sampling_", 1:3), combined_host_sampling_n,
                       combined_host_n, combined_host_total, combined_host)) |>
      dplyr::rename(
        host_sampling = indoor_host_sampling,
        host_n = indoor_host_n,
        host_total = indoor_host_total,
        host_perc = indoor_host
      ) |>
      dplyr::mutate(location = "indoors")

    HBI_outdoors <- repo |>
      dplyr::filter(outdoor_host_sampling %in% c(outdoor_methods, "WinExit")) |>
      dplyr::select(-c(indoor_host_sampling, indoor_host_n, indoor_host_total, indoor_host,
                       paste0("combined_host_sampling_", 1:3), combined_host_sampling_n,
                       combined_host_n, combined_host_total, combined_host)) |>
      dplyr::rename(
        host_sampling = outdoor_host_sampling,
        host_n = outdoor_host_n,
        host_total = outdoor_host_total,
        host_perc = outdoor_host
      ) |>
      dplyr::mutate(location = ifelse(host_sampling == "WinExit", "indoors", "outdoors"))

    HBI_others <- repo |>
      dplyr::filter(combined_host_sampling_1 != "" & combined_host_sampling_n != "t") |>
      dplyr::select(-c(indoor_host_sampling, indoor_host_n, indoor_host_total, indoor_host,
                       outdoor_host_sampling, outdoor_host_n, outdoor_host_total, outdoor_host,
                       combined_host_sampling_n)) |>
      dplyr::filter(dplyr::if_all(dplyr::contains("combined_host_sampling_"),
                                  ~ . %in% c(indoor_methods, outdoor_methods, "")))
      HBI_others <- dplyr::union(
      dplyr::filter(dplyr::filter(HBI_others,
                                  dplyr::if_all(dplyr::contains("combined_host_sampling_"),
                                                ~ . %in% c(indoor_methods, ""))),
      ),
      dplyr::union(
        dplyr::filter(HBI_others,
                      dplyr::if_all(dplyr::contains("combined_host_sampling_"),
                                    ~ . %in% c(outdoor_methods[outdoor_methods != "RO (ani-shelter)"], ""))),
        dplyr::filter(HBI_others,
                      dplyr::if_all(dplyr::contains("combined_host_sampling_"),
                                    ~ . %in% c("RO (ani-shelter)", "")))
      )
    ) |>
      dplyr::mutate(location = ifelse(combined_host_sampling_1 %in% indoor_methods, "indoors", "outdoors")) |>
      dplyr::select(-paste0("combined_host_sampling_", 2:3)) |>
      dplyr::rename(
        host_sampling = combined_host_sampling_1,
        host_n = combined_host_n,
        host_total = combined_host_total,
        host_perc = combined_host
      )

    repo <- dplyr::distinct(rbind(HBI_indoors, rbind(HBI_outdoors, HBI_others)))


    if (varname == "HBI") {
      nice_varname <- "HBI"
    } else if (varname == "indoor_HBI") {
      nice_varname <- "Indoor HBI"
      repo <- dplyr::filter(repo, location == "indoors")
    } else if (varname == "outdoor_HBI") {
      nice_varname <- "Outdoor HBI"
      repo <- dplyr::filter(repo, location == "outdoors")
    } else {
      stop("Unrecognized variable name. Supported variables: parous_rate, endophagy, endophily, outdoor_HBI, indoor_HBI, HBI, sac_rate.")
    }
  } else if (varname == "resting_duration") {


    fed_gravid_indoors <- repo %>% filter(!(resting_unit %in% c("", "per man hour", "Per man hour")) & indoor_resting_sampling != "") %>%
      select(-c(outdoor_resting_sampling:other_total)) %>%
      rename_with(~gsub("indoor_", "", .), indoor_resting_sampling:indoor_total) %>%
      mutate(location = "indoors")

    fed_gravid_outdoors <- repo %>% filter(!(resting_unit %in% c("", "per man hour", "Per man hour")) & outdoor_resting_sampling != "") %>%
      select(-c(indoor_resting_sampling:indoor_total, other_resting_sampling:other_total)) %>%
      rename_with(~gsub("outdoor_", "", .), outdoor_resting_sampling:outdoor_total) %>%
      mutate(location = ifelse(resting_sampling == "WinExit", "indoors", "outdoors"))

    fed_gravid_others <- repo %>% filter(!(resting_unit %in% c("", "per man hour", "Per man hour")) & other_resting_sampling != "") %>%
      select(-c(indoor_resting_sampling:outdoor_total)) %>%
      rename_with(~gsub("other_", "", .), other_resting_sampling:other_total) %>%
      mutate(location = ifelse(resting_sampling %in% c("HRI", "WinExit"), "indoors",
                               ifelse(grepl("RO", resting_sampling), "outdoors", "other")))

    repo <- rbind(rbind(fed_gravid_indoors, fed_gravid_outdoors), fed_gravid_others) %>% distinct() %>%
      mutate(resting_duration = ifelse(resting_unit == "fed:gravid",
                                       ifelse(fed == 1, 1/gravid + 1, 1/fed + 1),
                                       gravid/fed + 1))

    create_datareq_rest <- function(repo, observation_variables, reqP = NULL){
      repo$intervened = with(repo, ifelse(insecticide_control == 'f ', 2, as.numeric(as.factor(insecticide_control))))
      data.req <- repo %>% filter(!is.na(get(observation_variables)) & !is.infinite(get(observation_variables)))

      data.req <- data.req %>% filter(intervened == 2)

      return(data.req)
    }

    observation_variables <- "resting_duration"
    repo = create_datareq_rest(repo, observation_variables)


    repo$deno_rest <- as.integer(repo$fed + repo$gravid)
    repo$fed <- as.integer(repo$fed)

    repo <- repo %>% select(-resting_duration)
    repo <- repo %>% select(-total)

    nice_varname <- "Resting duration from fed/gravid ratio"
    varname <- "resting_duration"
    numerator_variables <- "fed"
    denominator_variables <- "deno_rest"
    percentage_variables <- "unfed"
    remove_unknown_insecticide <- FALSE
  } else {
     stop("Unrecognized variable name. Supported variables: parous_rate, endophagy, endophily, outdoor_HBI, indoor_HBI, HBI, sac_rate, resting_duration.")
  }

  result <- list(
    repo = repo,
    nice_varname = nice_varname,
    denominator_variables = denominator_variables,
    numerator_variables = numerator_variables,
    percentage_variables = percentage_variables,
    remove_unknown_insecticide = remove_unknown_insecticide
  )

  return(result)
}


#' Filter Dataset by Geographic Region and Time Period
#'
#' Enhances and filters a repository dataset based on specified geographic regions
#' and a defined time window. Regional information is added by merging with an
#' external country-to-region mapping file.
#'
#' @param repo A `data.frame` or tibble containing the input dataset. Must include a `country` column.
#' @param geo Character vector of region names to retain. Defaults to common WHO-defined macro-regions:
#'   `"Africa-E"`, `"Africa-W"`, `"Americas"`, `"Asia-Pacific"`, and `NA`.
#' @param year_min Integer or numeric. Minimum year to include (inclusive). Defaults to `-Inf` (no lower bound).
#' @param year_max Integer or numeric. Maximum year to include (inclusive). Defaults to `Inf` (no upper bound).
#'
#' @return A filtered dataframe including only rows matching the selected region(s) and within the defined time period.
#'
#' @details The function uses a file named `"countries_continent_region.csv"` (semicolon-separated)
#'   which must include at least two columns: `country` and `region`.
#'
#' @export
region_period_filter <- function(repo, geo =  c("Africa-E", "Africa-W", "Americas", "Asia-Pacific", NA), year_min = -Inf, year_max = +Inf) {

  countries_region <- read_data_file("countries_continent_region.csv", sep = ";")
  required_cols(countries_region, c("country", "region"), "countries_continent_region.csv")

  repo <- dplyr::left_join(repo, countries_region, by = "country")

  if (year_min == -Inf && year_max == Inf) {
    filtered <- repo |> dplyr::filter(region %in% geo)
  } else {
    filtered <- repo |>
      dplyr::filter(year_start >= year_min, year_start <= year_max, region %in% geo)
  }

  return(filtered)
}


#' Add Aggregated Numerator and Denominator Columns Based on Selected Variables
#'
#' Computes and appends row-wise total denominators and numerators from specified
#' variables. Optionally estimates missing numerators using available percentage values.
#'
#' @param df A `data.frame` or tibble containing the dataset.
#' @param varname A string used as prefix to name the new columns (e.g., `HBI.num`, `HBI.den`).
#' @param denominator_variables Character vector specifying column(s) to sum for the denominator.
#' @param numerator_variables Character vector specifying column(s) to sum for the numerator.
#' @param percentage_variables Optional character vector of percentage column(s). When provided,
#'   missing numerator values are estimated using: \code{numerator = denominator * percentage / 100}.
#'
#' @return A dataframe identical to `df` but with two additional columns:
#' \describe{
#'   \item{\code{<varname>.num}}{Total numerator, estimated using percentages if necessary.}
#'   \item{\code{<varname>.den}}{Total denominator. Zero values are treated as `NA`.}
#' }
augment_withProportion_modif <- function(df, varname, denominator_variables, numerator_variables, percentage_variables = NULL) {
  total <- rowSums(subset(df, select = denominator_variables), na.rm = FALSE)
  total[total == 0] <- NA

  num <- rowSums(subset(df, select = numerator_variables), na.rm = FALSE)
  if (varname != "resting_duration") {
    if (!is.null(percentage_variables)) {
      perc <- rowMeans(subset(df, select = percentage_variables), na.rm = FALSE)
      to_overwrite <- with(df, is.na(num) & !is.na(total * perc))
      num[to_overwrite] <- round(total[to_overwrite] * perc[to_overwrite] / 100)
      }
    }

  augmented_repo <- cbind(df, num, total)
  names(augmented_repo)[names(augmented_repo) == 'num'] <- paste(varname, "num", sep = ".")
  names(augmented_repo)[names(augmented_repo) == 'total'] <- paste(varname, "den", sep = ".")

  return(augmented_repo)
}


#' Filter and Prepare Dataset for Proportion-Based Analysis
#'
#' Prepares a dataset for proportion-based analysis by:
#' \itemize{
#'   \item Removing rows with unknown insecticide intervention (if requested),
#'   \item Renaming and standardizing column names,
#'   \item Filtering out rows with missing numerators or denominators,
#'   \item Warning when the numerator exceeds the denominator.
#' }
#'
#' This function assumes that proportions have already been computed using
#' \code{\link{augment_withProportion_modif}}, resulting in columns like `<varname>.num` and `<varname>.den`.
#'
#' @param data.req A `data.frame` containing the dataset.
#' @param varname Character string; prefix used to identify the computed proportion columns (e.g., `"endophily"`, `"parous_rate"`).
#' @param remove_unknown_insecticide Logical; if `TRUE` (default), removes rows where `insecticide_control` is `"t"` or unknown.
#'
#' @return A filtered and cleaned dataframe, excluding rows with missing denominators and (optionally) unknown insecticide interventions.
#'
#' @details
#' Additional processing steps:
#' \itemize{
#'   \item The `species` column is renamed to `survey`.
#'   \item Whitespace is removed from `insecticide_control` to create a new column `intervened`.
#'   \item A warning is issued if any rows have `varname.num > varname.den`.
#'   \item The total number of valid observations is printed.
#' }
create_datareq <- function(data.req,
                           varname,
                           remove_unknown_insecticide = TRUE) {

  data.req$intervened <- gsub(" ", "", data.req$insecticide_control)

  colnames(data.req)[which(colnames(data.req) == "species")] <- "survey"

  if (varname != "resting_duration") {
    if (remove_unknown_insecticide) {
        data.req <- dplyr::filter(data.req, intervened == "f")
    } else {
        data.req <- dplyr::filter(data.req, intervened != "t")
      }
  }

  den_col <- paste0(varname, ".den")
  num_col <- paste0(varname, ".num")
  total_obs <- dplyr::filter(data.req,
                             !is.na(data.req[[den_col]]) & !is.na(data.req[[num_col]]))

  print(paste0("Total observations: ", nrow(total_obs)))

  check <- dplyr::filter(data.req,
                         data.req[[num_col]] > data.req[[den_col]])

  if (nrow(check) == 0) {
    print("denominator is always larger or equal to numerator: good!")
  } else {
    print(paste0("denominator is smaller than numerator in ", nrow(check), " rows: please check."))
  }

  return(dplyr::filter(data.req, !is.na(data.req[[den_col]])))
}


#' Filter Studies Based on Barnabas Zogo Criteria
#'
#' Applies variable-specific filtering rules following methodological criteria defined by Barnabas Zogo.
#' These filters help retain only studies with appropriate sampling methods for analyzing specific
#' entomological indicators such as endophagy, endophily, or human blood index (HBI).
#'
#' @param datareq A `data.frame` containing the dataset to be filtered.
#' @param varname A character string indicating the variable of interest (e.g., `"endophagy"`, `"endophily"`, `"outdoor_HBI"`).
#' @param nice_varname A descriptive string (e.g., `"Endophagy"`, `"Endophily"`, `"HBI"`) that determines the filter rules applied.
#'
#' @return A tibble filtered according to the variable-specific methodology.
#'
#' @details
#' Filters applied by \code{nice_varname}:
#' \itemize{
#'   \item \strong{Endophagy:} Retains rows where:
#'     \itemize{
#'       \item `indoor_biting_sampling == "MBI"`
#'       \item `outdoor_biting_sampling == "MBO"`
#'       \item `HLC_same == TRUE`
#'     }
#'   \item \strong{Endophily:} Retains rows where:
#'     \itemize{
#'       \item `indoor_resting_sampling == "HRI"`
#'       \item `outdoor_resting_sampling == "WinExit"`
#'       \item `WinExit_in_HRI_houses == "t"`
#'     }
#'   \item \strong{HBI:} Removes rows where `host_unit == "AI"`.
#'     If `varname == "outdoor_HBI"`, further restricts `host_sampling` to:
#'     \code{c("RO", "RO (shelter)", "RO (pit)")}.
#' }
#'
filter_bz_studies <- function(datareq, varname, nice_varname) {

  if (nice_varname == "Endophagy") {
    datareq <- datareq |>
      dplyr::filter(indoor_biting_sampling == "MBI",
                    outdoor_biting_sampling == "MBO",
                    HLC_same)
  }

  if (nice_varname == "Endophily") {
    datareq <- datareq |>
      dplyr::filter(indoor_resting_sampling == "HRI",
                    outdoor_resting_sampling == "WinExit",
                    WinExit_in_HRI_houses == "t")
  }

  if (grepl("HBI", varname)) {
    datareq <- datareq |>
      dplyr::filter(host_unit != "AI")

    if (varname == "outdoor_HBI") {
      datareq <- datareq |>
        dplyr::filter(host_sampling %in% c("RO", "RO (shelter)", "RO (pit)"))
    }
  }

  return(datareq)
}


#' Augment Dataset with Species-Complex Taxonomic Harmonization
#'
#' Enriches a dataset with harmonized taxonomic information by merging a compatibility table and generating
#' synthetic entries when needed. This ensures that all observations are mapped at the species level—even when only
#' complex-level data is available—and prevents imbalances in downstream statistical models.
#'
#' @param data A `data.frame` containing the dataset to be augmented. Must include a `survey` column,
#'   and optionally `species` and/or `complex` columns.
#' @param compat A `data.frame` or tibble providing taxonomic mapping for `survey` identifiers, typically
#'   including `survey`, `species`, and `complex` columns.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`data`}{The augmented dataset, with harmonized species/complex names and corresponding numeric IDs.}
#'   \item{`species_complex`}{A reference tibble mapping unique species and complex combinations to numeric codes.}
#' }
#'
#' @details
#' The following steps are applied to ensure taxonomic consistency:
#' \enumerate{
#'   \item \strong{Taxonomic Join:} The `compat` table is joined to the input dataset via the `survey` column.
#'
#'   \item \strong{Synthetic Complex Names:} If `complex` is missing (`""`) for a given species, a synthetic complex is created
#'     as `"[species]_complex"`.
#'
#'   \item \strong{Synthetic Species Names:} If an entry is missing `species` but has a valid `complex`, a pseudo-species is generated
#'     with the label `"unlabel_[complex]"` to ensure all rows are treated as species-level data.
#'
#'   \item \strong{Numeric Identifiers:}
#'     \itemize{
#'       \item `speciesNb`: numeric ID for each (real or synthetic) species.
#'       \item `complexNb`: numeric ID for each complex, offset by 1 to avoid overlap.
#'     }
#' }
#'
#' These steps normalize taxonomic granularity in the dataset, preventing over- or under-representation of records
#' during model estimation (e.g., of intercepts like `beta0`).
augment_with_taxonomy <- function(data, compat) {

  data <- dplyr::left_join(data, compat, by = "survey")

  idx <- which(data$complex == "")
  if (length(idx) > 0) {
    data$complex[idx] <- paste0(data$species[idx], "_complex")
  }

  data <- data |>
    dplyr::mutate(name = ifelse(species == "", complex, species))

  data <- data |>
    dplyr::filter(species != "") |>
    dplyr::bind_rows(
      data |>
        dplyr::filter(species == "") |>
        dplyr::mutate(
          species = paste0("unlabeled ", complex),
          name = paste0("unlabeled ", complex),
          detail = "species"
        )
    )

  data$speciesNb <- as.numeric(as.factor(data$species))
  data$complexNb <- as.numeric(as.factor(data$complex)) + 1

  species_complex <- data |>
    dplyr::distinct(speciesNb, .keep_all = TRUE) |>
    dplyr::select(species, speciesNb, complex, complexNb) |>
    dplyr::arrange(speciesNb)

  return(list(data = data, species_complex = species_complex))
}


#' Prepare Clean and Taxonomically Harmonized Dataset for Bayesian Modeling
#'
#' Prepares a cleaned and processed dataset ready for Bayesian analysis of a specified entomological variable.
#' It applies geographic and temporal filters, computes proportions, cleans insecticide data, applies study-specific filters,
#' harmonizes taxonomy, and computes the final outcome variable.
#'
#' @param varname Character string. The variable of interest.
#'    Supported values: `"parous_rate"`, `"endophagy"`, `"endophily"`, `"indoor_HBI"`, `"outdoor_HBI"`, `"sac_rate"`, `"HBI"`
#' @param geo Character vector. Geographic regions or continents to include in the analysis.
#'   Defaults to WHO major regions: `c("Africa-E", "Africa-W", "Americas", "Asia-Pacific", NA)`.
#' @param year_min Integer or numeric. Minimum year of data collection (inclusive). Default is `-Inf` (no limit).
#' @param year_max Integer or numeric. Maximum year of data collection (inclusive). Default is `+Inf` (no limit).
#' @param extern_data Data frame. Optional user-supplied dataset to append to the internal repository.
#'   Must follow the same structure as the internal dataset used in `create_repo()`. Default is `NULL`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{`data.req`}{The fully prepared and filtered dataset.}
#'   \item{`varname`}{The variable name used in the processing.}
#'   \item{`nice_varname`}{Human-readable label of the variable of interest.}
#'   \item{`species_complex`}{Reference table mapping species and complexes to numeric identifiers.}
#' }
#'
#' @details
#' This function orchestrates multiple preprocessing steps to transform raw entomological survey data
#' into a format suitable for Bayesian analysis. If the user provides an external dataset via `extern_data`,
#' it is appended to the internal dataset before filtering and processing.
#'
#' The steps are as follows:
#' \enumerate{
#'   \item Loads raw data using `create_repo()`. If `extern_data` is provided, it is appended using `adding_data_extern()`.
#'   \item Sets variable-specific parameters via `set_var_params()`.
#'   \item Filters dataset by specified geographic regions and years using `region_period_filter()`.
#'   \item Computes numerators and denominators for proportions with `augment_withProportion_modif()`.
#'   \item Removes or keeps rows based on unknown insecticide intervention using `create_datareq()`.
#'   \item Applies variable-specific epidemiological filters using `filter_bz_studies()`.
#'   \item Harmonizes taxonomy and generates numeric IDs using `augment_with_taxonomy()`.
#'   \item Calculates the outcome variable as the ratio of numerator to denominator.
#' }
#'
#' Internal checks stop execution early if filtering steps remove all observations.
#'
#' @export
creation_df <- function(varname,
                        geo = c("Africa-E", "Africa-W", "Americas", "Asia-Pacific", NA),
                        year_min = -Inf,
                        year_max = +Inf,
                        extern_data = NULL) {

  # Step 1: Load data
  repo <- create_repo()

  # Step 2: Adding data
  if (!is.null(extern_data)) {
    repo <- adding_data_extern(varname, repo, extern_data)
  }

  # Step 3: Set variable-specific parameters
  var_params <- set_var_params(varname, repo)
  repo <- var_params$repo

  # Step 4: Filter region and time
  repo_filtered <- region_period_filter(repo, geo, year_min, year_max)

  if (nrow(repo_filtered) == 0) {
    stop("No observations found: the specified region and date range are too restrictive.")
  }

  # Step 5: Compute proportions
  data.req <- augment_withProportion_modif(
    repo_filtered,
    varname,
    var_params$denominator_variables,
    var_params$numerator_variables,
    var_params$percentage_variables
  )

  if (nrow(data.req) == 0) {
    stop("No observations found after proportion computation: check filtering criteria.")
  }

  # Step 6: Clean insecticide intervention data
  data.req <- create_datareq(
    data.req,
    varname,
    var_params$remove_unknown_insecticide
  )

  if (nrow(data.req) == 0) {
    stop("No observations found after insecticide intervention filtering.")
  }

  # Step 7: Apply variable-specific epidemiological filters
  data.req <- filter_bz_studies(
    datareq = data.req,
    varname = varname,
    nice_varname = var_params$nice_varname
  )

  if (nrow(data.req) == 0) {
    stop("No observations found after epidemiological filtering.")
  }


  # Step 8: Taxonomic harmonization
  compat_repo <- read_data_file("compatibility_repo_taxonomy_v2.csv", sep = ";")
  required_cols(compat_repo, c("survey"), "compatibility_repo_taxonomy_v2.csv")

  merging <- augment_with_taxonomy(data.req, compat_repo)
  data.req <- merging$data
  species_complex <- merging$species_complex

  # Step 9: Compute outcome variable (proportion)
  data.req <- data.req |>
    dplyr::mutate(value = get(paste(varname, "num", sep = ".")) / get(paste(varname, "den", sep = ".")))

  # Return results
  return(list(
    data.req = data.req,
    varname = varname,
    nice_varname = var_params$nice_varname,
    species_complex = species_complex
  ))
}

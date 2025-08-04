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
#'
#' @export
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
#'
#' @export
required_cols <- function(df, expected, name) {
  missing <- setdiff(expected, colnames(df))
  if (length(missing) > 0) {
    stop(paste0("Missing required columns in ", name, ": ", paste(missing, collapse = ", ")))
  }
}





























multi_species_pie <- function(seuil_prop_autres = 0.05,
                              plot_dir = "chemin/vers/ton/dossier/") {
  varnames <- c("endophagy", "endophily",
                "indoor_HBI", "outdoor_HBI", "parous_rate", "sac_rate")

  colors_map <- read_data_file(file = "new_palette_density_plots.csv")

  all_data_clean_list <- list()  # Pour stocker les data_clean de chaque plot

  plot_for_var <- function(varname) {
    prepared <- creation_df(varname = varname)
    data.req <- prepared$data.req
    nice_varname <- prepared$nice_varname
    obs <- paste0(varname, ".den")

    data_clean <- data.req |>
      dplyr::mutate(
        is_complex = grepl("^unlabel ", species),
        species_clean = ifelse(is_complex, sub("^unlabel ", "", species), species)
      ) |>
      dplyr::group_by(species_clean, is_complex) |>
      dplyr::summarise(
        total_obs = sum(.data[[obs]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        label = ifelse(is_complex,
                       paste0("Complex: ", species_clean),
                       paste0("Species: ", species_clean)),
        prop = total_obs / sum(total_obs)
      ) |>
      dplyr::left_join(colors_map, by = c("species_clean" = "name"))

    if (nrow(data_clean) >= 8) {
      data_clean <- data_clean |>
        dplyr::arrange(prop) |>
        dplyr::mutate(cum_prop = cumsum(prop)) |>
        dplyr::mutate(
          label_grouped = ifelse(cum_prop <= seuil_prop_autres, "Other", label),
          pal = ifelse(cum_prop <= seuil_prop_autres, NA, pal)
        ) |>
        dplyr::group_by(label_grouped) |>
        dplyr::summarise(
          total_obs = sum(total_obs),
          pal = dplyr::first(na.omit(pal)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          prop = total_obs / sum(total_obs),
          pal = ifelse(is.na(pal), "#CCCCCC", pal),
          label = label_grouped
        )
    } else {
      data_clean <- data_clean |>
        dplyr::mutate(
          label = label,
          pal = ifelse(is.na(pal), "#CCCCCC", pal)
        )
    }

    data_clean <- data_clean |>
      dplyr::arrange(prop) |>
      dplyr::mutate(
        label_pct = paste0(label, " · ", round(prop * 100), "%"),
        label = factor(label, levels = label)
      )

    all_data_clean_list[[varname]] <<- data_clean  # Sauvegarde pour la légende

    ggplot2::ggplot(data_clean, ggplot2::aes(x = "", y = prop, fill = label)) +
      ggplot2::geom_col(width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(prop > 0.03, paste0(round(prop * 100), "%"), "")),
        position = ggplot2::position_stack(vjust = 0.5),
        color = "black",
        size = 17
      ) +
      ggplot2::scale_fill_manual(values = setNames(data_clean$pal, data_clean$label)) +
      ggplot2::theme_void(base_size = 25) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(t = 5, b = 10),
        plot.title = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(hjust = 0.5, size = 50, face = "bold")
      ) +
      ggplot2::labs(caption = nice_varname)
  }

  # Génération des graphiques
  plots <- base::lapply(varnames, plot_for_var)

  # Construction propre de la légende à partir des data_clean
  legend_data <- dplyr::bind_rows(all_data_clean_list)
  legend_df <- legend_data |>
    dplyr::distinct(label, pal) |>
    dplyr::arrange(label)

  legend_plot <- ggplot2::ggplot(legend_df, ggplot2::aes(x = 1, y = label, fill = label)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = setNames(legend_df$pal, legend_df$label)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 60),
      legend.key.size = ggplot2::unit(3, "lines"),
      legend.spacing.y = ggplot2::unit(10, "lines")
    )

  legend <- cowplot::get_legend(legend_plot)

  plot_grid_final <- gridExtra::grid.arrange(
    cowplot::plot_grid(plotlist = plots, ncol = 2, nrow = 3,
                       rel_widths = c(1, 1), rel_heights = c(1.2, 1.2, 1.2), hjust = 0),
    legend,
    ncol = 2,
    widths = c(1.9, 1.1)  # plus d’espace à droite
  )

  outfile <- base::file.path(plot_dir, "multi_species_piechart.png")
  ggplot2::ggsave(outfile, plot = plot_grid_final, width = 37, height = 40, dpi = 300)
  base::message("Plot saved at: ", outfile)
}


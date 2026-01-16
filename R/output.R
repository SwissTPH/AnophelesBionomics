#' Generate Lollipop Plots by Complex
#'
#' @description
#' Creates a set of lollipop-style density plots showing the distribution of a specified
#' bionomics variable (e.g., endophagy, endophily, parous rate) across mosquito species,
#' grouped by higher-level species complexes. Each complex is displayed as a separate
#' subplot with consistent scaling and color mapping. Optionally saves the final
#' combined plot as a PNG file.
#'
#' @param data A list containing the following elements:
#'   - `data.req`: A data frame with observation data. Must include columns `name`,
#'     `value`, and a density column named following the pattern `<varname>.den`
#'     (e.g., `"endophagy.den"`).
#'   - `species_complex`: A data frame linking species names to their corresponding
#'     higher-level complexes, with columns `species` and `complex`.
#'
#' @param varname A character string indicating the base name of the behavioral
#' variable to plot (e.g., `"endophagy"`, `"parous_rate"`). The corresponding
#' density column must be present in `data$data.req`.
#'
#' @param path Optional. A character string giving the file path where the final
#' combined PNG plot should be saved. If `NULL`, the plot is not saved.
#'
#' @return A combined `ggplot` object containing one lollipop density plot per
#' species complex, aligned vertically and annotated with appropriate axis and
#' variable labels.
#'
#' @details
#' - Species names prefixed with `"unlabeled"` are cleaned before plotting.
#' - Species are matched to colors from a palette file named
#'   `"new_palette_density_plots.csv"`. Species missing from the palette are
#'   automatically assigned a default grey color.
#' - The resulting plots are combined and annotated using **cowplot**.
#' - If `path` is provided, the function saves the figure as a high-resolution PNG.
#'
#' @export
plot_lollipop <- function(data, varname, path = NULL) {

  dplyr::mutate(data$data.req, name = sub("^unlabeled\\s+", "", data$data.req$name))

  species_complex <- data$species_complex

  if ("species" %in% names(species_complex)) {
    data_2 <- dplyr::left_join(data$data.req, species_complex, by = c("name" = "species"))
    possible_cols <- intersect(c("complex", "complex.x", "complex.y"), names(data_2))
    if (length(possible_cols) == 0) stop("Aucune colonne 'complex' trouvée après la jointure.")
    data_2 <- data_2 |>
      dplyr::rename(higher_level = !!rlang::sym(possible_cols[1])) |>
      dplyr::mutate(higher_level = sub("_complex$", "", higher_level))
  }

  colors_map <- read_data_file(file = "new_palette_density_plots.csv")
  missing_names <- setdiff(unique(data_2$name), colors_map$name)
  if (length(missing_names) > 0) {
    colors_map <- dplyr::bind_rows(colors_map, data.frame(name = missing_names, pal = "#D3D3D3"))
  }
  data_2 <- dplyr::left_join(data_2, colors_map, by = "name")

  grouped_df <- dplyr::group_by(data_2, higher_level)
  split_list <- dplyr::group_split(grouped_df)
  group_names <- dplyr::pull(dplyr::group_keys(grouped_df), higher_level)

  y_col <- rlang::sym(paste0(varname, ".den"))

  var_labels <- c(
    endophagy = "Endophagy",
    endophily = "Endophily",
    indoor_HBI = "Indoor HBI",
    outdoor_HBI = "Outdoor HBI",
    parous_rate = "Parous rate",
    sac_rate = "Sac rate",
    resting_duration = "Resting Duration"
  )

  p_list <- lapply(seq_along(split_list), function(i) {
    sub_df <- droplevels(split_list[[i]])
    p <- ggplot2::ggplot(sub_df, ggplot2::aes(x = value, y = !!y_col)) +
      ggplot2::geom_segment(ggplot2::aes(x = value, xend = value, y = 0, yend = !!y_col),
                            color = "grey80", linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes(color = pal), alpha = 0.8, size = 3) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 13),
        axis.title.y = ggplot2::element_text(size = 13)
      )

    legend_df <- unique(sub_df[, c("name", "pal")])
    legend_plot <- ggplot2::ggplot(legend_df, ggplot2::aes(x = 1, y = reorder(name, desc(name)), color = pal)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_text(ggplot2::aes(label = name), hjust = 0, nudge_x = 0.1, color = "black", size = 4) +
      ggplot2::scale_color_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(5, 20, 5, 5), legend.position = "none") +
      ggplot2::xlim(1, 3)

    cowplot::plot_grid(p, legend_plot, rel_widths = c(0.65, 0.35))
  })

  names(p_list) <- group_names

  combined <- cowplot::plot_grid(plotlist = p_list, ncol = 1, align = "v")
  combined <- cowplot::ggdraw() +
    cowplot::draw_plot(combined, x = 0.1, y = 0.1, width = 0.85, height = 0.85) +
    cowplot::draw_label(var_labels[[varname]],
                        x = 0.5, y = 0.07,
                        hjust = 0.5, vjust = 0.5, size = 14) +
    cowplot::draw_label("Number of observed mosquitoes",
                        x = 0.04, y = 0.5,
                        angle = 90,
                        hjust = 0.5, vjust = 0.5, size = 14)

  if (!is.null(path)) {
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    ggplot2::ggsave(path, plot = combined,
                    width = 10,
                    height = length(p_list) * 2,
                    dpi = 300,
                    device = "png")
  }

  return(combined)
}

#' Generate a Pie Chart of Observations by Species or Complex
#'
#' @description
#' Creates an interactive pie chart showing the distribution of observations
#' by species or species complexes, based on a specified observation variable
#' from a pre-processed dataset. Optionally groups species with a small
#' proportion of observations into an "Other" category, and can save the plot
#' as an HTML file for sharing or embedding.

#' @param data A list containing the following elements:
#'   - `data.req`: A data frame with observation data. Must contain a column
#'     named with the pattern `<varname>.den` (e.g., `"parous_rate.den"`).
#'   - `varname`: A character string indicating the base name of the observation variable.
#'   - `nice_varname`: A character string used for naming the saved output file.
#'
#' @param threshold_prop_other A numeric value between 0 and 1 indicating the
#' minimum proportion of total observations required for a species or complex
#' to appear as its own slice in the pie chart. Species below this threshold
#' are grouped under "Other". Default is 0.05 (5 percent).
#'
#' @param plot_dir Optional. A character string giving the directory path where
#' the HTML version of the pie chart should be saved. If `NULL`, the chart is not saved.
#' Default is a specific folder on the user's system.

#' @return A list containing:
#'   - `plotly`: An interactive pie chart showing the distribution of observations.
#'   - `HTML file`: If `plot_dir` is provided, the chart is also saved as an HTML file.
#'
#' @details
#' - Species names starting with `"unlabel_"` are treated as complexes and labeled accordingly.
#' - The observation variable used in the calculation must match the pattern `<varname>.den`.
#' - The pie chart uses **plotly** for interactive visualization and **htmlwidgets** to export.
#'
#' @export
obs_complex_species_pie <- function(data,
                                    threshold_prop_other = 0.05,
                                    plot_dir = NULL) {
  sd_fun <- sd
  mean_fun <- mean
  sum_fun <- base::sum
  round_fun <- base::round

  data.req <- data$data.req
  varname <- data$varname
  nice_varname <- data$nice_varname
  obs <- paste0(varname, ".den")

  mean_value <- mean_fun(data.req$value, na.rm = TRUE)
  sd_value <- sd_fun(data.req$value, na.rm = TRUE)
  total_obs_all <- sum_fun(data.req[[obs]], na.rm = TRUE)
  total_surveys_all <- nrow(data.req)

  colors_map <- read_data_file(file = "new_palette_density_plots.csv")

  data_clean <- dplyr::mutate(data.req,
                              is_complex = grepl("^unlabeled ", species),
                              species_clean = ifelse(is_complex, sub("^unlabeled ", "", species), species)) |>
    dplyr::group_by(species_clean, is_complex) |>
    dplyr::summarise(
      total_obs = sum_fun(.data[[obs]], na.rm = TRUE),
      nb_lines = dplyr::n(),
      mean_value = mean_fun(value, na.rm = TRUE),
      sd_value = sd_fun(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = ifelse(is_complex,
                     paste0("Complex: ", species_clean),
                     paste0("Species: ", species_clean)),
      prop = total_obs / sum_fun(total_obs)
    ) |>
    dplyr::left_join(colors_map, by = c("species_clean" = "name"))

  n_species <- nrow(data_clean)

  if (n_species >= 8) {
    data_grouped <- data_clean |>
      dplyr::arrange(prop) |>
      dplyr::mutate(
        cum_prop = cumsum(prop),
        label_grouped = ifelse(cum_prop <= threshold_prop_other, "Other", label),
        pal = ifelse(label_grouped == "Other", NA, pal)
      ) |>
      dplyr::group_by(label_grouped) |>
      dplyr::summarise(
        total_obs = sum_fun(total_obs),
        nb_lines = sum_fun(nb_lines),
        mean_value = mean_fun(mean_value),
        sd_value = mean_fun(sd_value),
        pal = dplyr::first(stats::na.omit(pal)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        prop = total_obs / sum_fun(total_obs),
        pal = ifelse(is.na(pal), "#CCCCCC", pal),
        hover_text = paste0(
          label_grouped, "<br>",
          "Number of observations (sum): ", total_obs, "<br>",
          "Number of surveys (rows): ", nb_lines, "<br>",
          "Mean value: ", round_fun(mean_value, 2), "<br>",
          "SD value: ", round_fun(sd_value, 2), "<br>",
          "Proportion: ", round_fun(prop, 1)  # Sans multiplication par 100
        ),
        label_text = paste0(label_grouped, " : ", round_fun(prop, 1))
      )

    message("Proportion of the 'Other' category' : ",
            round_fun(sum_fun(data_grouped$prop[data_grouped$label_grouped == "Other"]), 2))
  } else {
    data_grouped <- data_clean |>
      dplyr::mutate(
        label_grouped = label,
        prop = prop,
        pal = ifelse(is.na(pal), "#CCCCCC", pal),
        hover_text = paste0(
          label_grouped, "<br>",
          "Number of observations (sum): ", total_obs, "<br>",
          "Number of surveys (rows): ", nb_lines, "<br>",
          "Mean value: ", round_fun(mean_value, 2), "<br>",
          "SD value: ", round_fun(sd_value, 2), "<br>",
          "Proportion: ", round_fun(prop, 1)  # Sans multiplication par 100
        ),
        label_text = paste0(label_grouped, " : ", round_fun(prop, 1))
      )
  }

  title_text <- paste0(nice_varname)

  p <- plotly::plot_ly(
    data = data_grouped,
    labels = ~label_grouped,
    values = ~total_obs,
    type = "pie",
    textposition = 'outside',
    text = ~hover_text,
    hoverinfo = "text",
    texttemplate = ~label_text,
    insidetextorientation = 'radial',
    marker = list(
      colors = data_grouped$pal,
      line = list(color = '#FFFFFF', width = 1)
    ),
    textfont = list(size = 20, family = "Arial"),
    domain = list(x = c(0.3, 0.7), y = c(0.4, 0.8)),
    direction = "counterclockwise",
    rotation = 120
  )|>
    plotly::layout(
      annotations = list(
        list(
          text = paste0("<b>", title_text, "</b>"),
          x = 0.5,
          y = 0.21,
          xanchor = 'center',
          yanchor = 'bottom',
          showarrow = FALSE,
          font = list(size = 25, family = "Arial", color = "black"),
          xref = "paper",
          yref = "paper"
        )
      ),
      margin = list(b = 0),
      showlegend = FALSE
    )

  if (!is.null(plot_dir)) {
    output_path <- file.path(plot_dir, paste0(nice_varname, "_piechart.html"))
    htmlwidgets::saveWidget(p, output_path, selfcontained = TRUE)
    message("Chart saved as HTML: ", output_path)
  }

  return(p)
}


#' Generate a Pie Chart of Observations by Region
#'
#' Generates an interactive pie chart showing the distribution of observations
#' by region using Plotly. Each slice of the pie represents a region, with proportions
#' based on the sum of a specified observation variable.
#'
#' @param data A list containing:
#'   - `data.req`: a data frame that includes a column `region`, a numeric column `value`,
#'     and an observation count column named `varname.den`.
#'   - `varname`: the base name of the variable of interest (e.g., "n").
#'   - `nice_varname`: a human-readable version of the variable name (unused in this function).
#'
#' @param plot_dir Optional string. Directory path where the plot will be saved as an HTML file.
#'   If `NULL` (default), the plot is not saved.
#'
#' @return A Plotly pie chart object representing the proportion of observations per region.
#'   Optionally saves the chart to an HTML file if `plot_dir` is provided.
#'
#' @details
#' The function:
#' - Replaces missing region values with `"Unspecified"`.
#' - Aggregates data by region to compute total observations, number of rows, mean and standard deviation of `value`.
#' - Calculates the proportion of total observations for each region.
#' - Assigns colors using a rainbow palette (unless `colors_map` already exists in the environment).
#' - Builds an interactive pie chart with detailed hover text and labels.
#'
#' @export
obs_region_pie <- function(data,
                           plot_dir = NULL) {
  sd_fun <- sd
  mean_fun <- mean
  sum_fun <- base::sum
  round_fun <- base::round

  data.req <- data$data.req
  varname <- data$varname
  nice_varname <- data$nice_varname
  obs <- paste0(varname, ".den")

  data_clean <- data.req |>
    dplyr::mutate(region_clean = ifelse(is.na(region), "Unspecified", as.character(region))) |>
    dplyr::group_by(region_clean) |>
    dplyr::summarise(
      total_obs = sum_fun(.data[[obs]], na.rm = TRUE),
      nb_lines = dplyr::n(),
      mean_value = mean_fun(value, na.rm = TRUE),
      sd_value = sd_fun(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prop = total_obs / sum_fun(total_obs),
      label_grouped = paste0("Region: ", region_clean)
    )

  if (!exists("colors_map") || is.null(colors_map)) {
    set.seed(42)
    colors_map <- data.frame(
      name = unique(data_clean$region_clean),
      pal = grDevices::rainbow(length(unique(data_clean$region_clean)))
    )
  }

  data_clean <- dplyr::left_join(data_clean, colors_map, by = c("region_clean" = "name"))
  data_clean$pal[is.na(data_clean$pal)] <- "#CCCCCC"

  data_clean <- data_clean |>
    dplyr::mutate(
      hover_text = paste0(
        label_grouped, "<br>",
        "Number of observations (sum): ", total_obs, "<br>",
        "Number of surveys (rows): ", nb_lines, "<br>",
        "Mean value: ", round_fun(mean_value, 2), "<br>",
        "SD value: ", round_fun(sd_value, 2), "<br>",
        "Proportion: ", round_fun(prop * 100, 1), "%"
      ),
      label_text = paste0(label_grouped, " : ", round_fun(prop * 100), "%")
    )

  title_text <- paste0("Distribution of Observations by Region")

  p <- plotly::plot_ly(
    data = data_clean,
    labels = ~label_grouped,
    values = ~total_obs,
    type = "pie",
    textposition = 'outside',
    text = ~hover_text,
    hoverinfo = "text",
    texttemplate = ~label_text,
    insidetextorientation = 'radial',
    marker = list(
      colors = data_clean$pal,
      line = list(color = '#FFFFFF', width = 1)
    ),
    textfont = list(size = 20, family = "Arial"),
    domain = list(x = c(0.3, 0.7), y = c(0.4, 0.8)),
    direction = "counterclockwise",
    rotation = 120
  ) |>
    plotly::layout(
      annotations = list(
        list(
          text = paste0("<b>", title_text, "</b>"),
          x = 0.5,
          y = 0.21,
          xanchor = 'center',
          yanchor = 'bottom',
          showarrow = FALSE,
          font = list(size = 25, family = "Arial", color = "black"),
          xref = "paper",
          yref = "paper"
        )
      ),
      margin = list(b = 0),
      showlegend = FALSE
    )

  if (!is.null(plot_dir)) {
    output_path <- file.path(plot_dir, paste0(varname, "_region_piechart.html"))
    htmlwidgets::saveWidget(p, output_path, selfcontained = TRUE)
    message("Chart saved as HTML: ", output_path)
  }

  return(p)
}


#' Format MCMC Samples for Visualization
#'
#' @description
#' Converts MCMC samples from a hierarchical Bayesian model into a tidy (long-format) `data.frame` enriched with taxonomic information.
#' The function identifies the level (species, complex, or genus), assigns names based on taxonomy, and maps groupings for later plotting.
#'
#' @param stan_results A list containing all MCMC results and metadata, expected to include:
#'   \describe{
#'     \item{samples}{An object of class `mcmc.list` (posterior samples for species- and complex-level parameters).}
#'     \item{chain_length}{Integer. Total number of MCMC iterations per chain.}
#'     \item{burnin}{Integer. Number of initial iterations to discard.}
#'     \item{thinning}{Integer. Thinning interval used during sampling.}
#'     \item{species_complex}{A `data.frame` mapping species and complexes with columns `species`, `complex`, `speciesNb`, `complexNb`.}
#'   }
#'
#' @return A `data.frame` in long format with the following columns:
#' \describe{
#'   \item{`value`}{Posterior sample value.}
#'   \item{`Nb`}{Index number of the parameter (species or complex ID).}
#'   \item{`level`}{Level of the parameter: `"species"`, `"complex"`, or `"genus"`.}
#'   \item{`name`}{Name of the species, complex, or `"GENUS"` (for group-level effect).}
#'   \item{`higher_level`}{Grouping label: complex name for species, self-name for complex/genus.}
#' }
#'
#' @details
#' Species-level parameters are assumed to be named `p2[...]` and complex-level parameters `p1[...]`.
#'A special case is handled when `p1[1]` represents the genus-level intercept. The function processes indices and variable names using regular expressions, and joins taxonomic metadata using `species_complex`.
#'
#' @export
prepare_data_density <- function(stan_results) {

  samples_all <- stan_results$fit

  species_complex <- stan_results$species_complex
  nice_varname <- stan_results$nice_varname
  iter <- stan_results$iter
  thinning <- stan_results$thinning
  warmup <- stan_results$warmup

  samples <- coda::mcmc.list(
    lapply(samples_all, function(chain_mcmc) {
      subset_chain <- chain_mcmc[, base::grepl("^p1|^p2", base::colnames(chain_mcmc)), drop = FALSE]
      coda::mcmc(subset_chain, start = 1, end = nrow(subset_chain), thin = 1)
    })
  )
  samples.df <- base::as.data.frame(base::as.matrix(samples), header = TRUE)

  samples.df <- tidyr::gather(samples.df, Nb)

  samples.df <- dplyr::mutate(
    samples.df,
    level = base::ifelse(base::substr(Nb, 1, 2) == "p1", "complex", "species"),
    Nb = base::ifelse(base::grepl("\\[", Nb), Nb, base::paste0(Nb, "[1]"))
  )

  samples.df$Nb <- base::as.numeric(
    unlist(stringr::str_match_all(samples.df$Nb, "\\[(.*?)\\]"))[c(FALSE, TRUE)]
  )

  samples.df$level[base::which(samples.df$level == "complex" & samples.df$Nb == 1)] <- "genus"

  for (r in base::seq_len(base::nrow(samples.df))) {
    samples.df$name[r] <- base::ifelse(
      samples.df$level[r] == "complex",
      species_complex$complex[base::which(species_complex$complexNb == samples.df$Nb[r])],
      species_complex$species[base::which(species_complex$speciesNb == samples.df$Nb[r])]
    )
  }

  samples.df$name[base::which(samples.df$level == "genus")] <- "GENUS"

  for (r in base::seq_len(base::nrow(samples.df))) {
    samples.df$higher_level[r] <- base::ifelse(
      samples.df$level[r] == "species",
      species_complex$complex[base::which(species_complex$species == samples.df$name[r])],
      samples.df$name[r]
    )
  }

  samples.df <- dplyr::filter(samples.df, !base::grepl("complex", name, ignore.case = TRUE))

  target_species <- dplyr::filter(species_complex, base::grepl("unlabeled", species, ignore.case = TRUE))

  for (i in base::seq_len(base::nrow(target_species))) {
    sp_name <- target_species$species[i]
    sp_complex <- target_species$complex[i]
    associated_species <- dplyr::filter(species_complex, complex == sp_complex)
    if (base::nrow(associated_species) == 1) {
      sp_values <- dplyr::filter(samples.df, name == sp_name)
      if (base::nrow(sp_values) > 0) {
        samples.df <- dplyr::filter(samples.df, !(name == sp_complex & level == "complex"))
        sp_values <- dplyr::mutate(sp_values,
                                   name = sp_complex,
                                   level = "complex")
        samples.df <- dplyr::bind_rows(samples.df, sp_values)
      }
      samples.df <- dplyr::filter(samples.df, name != sp_name)
    }
  }

  return(samples.df)
}


#' @title Generate Posterior Density Plots by Species Complex
#'
#' @description
#' This function creates posterior density plots for the hierarchical Bayesian model parameters
#' aggregated by species complex. It uses the stanfit object and associated data generated by `run_stan()`.
#' The plot can optionally be saved to a specified directory.
#'
#' @param stan_results Named list output from `run_stan()` containing the fitted model and related data.
#' @param unlabel Logical. If TRUE, removes labels from the plot. Default is FALSE.
#' @param plot_dir Optional character. Directory path to save the plot image. If NULL, the plot is not saved.
#' @param complex_names Optional character vector. Names of species complexes to include in the plot. If NULL, all are included.
#'
#' @export
plot_density <- function(stan_results,
                           unlabel = FALSE,
                           path = NULL,
                           complex_names = NULL) {

  fit <- stan_results$fit
  species_complex <- stan_results$species_complex
  nice_varname <- stan_results$nice_varname

  df <- prepare_data_density(stan_results)

  if (unlabel) {
    df <- dplyr::filter(df, !base::grepl("unlabeled", name, ignore.case = TRUE))
  }

  if (!is.null(complex_names)) {
    df <- dplyr::filter(df, higher_level %in% c(complex_names, "GENUS"))
  }

  duplicated_genus <- dplyr::filter(df, name == "GENUS")
  duplicated_genus <- duplicated_genus[base::rep(1:base::nrow(duplicated_genus),
                                                 each = base::length(base::unique(df$higher_level))), ]
  duplicated_genus$higher_level <- base::unique(df$higher_level)
  df <- dplyr::distinct(base::rbind(df, duplicated_genus))

  df <- dplyr::left_join(df, data.frame(
    level = c("genus", "complex", "species"),
    line_type = c("dotted", "longdash", "solid")
  ), by = "level")

  df$level <- base::factor(df$level, levels = c("genus", "complex", "species"))

  other_names <- df |>
    dplyr::filter(name != "GENUS") |>
    dplyr::distinct(name) |>
    dplyr::arrange(name) |>
    dplyr::pull()
  df$name <- base::factor(df$name, levels = c("GENUS", other_names))

  other_higher <- df |>
    dplyr::filter(higher_level != "GENUS") |>
    dplyr::distinct(higher_level) |>
    dplyr::arrange(higher_level) |>
    dplyr::pull()
  df$higher_level <- base::factor(df$higher_level, levels = c(other_higher, "GENUS"))

  colors_map <- read_data_file(file = "new_palette_density_plots.csv")

  if (!("GENUS" %in% colors_map$name)) {
    colors_map <- dplyr::bind_rows(
      data.frame(name = "GENUS", pal = "black"),
      colors_map
    )
  }

  missing_names <- base::setdiff(base::unique(df$name), colors_map$name)
  if (base::length(missing_names) > 0) {
    base::warning("The following names do not have a defined color in palette_name : ",
                  base::paste(missing_names, collapse = ", "))
    colors_map <- dplyr::bind_rows(
      colors_map,
      data.frame(name = missing_names, pal = "#D3D3D3")
    )
  }

  df <- dplyr::left_join(df, colors_map, by = "name")

  legend_plot <- ggplot2::ggplot(df) +
    ggplot2::geom_density(ggplot2::aes(x = value, linetype = line_type),
                          lwd = 1.5, stat = "density", color = "black") +
    ggplot2::scale_linetype_identity("Level:",
                                     labels = base::levels(df$level),
                                     breaks = c("dotted", "longdash", "solid"),
                                     guide = "legend") +
    ggplot2::labs(title = nice_varname) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 30),
      text = ggplot2::element_text(size = 25),
      legend.position = "top",
      legend.box = "vertical",
      legend.justification = c(0.9, 0),
      legend.text = ggplot2::element_text(size = 30)
    )

  common_legend <- cowplot::get_legend(legend_plot)
  title <- cowplot::get_title(legend_plot)

  grouped_df <- dplyr::group_by(df, higher_level)
  split_list <- dplyr::group_split(grouped_df)
  group_names <- dplyr::pull(dplyr::group_keys(grouped_df), higher_level)

  p_list <- base::lapply(base::seq_along(split_list), function(i) {
    sub_df <- base::droplevels(split_list[[i]])

    #taxa_names <- paste0("*",base::unique(sub_df$name[sub_df$name != "GENUS"]),"*")
    taxa_names <- base::unique(sub_df$name[sub_df$name != "GENUS"])
    taxa_colors <- colors_map$pal[base::match(taxa_names, colors_map$name)]
    taxa_names_italics <- ifelse(stringr::str_detect(taxa_names,"unlabeled"),taxa_names,paste0("*",taxa_names,"*"))

    ggplot2::ggplot(sub_df) +
      ggplot2::geom_density(
        ggplot2::aes(
          x = value,
          y = ggplot2::after_stat(scaled),
          colour = pal,
          linetype = line_type
        ),
        lwd = 1.5
      ) +
      ggplot2::scale_colour_identity(
        name = "",
        breaks = taxa_colors,
        labels = taxa_names_italics,
        guide = "legend"
      ) +
      ggplot2::scale_linetype_identity(
        "Level:",
        labels = base::levels(df$level),
        breaks = c("dotted", "longdash", "solid"),
        guide = "legend"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.text = ggtext::element_markdown(size = 30),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        #legend.text = ggplot2::element_text(size = 30),
        text = ggplot2::element_text(size = 25)
      ) +
      ggplot2::guides(
        linetype = "none",
        colour = ggplot2::guide_legend(ncol = base::max(1, base::round(base::length(taxa_names) / 6)))
      ) +
      {
        if (stan_results$varname == "resting_duration") {
          ggplot2::xlim(1, 4)
        } else {
          ggplot2::xlim(0, 1)
        }
      }
  })


  base::names(p_list) <- group_names
  p_list <- p_list[base::names(p_list) != "GENUS"]

  density_plots <- cowplot::plot_grid(plotlist = p_list, ncol = 1, align = "v")

  dens <- cowplot::plot_grid(common_legend, density_plots, title,
                             ncol = 1,
                             rel_heights = c(0.1, 0.5, 0.1))

  nb_complex <- length(unique(df$higher_level[df$higher_level != "GENUS"]))

  height_per_complex <- 45.6 / 15
  total_height <- nb_complex * height_per_complex
  total_width_target <- 20

  if (!is.null(path)) {
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    ggplot2::ggsave(path,
                    plot = dens,
                    width = total_width_target,
                    height = total_height,
                    dpi = 300,
                    device = "png")
  }

  return(dens)
}


#' Extract Summary Statistics from Stan Model Results
#'
#' @description
#' This function extracts and summarizes posterior estimates from a fitted Stan model,
#' organizing the results by taxonomic levels: genus, species complex, and species.
#' It calculates means, variances, and confidence intervals, formats estimates with confidence intervals,
#' and optionally includes all species and complexes from a compatibility repository.
#' The output is a tidy data.frame suitable for further analysis or reporting.
#'
#' @param results A list containing the Stan fit results and species metadata. Must include:
#'   - `fit`: The fitted Stan model object (class `stanfit`),
#'   - `species_complex`: A data frame linking species and complexes with indices.
#' @param all Logical indicating whether to include all species and complexes from an external compatibility repository (default is TRUE).
#' @param output_dir Optional character. Directory path to save the CSV. If NULL, the plot is not saved.
#'
#' @return A data.frame with summarized posterior statistics including:
#'   - `name`: Taxonomic unit name (genus, complex, or species),
#'   - `level`: Taxonomic level ("genus", "complex", or "species"),
#'   - `mean`: Formatted estimate
#'   - `variance`: Variance of the estimate,
#'   - `ci_lower`: Lower bound of the confidence interval,
#'   - `ci_upper`: Upper bound of the confidence interval,
#'   - additional metadata columns when `all = TRUE`.
#'
#' @details
#' The function first extracts summary statistics from the Stan fit object, including means,
#' standard deviations, and confidence intervals for parameters representing genus, complex, and species levels.
#' It then maps these parameters to species and complex names using the `species_complex` data frame.
#' Unlabelled species are handled according to their complex membership.
#' When `all = TRUE`, missing species and complexes are supplemented based on a compatibility repository to ensure completeness.
#'
#'
#' @examples
#' \dontrun{
#' # Assuming `stan_results` is a list with the required components:
#' summary_df <- extract_stan_summary(
#'   results = stan_results,
#'   all = TRUE
#' )
#' }
#'
#' @export
species_complex_result <- function(results,
                                   all = FALSE,
                                   output_dir = NULL) {

  stan_fit <- results$fit
  SP <- results$species_complex
  varname <- results$varname

  posterior_samples <- do.call(rbind, stan_fit)

  summary_stats <- data.frame(
    mean = apply(posterior_samples, 2, mean),
    sd = apply(posterior_samples, 2, sd),
    lower_col = apply(posterior_samples, 2, quantile, 0.025),
    upper_col = apply(posterior_samples, 2, quantile, 0.975)
  )

  param_names <- colnames(stan_fit[[1]])
  rownames(summary_stats) <- param_names

  lower_col <- "lower_col"
  upper_col <- "upper_col"

  genus_est <- base::data.frame(
    param = "p1[1]",
    estimate = summary_stats["p1[1]", "mean"],
    variance = summary_stats["p1[1]", "sd"]^2,
    ci_lower = summary_stats["p1[1]", lower_col],
    ci_upper = summary_stats["p1[1]", upper_col],
    level = "genus",
    stringsAsFactors = FALSE
  )

  p1_idx <- base::grep("^p1\\[", param_names)
  p1_values <- summary_stats[p1_idx, c("mean", "sd", lower_col, upper_col)]
  p1_df <- base::data.frame(
    param = param_names[p1_idx],
    estimate = p1_values[, "mean"],
    variance = p1_values[, "sd"]^2,
    ci_lower = p1_values[, lower_col],
    ci_upper = p1_values[, upper_col],
    level = "complex",
    stringsAsFactors = FALSE
  )

  p2_idx <- base::grep("^p2\\[", param_names)
  p2_values <- summary_stats[p2_idx, c("mean", "sd", lower_col, upper_col)]
  p2_df <- base::data.frame(
    param = param_names[p2_idx],
    estimate = p2_values[, "mean"],
    variance = p2_values[, "sd"]^2,
    ci_lower = p2_values[, lower_col],
    ci_upper = p2_values[, upper_col],
    level = "species",
    stringsAsFactors = FALSE
  )

  complex_map <- SP |>
    dplyr::filter(!base::is.na(complexNb)) |>
    dplyr::distinct(complexNb, complex) |>
    dplyr::mutate(param = base::paste0("p1[", complexNb, "]"))

  species_map <- SP |>
    dplyr::filter(!base::is.na(speciesNb)) |>
    dplyr::distinct(speciesNb, species) |>
    dplyr::mutate(param = base::paste0("p2[", speciesNb, "]"))

  p1_df <- p1_df |>
    dplyr::left_join(complex_map, by = "param") |>
    dplyr::rename(name = complex)

  p2_df <- p2_df |>
    dplyr::left_join(species_map, by = "param") |>
    dplyr::rename(name = species)

  genus_est$name <- "Anopheles"

  res <- dplyr::bind_rows(genus_est, p1_df, p2_df) |>
    dplyr::select(name, level, estimate, variance, ci_lower, ci_upper)

  res_clean <- res |>
    dplyr::filter(!base::grepl("unlabeled", name, ignore.case = TRUE))

  res_clean <- res_clean |>
    dplyr::mutate(
      name = base::gsub("_complex", "", name)
    )

  if (all) {
    compat_repo <- read_data_file("compatibility_repo_taxonomy_v2.csv", sep = ";")


    res_clean <- res_clean |>
      dplyr::mutate(
        data = TRUE)

    for (i in base::seq_len(base::nrow(compat_repo))) {
      sp <- compat_repo$species[i]
      cx <- compat_repo$complex[i]

      if (sp != "" && !sp %in% res_clean$name) {
        complex_row <- res_clean |>
          dplyr::filter(name == cx, level == "complex")

        if (base::nrow(complex_row) > 0) {
          new_row <- complex_row |>
            dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE            )
          res_clean <- dplyr::bind_rows(res_clean, new_row)
          next
        }

        genus_row <- res_clean |>
          dplyr::filter(level == "genus")

        if (base::nrow(genus_row) == 1) {
          new_row <- genus_row |>
            dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE
            )
          res_clean <- dplyr::bind_rows(res_clean, new_row)
        } else {
          base::warning(base::paste("Unable to complete species", sp, ": genus row missing or multiple."))
        }
      }

      if (sp == "" && !cx %in% res_clean$name) {
        genus_row <- res_clean |>
          dplyr::filter(level == "genus")

        if (base::nrow(genus_row) == 1) {
          new_row <- genus_row |>
            dplyr::mutate(
              name = cx,
              level = "complex",
              data = FALSE
            )
          res_clean <- dplyr::bind_rows(res_clean, new_row)
        } else {
          base::warning(base::paste("Unable to complete complex", cx, ": genus row missing or multiple."))
        }
      }
    }
  }
  res_clean <- res_clean|>
    dplyr::filter(!is.na(name))
  if (!is.null(output_dir)) {
    filename <- base::paste0("stan_summary", varname, ".csv")
    output_path <- base::file.path(output_dir, filename)
    write.csv(res_clean, output_path, row.names = FALSE)
    return(res_clean)
  }

  return(res_clean)

}




#' Estimate Species-Specific Bionomic Traits for Anopheles Using a Hierarchical Bayesian Framework
#'
#' This function estimates key bionomic traits of *Anopheles* mosquitoes using a set of
#' hierarchical Bayesian models fitted independently for each trait across three taxonomic
#' levels: genus, species complex, and species. The estimated traits include parous rate,
#' endophagy, endophily, sac rate, indoor and outdoor human blood indices, and resting duration.
#'
#' For human blood feeding behaviour, indoor and outdoor human blood indices are first estimated
#' separately. The overall human blood index (HBI, denoted \code{Chi}) is then derived a posteriori
#' as a deterministic function of endophagy:
#' \deqn{
#'   Chi = indoor\_HBI \times endophagy + outdoor\_HBI \times (1 - endophagy)
#' }
#' Indoor and outdoor HBI estimates are not returned in the final output.
#'
#' Resting duration is estimated by the model and returned as the parameter \code{tau},
#' preserving compatibility with downstream transmission models.
#'
#' Internally, the function relies on \code{creation_df()} to format input data for each trait and
#' \code{run_stan()} to perform Bayesian inference using Stan. Posterior summaries are aggregated
#' across taxonomic levels using an internal helper function, and missing species or complexes are
#' imputed using information from higher taxonomic levels when necessary.
#'
#' @return A \code{data.frame} containing bionomic estimates for each species, species complex,
#' or genus, with the following columns:
#'   \describe{
#'     \item{species_name}{Name of the species, complex, or genus (e.g., \code{"Anopheles gambiae"},
#'       \code{"Gambiae complex"}, \code{"GENUS"}).}
#'     \item{M, M.sd}{Parous rate and its standard deviation.}
#'     \item{endophagy, endophagy.sd}{Endophagy level and its standard deviation.}
#'     \item{endophily, endophily.sd}{Endophily level and its standard deviation.}
#'     \item{A0, A0.sd}{Sac rate and its standard deviation.}
#'     \item{Chi}{Overall human blood index (HBI), derived from indoor and outdoor components.}
#'     \item{tau, tau.sd}{Resting duration and its standard deviation.}
#'   }
#'
#' @details
#' Missing species-level estimates are imputed using available information at the species-complex
#' or genus level, following a predefined taxonomic compatibility table. Species and complex names
#' are normalised (e.g., mapping \code{"Anopheles gambiae s.s. / Colluzi"} to
#' \code{"Anopheles gambiae"}, and genus-level estimates are labelled as \code{"GENUS"}).
#' A synthetic \code{"Jamesii complex"} row is added based on genus-level estimates to ensure
#' completeness of the output.
#'
#' @note
#' This function assumes that \code{creation_df()} and \code{run_stan()} are available in the
#' environment and return appropriately formatted data and fitted Stan model objects.
#'
#' @export
Bionomics_For_AnopholesModel <- function() {


  species_complex_result_ano_model <- function(results, all = TRUE) {

    stan_fit <- results$fit
    SP <- results$species_complex
    varname <- results$varname

    posterior_samples <- do.call(rbind, stan_fit)

    summary_stats <- data.frame(
      mean = apply(posterior_samples, 2, mean),
      sd = apply(posterior_samples, 2, sd)
    )

    param_names <- colnames(stan_fit[[1]])
    rownames(summary_stats) <- param_names

    genus_est <- base::data.frame(
      param = "p1[1]",
      estimate = summary_stats["p1[1]", "mean"],
      variance = summary_stats["p1[1]", "sd"]^2,
      level = "genus",
      stringsAsFactors = FALSE
    )

    p1_idx <- base::grep("^p1\\[", param_names)
    p1_df <- base::data.frame(
      param = param_names[p1_idx],
      estimate = summary_stats[p1_idx, "mean"],
      variance = summary_stats[p1_idx, "sd"]^2,
      level = "complex",
      stringsAsFactors = FALSE
    )

    p2_idx <- base::grep("^p2\\[", param_names)
    p2_df <- base::data.frame(
      param = param_names[p2_idx],
      estimate = summary_stats[p2_idx, "mean"],
      variance = summary_stats[p2_idx, "sd"]^2,
      level = "species",
      stringsAsFactors = FALSE
    )

    complex_map <- SP |>
      dplyr::filter(!is.na(complexNb)) |>
      dplyr::distinct(complexNb, complex) |>
      dplyr::mutate(param = paste0("p1[", complexNb, "]"))

    species_map <- SP |>
      dplyr::filter(!is.na(speciesNb)) |>
      dplyr::distinct(speciesNb, species) |>
      dplyr::mutate(param = paste0("p2[", speciesNb, "]"))

    p1_df <- p1_df |>
      dplyr::left_join(complex_map, by = "param") |>
      dplyr::rename(name = complex)

    p2_df <- p2_df |>
      dplyr::left_join(species_map, by = "param") |>
      dplyr::rename(name = species)

    genus_est$name <- "Anopheles"

    res <- dplyr::bind_rows(genus_est, p1_df, p2_df) |>
      dplyr::select(name, level, estimate, variance)

    species_to_complex <- SP |>
      dplyr::filter(!is.na(species), !is.na(complex)) |>
      dplyr::distinct(species, complex)

    res <- res |>
      dplyr::left_join(species_to_complex, by = c("name" = "species")) |>
      dplyr::mutate(complex = dplyr::if_else(level == "species", complex, NA_character_))

    res_clean <- res |>
      dplyr::filter(!grepl("complex", name, ignore.case = TRUE))

    unlabelled_rows <- res_clean |>
      dplyr::filter(grepl("unlabeled", name, ignore.case = TRUE))

    for (i in seq_len(nrow(unlabelled_rows))) {
      unlabel_name <- unlabelled_rows$name[i]
      sp_info <- SP |> dplyr::filter(species == unlabel_name)

      if (nrow(sp_info) == 1) {
        complex_name <- sp_info$complex
        n_species <- SP |> dplyr::filter(complex == complex_name) |> nrow()

        if (n_species == 1) {
          existing_complex_row <- res_clean |>
            dplyr::filter(name == complex_name, level == "complex")

          if (nrow(existing_complex_row) == 1) {
            new_row <- unlabelled_rows[i, ]
            new_row$name <- complex_name
            new_row$complex <- NA_character_
            new_row$level <- "complex"

            res_clean <- res_clean |> dplyr::filter(!(name == complex_name & level == "complex"))
            res_clean <- dplyr::bind_rows(res_clean, new_row)
          } else {
            res_clean <- res_clean |> dplyr::mutate(
              level = dplyr::if_else(name == unlabel_name, "complex", level),
              name  = dplyr::if_else(name == unlabel_name, complex_name, name),
              complex = dplyr::if_else(name == complex_name, NA_character_, complex)
            )
          }
        } else {
          res_clean <- res_clean |> dplyr::filter(name != unlabel_name)
        }
      } else {
        res_clean <- res_clean |> dplyr::filter(name != unlabel_name)
      }
    }

    res_clean <- res_clean |> dplyr::filter(!grepl("unlabeled", name, ignore.case = TRUE))
    res_clean <- res_clean |>
      dplyr::mutate(
        name = base::gsub("_complex", "", name)
      )
    if (all) {
      compat_repo <- read_data_file("compatibility_repo_taxonomy_v2.csv", sep = ";")

      if (!"complex" %in% names(res_clean)) {
        res_clean$complex <- NA_character_
      }


      for (i in seq_len(nrow(compat_repo))) {
        sp <- compat_repo$species[i]
        cx <- compat_repo$complex[i]

        if (sp != "" && !sp %in% res_clean$name) {
          complex_row <- res_clean |> dplyr::filter(name == cx, level == "complex")
          if (nrow(complex_row) > 0) {
            new_row <- complex_row |> dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE
            )
            res_clean <- dplyr::bind_rows(res_clean, new_row)
            next
          }
          genus_row <- res_clean |> dplyr::filter(level == "genus")
          if (nrow(genus_row) == 1) {
            new_row <- genus_row |> dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE
            )
            res_clean <- dplyr::bind_rows(res_clean, new_row)
          } else {
            warning(paste("Unable to complete species", sp, ": genus row missing or multiple."))
          }
        }

        if (sp == "" && !cx %in% res_clean$name) {
          genus_row <- res_clean |> dplyr::filter(level == "genus")
          if (nrow(genus_row) == 1) {
            new_row <- genus_row |> dplyr::mutate(
              name = cx,
              level = "complex",
              data = FALSE
            )
            res_clean <- dplyr::bind_rows(res_clean, new_row)
          } else {
            warning(paste("Unable to complete complex", cx, ": genus row missing or multiple."))
          }
        }
      }
    }

      res_clean <- res_clean |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Anopheles gambiae s.s. / Colluzi" ~ "Anopheles gambiae",
          name == "Anopheles gambiae" ~ "Gambiae complex",
          name == "Anopheles" ~ "GENUS",
          level == "complex" ~ paste0(name, " complex"),
          TRUE ~ name
        )
      )

    genus_row <- res_clean |>
      dplyr::filter(name == "GENUS")


    jamesii_row <- genus_row |> dplyr::mutate(name = "Jamesii complex")
    res_clean <- dplyr::bind_rows(res_clean, jamesii_row)

    out <- res_clean |>
      dplyr::select(name, estimate, variance)


    colnames(out)[colnames(out) == "estimate"] <- varname
    colnames(out)[colnames(out) == "variance"] <- paste0(varname, "_variance")


    return(out)
  }



  varnames <- c(
    "parous_rate",
    "endophagy",
    "endophily",
    "sac_rate",
    "indoor_HBI",
    "outdoor_HBI",
    "resting_duration"
  )

  name_map <- list(
    parous_rate       = c("M", "M.sd"),
    endophagy         = c("endophagy", "endophagy.sd"),
    endophily         = c("endophily", "endophily.sd"),
    sac_rate          = c("A0", "A0.sd"),
    indoor_HBI        = c("indoor_HBI", "indoor_HBI.sd"),
    outdoor_HBI       = c("outdoor_HBI", "outdoor_HBI.sd"),
    resting_duration  = c("tau", "tau.sd")
  )

  final_df <- NULL

  for (v in varnames) {

    resultats <- creation_df(v)
    fit <- run_stan(resultats)

    res <- species_complex_result_ano_model(fit)

    res <- res |>
      dplyr::mutate(sd = sqrt(.data[[paste0(v, "_variance")]])) |>
      dplyr::select(name, !!v, sd)

    colnames(res) <- c("species_name", name_map[[v]])

    final_df <- if (is.null(final_df)) res else
      dplyr::left_join(final_df, res, by = "species_name")
  }

  genus_row <- which(final_df$species_name == "GENUS")

  final_df[] <- lapply(final_df, function(x) {
    x[is.na(x)] <- x[genus_row]
    x
  })

  final_df <- final_df |>
    dplyr::mutate(
      Chi = indoor_HBI * endophagy +
        outdoor_HBI * (1 - endophagy)
    )

  final_df <- final_df |>
    dplyr::select(
      -indoor_HBI, -indoor_HBI.sd,
      -outdoor_HBI, -outdoor_HBI.sd
    )

  final_df$zeta.3 <- 1
  final_df$td <- 0.33
  final_df$ts <- 10
  final_df$to <- 5

  final_df <- final_df |>
    dplyr::select(
      species_name,
      M, M.sd,
      Chi,
      A0, A0.sd,
      zeta.3, td,
      tau, tau.sd,
      ts, to,
      endophily, endophily.sd,
      endophagy, endophagy.sd
    )

  return(final_df)
}




#' Generate Multi-Species Pie Charts
#'
#' This function creates pie charts for multiple species-related variables, visualizing
#' the proportion of observations for each species. It handles complex species labels,
#' groups minor proportions under "Other", and applies a custom color palette for each species.
#'
#' @param seuil_prop_autres Numeric, default 0.05. Threshold for grouping low-proportion species into "Other".
#' @param plot_dir Character, default "c"path/to/your/folder/". Directory where the output PNG file will be saved.
#'
#' @details
#' The function processes a set of predefined variables (`endophagy`, `endophily`,
#' `indoor_HBI`, `outdoor_HBI`, `parous_rate`, `sac_rate`) and generates a pie chart
#' for each. Complex species names are identified and labeled separately. Colors are
#' assigned using a custom palette loaded from a CSV file (`new_palette_density_plots.csv`).
#' All generated plots are combined into a single grid with a shared legend.
#'
#' The output is saved as a PNG file named "multi_species_piechart.png" in the specified directory.
#'
#' @return
#' The function does not return a value but saves the plot as a PNG file.
#'
#'
#' @export
multi_species_pie <- function(seuil_prop_autres = 0.05,
                              plot_dir = "path/to/your/folder/") {
  varnames <- c("endophagy", "endophily",
                "indoor_HBI", "outdoor_HBI", "parous_rate", "sac_rate", "resting_duration")

  colors_map <- read_data_file(file = "new_palette_density_plots.csv")

  all_data_clean_list <- list()

  plot_for_var <- function(varname) {
    prepared <- creation_df(varname = varname)
    data.req <- prepared$data.req
    nice_varname <- prepared$nice_varname
    obs <- paste0(varname, ".den")

    data_clean <- data.req |>
      dplyr::mutate(
        is_complex = grepl("^unlabeled ", species),
        species_clean = ifelse(is_complex, sub("^unlabeled ", "", species), species)
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
        label_pct = paste0(label, " : ", round(prop * 100), "%"),
        label = factor(label, levels = label)
      )

    all_data_clean_list[[varname]] <<- data_clean

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

  plots <- base::lapply(varnames, plot_for_var)

  legend_data <- dplyr::bind_rows(all_data_clean_list)
  legend_df <- legend_data |>
    dplyr::distinct(label, pal) |>
    dplyr::arrange(label) |>
    dplyr::mutate(label0=label,
                  label=ifelse(label0=="Other", "Other",
                               paste0(gsub("Species: ", "Species: *",
                                           gsub("Complex: ", "Complex: *", label)), "*")))

  legend_plot <- ggplot2::ggplot(legend_df, ggplot2::aes(x = 1, y = label, fill = label)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = setNames(legend_df$pal, legend_df$label)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown(size = 50),
      legend.key.size = ggplot2::unit(3, "lines"),
      legend.spacing.y = ggplot2::unit(30, "lines")
    )

  legend <- cowplot::get_legend(legend_plot)

  empty_plot <- ggplot2::ggplot() + ggplot2::theme_void()
  plots_with_empty <- c(plots, list(empty_plot))

  plot_grid_final <- gridExtra::grid.arrange(
    cowplot::plot_grid(plotlist = plots_with_empty, ncol = 2, nrow = 4,
                       rel_widths = c(1, 1), rel_heights = rep(1.2, 4), hjust = 0),
    legend,
    ncol = 2,
    widths = c(1.9, 1.1)
  )

  outfile <- base::file.path(plot_dir, "multi_species_piechart.png")
  ggplot2::ggsave(outfile, plot = plot_grid_final, width = 40, height = 40, dpi = 300)
  base::message("Plot saved at: ", outfile)
}

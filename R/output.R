#' Generate a Pie Chart of Observations by Species or Complex
#'
#' @description
#' This function creates an interactive pie chart that shows the distribution
#' of observations by species or species complexes, based on a specified
#' observation variable from a pre-processed dataset. It optionally groups
#' species with a small proportion of observations into a single "Other" category,
#' and can save the plot as an HTML file for sharing or embedding.
#'
#' @param data A list containing the following named elements:
#'   - `data.req`: A data frame with observation data. Must contain a column named
#'     after the pattern `"<varname>.den"` (e.g., `"psrous_rate.den"`), which stores
#'     the observation counts per species.
#'   - `varname`: A character string indicating the base name of the observation variable
#'     (used to identify the column `<varname>.den` in the data).
#'   - `nice_varname`: A character string used for naming the saved output file.
#'
#' @param threshold_prop_other A numeric threshold (between 0 and 1) indicating the
#' minimum proportion of total observations required for a species or complex to appear
#' as its own slice in the pie chart. Species below this threshold are grouped under "Other".
#' Default is `0.05` (1%).
#'
#' @param plot_dir Optional. A character string giving the directory path where
#' the HTML version of the pie chart should be saved. If `NULL`, the chart is not saved.
#' Default is a specific folder on the user's system.
#'
#' @return An interactive pie chart (`plotly` object) showing the distribution of
#' observations by species or species complex. If `plot_dir` is provided, the chart is also
#' saved as an HTML file.
#'
#' @details
#' - Species names starting with `"unlabel_"` are treated as complexes and labeled accordingly.
#' - The observation variable used in the calculation must match the pattern `<varname>.den`.
#' - The pie chart uses `plotly` for interactive visualization and `htmlwidgets` to export.
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
                              is_complex = grepl("^unlabel_", species),
                              species_clean = ifelse(is_complex, sub("^unlabel_", "", species), species)) |>
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
          "Proportion: ", round_fun(prop * 100, 1), "%"
        ),
        label_text = paste0(label_grouped, " · ", round_fun(prop * 100), "%")
      )

    message("Proportion de la catégorie 'Other' : ",
            round_fun(sum_fun(data_grouped$prop[data_grouped$label_grouped == "Other"]) * 100, 2), "%")
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
          "Proportion: ", round_fun(prop * 100, 1), "%"
        ),
        label_text = paste0(label_grouped, " · ", round_fun(prop * 100), "%")
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
#'     and an observation count column named `{varname}.den`.
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
      label_text = paste0(label_grouped, " · ", round_fun(prop * 100), "%")
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
#' @param samples An object of class `mcmc.list` (e.g., output from `coda::mcmc.list`),
#'    containing posterior samples for species- and complex-level parameters (named `p1[...]` and `p2[...]`).
#' @param chain_length Integer. Total number of MCMC iterations per chain.
#' @param burnin Integer. Number of initial iterations to discard as burn-in.
#' @param thinning Integer. Thinning interval used during sampling (e.g., every 10th draw).
#' @param species_complex A `data.frame` mapping species and complexes. Must include:
#' \itemize{
#'   \item `species`: species names,
#'   \item `complex`: complex names,
#'   \item `speciesNb`: integer codes for species (matching Stan),
#'   \item `complexNb`: integer codes for complexes (matching Stan).
#' }
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

  fit <- stan_results$fit
  species_complex <- stan_results$species_complex
  nice_varname <- stan_results$nice_varname
  iter <- stan_results$iter
  thinning <- stan_results$thinning
  warmup <- stan_results$warmup

  samples_all <- rstan::As.mcmc.list(fit)
  samples <- coda::mcmc.list(lapply(samples_all, function(chain_mcmc) {
    coda::mcmc(chain_mcmc[, base::grepl("^p1|^p2", base::colnames(chain_mcmc))])
  }))

  samples.df <- base::as.data.frame(base::as.matrix(samples), header = TRUE)[
    (warmup / thinning + 1):(iter / thinning),
  ]

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

  target_species <- dplyr::filter(species_complex, base::grepl("unlabel", species, ignore.case = TRUE))

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
#' @param genus Logical. Adjusts vertical size of the plot. Default is TRUE.
#' @param complex_names Optional character vector. Names of species complexes to include in the plot. If NULL, all are included.
#'
#' @export
plot_density <- function(stan_results,
                         unlabel = FALSE,
                         plot_dir = NULL,
                         complex_names = NULL) {

  fit <- stan_results$fit
  species_complex <- stan_results$species_complex
  nice_varname <- stan_results$nice_varname


  df <- prepare_data_density(stan_results)

  if (unlabel) {
    df <- dplyr::filter(df, !base::grepl("unlabel", name, ignore.case = TRUE))
  }

  if (!is.null(complex_names)) {
    df <- dplyr::filter(df, higher_level %in% c(complex_names,"GENUS"))

  }

  duplicated_genus <- dplyr::filter(df, name == "GENUS")
  duplicated_genus <- duplicated_genus[base::rep(1:base::nrow(duplicated_genus), each = base::length(base::unique(df$higher_level))), ]
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
    base::warning("The following names do not have a defined color in palette_name : ", base::paste(missing_names, collapse = ", "))
    colors_map <- dplyr::bind_rows(
      colors_map,
      data.frame(name = missing_names, pal = "#D3D3D3")
    )
  }

  df <- dplyr::left_join(df, colors_map, by = "name")

  legend_plot <- ggplot2::ggplot(df) +
    ggplot2::geom_density(ggplot2::aes(x = value, linetype = line_type), lwd = 1.5, stat = "density", color = "black") +
    ggplot2::scale_linetype_identity("Level:", labels = base::levels(df$level), breaks = c("dotted", "longdash", "solid"), guide = "legend") +
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

    taxa_names <- base::unique(sub_df$name[sub_df$name != "GENUS"])
    taxa_colors <- colors_map$pal[base::match(taxa_names, colors_map$name)]

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
      ggplot2::expand_limits(x = c(0, 1)) +
      ggplot2::scale_colour_identity(
        name = "",
        breaks = taxa_colors,
        labels = taxa_names,
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
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 30),
        text = ggplot2::element_text(size = 25)
      ) +
      ggplot2::guides(
        linetype = "none",
        colour = ggplot2::guide_legend(ncol = base::max(1, base::round(base::length(taxa_names) / 6)))
      )
  })

  base::names(p_list) <- group_names

  p_list <- p_list[base::names(p_list) != "GENUS"]

  density_plots <- cowplot::plot_grid(plotlist = p_list, ncol = 1, align = "v")

  dens <- cowplot::plot_grid(common_legend, density_plots, title,
                             ncol = 1,
                             rel_heights = c(0.1, 0.5, 0.1))

  if (!is.null(plot_dir)) {
    output_path <- base::file.path(plot_dir, base::paste0(nice_varname, "density.png"))

    nb_complex <- length(unique(df$higher_level[df$higher_level != "GENUS"]))
    height_per_plot <- 3
    total_height <- 0.1 * height_per_plot + nb_complex * height_per_plot + 0.1 * height_per_plot

    ggplot2::ggsave(output_path,
                    plot = dens,
                    width = 20,
                    height = total_height,
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
#' @param ci_level Numeric value between 0 and 1 specifying the confidence interval level (default is 0.95).
#' @param all Logical indicating whether to include all species and complexes from an external compatibility repository (default is TRUE).
#'
#' @return A data.frame with summarized posterior statistics including:
#'   - `name`: Taxonomic unit name (genus, complex, or species),
#'   - `level`: Taxonomic level ("genus", "complex", or "species"),
#'   - `estimate_ci`: Formatted estimate with confidence interval as a percentage string,
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
#'   ci_level = 0.95,
#'   all = TRUE
#' )
#' }
#'
#' @export
species_complex_result <- function(results,
                                   ci_level = 0.95,
                                   all = TRUE,
                                   output_dir = NULL) {

  stan_fit <- results$fit
  SP <- results$species_complex
  varname <- results$varname

  summary_stats <- rstan::summary(stan_fit)$summary
  param_names <- base::rownames(summary_stats)

  alpha <- 1 - ci_level
  lower_col <- "2.5%"
  upper_col <- "97.5%"

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
    dplyr::select(name, level, estimate, variance, ci_lower, ci_upper) |>
    dplyr::mutate(
      margin_error = base::pmax(ci_upper - estimate, estimate - ci_lower),
      estimate_pct = estimate * 100,
      margin_error_pct = margin_error * 100,
      estimate_ci = base::paste0(
        base::round(estimate_pct, 2), "% ± ", base::round(margin_error_pct, 2), "% (CI ", ci_level * 100, "%)"
      )
    ) |>
    dplyr::select(name, level, estimate_ci, variance, ci_lower, ci_upper)

  species_to_complex <- SP |>
    dplyr::filter(!base::is.na(species), !base::is.na(complex)) |>
    dplyr::distinct(species, complex)

  res <- res |>
    dplyr::left_join(species_to_complex, by = c("name" = "species")) |>
    dplyr::mutate(complex = dplyr::if_else(level == "species", complex, NA_character_))

  res_clean <- res |>
    dplyr::filter(!base::grepl("complex", name, ignore.case = TRUE))
  unlabelled_rows <- res_clean |>
    dplyr::filter(base::grepl("unlabel", name, ignore.case = TRUE))

  for (i in base::seq_len(base::nrow(unlabelled_rows))) {
    unlabel_name <- unlabelled_rows$name[i]
    sp_info <- SP |>
      dplyr::filter(species == unlabel_name)

    if (base::nrow(sp_info) == 1) {
      complex_name <- sp_info$complex
      n_species <- SP |>
        dplyr::filter(complex == complex_name) |>
        base::nrow()

      if (n_species == 1) {
        existing_complex_row <- res_clean |>
          dplyr::filter(name == complex_name, level == "complex")

        if (base::nrow(existing_complex_row) == 1) {
          new_row <- unlabelled_rows[i, ]
          new_row$name <- complex_name
          new_row$complex <- NA_character_
          new_row$level <- "complex"

          res_clean <- res_clean |>
            dplyr::filter(!(name == complex_name & level == "complex"))
          res_clean <- dplyr::bind_rows(res_clean, new_row)

        } else {
          res_clean <- res_clean |>
            dplyr::mutate(
              level = dplyr::if_else(name == unlabel_name, "complex", level),
              name  = dplyr::if_else(name == unlabel_name, complex_name, name),
              complex = dplyr::if_else(name == complex_name, NA_character_, complex)
            )
        }
      } else {
        res_clean <- res_clean |>
          dplyr::filter(name != unlabel_name)
      }
    } else {
      res_clean <- res_clean |>
        dplyr::filter(name != unlabel_name)
    }
  }

  res_clean <- res_clean |>
    dplyr::filter(!base::grepl("unlabel", name, ignore.case = TRUE))

  if (all) {

    compat_repo <- read_data_file("compatibility_repo_taxonomy_v2.csv", sep = ";")

    if (!"complex" %in% base::names(res_clean)) {
      res_clean$complex <- NA_character_
    }

    res_clean <- res_clean |>
      dplyr::mutate(
        data = TRUE,
        source_flag = base::as.integer(NA)
      )

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
              data = FALSE,
              source_flag = 1L,
              complex = cx
            )
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
              data = FALSE,
              source_flag = 3L,
              complex = cx
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
              data = FALSE,
              source_flag = 4L,
              complex = NA_character_
            )
          res_clean <- dplyr::bind_rows(res_clean, new_row)
        } else {
          base::warning(base::paste("Unable to complete complex", cx, ": genus row missing or multiple."))
        }
      }
    }
  }
  if (!is.null(output_dir)) {
      filename <- base::paste0("stan_summary", varname, ".csv")
      output_path <- base::file.path(output_dir, filename)
      write.csv(res_clean, output_path, row.names = FALSE)
      return(res_clean)
  }
  return(res_clean)
}













#' Estimate Species-Specific Bionomic Traits for Anopheles Using a Hierarchical Bayesian Model
#'
#' This function estimates various bionomic parameters (e.g., parous rate, endophagy, endophily,
#' sac rate, and human blood index) for Anopheles mosquitoes at three taxonomic levels: genus,
#' species complex, and species. It fits a hierarchical Bayesian model for each trait using Stan,
#' and merges the results into a single tidy data frame.
#'
#' Internally, it calls `creation_df()` to prepare input data for each trait, and `run_stan()` to
#' perform Bayesian inference using Stan. The posterior summaries are processed by an internal function
#' to aggregate results across levels and to impute missing species based on known complexes or genus-level data.
#'
#' @return A `data.frame` containing bionomic estimates for each species or complex, with the following columns:
#'   \describe{
#'     \item{species_name}{Name of the species, complex, or genus (e.g., `"Anopheles gambiae"`, `"Gambiae complex"`, `"GENUS"`).}
#'     \item{M, M.sd}{Parous rate and its standard deviation.}
#'     \item{endophagy, endophagy.sd}{Endophagy level and its standard deviation.}
#'     \item{endophily, endophily.sd}{Endophily level and its standard deviation.}
#'     \item{A0, A0.sd}{Sac rate and its standard deviation.}
#'     \item{Chi}{Human blood index (HBI)}
#'   }
#'
#' @details
#' The function handles missing data by imputing estimates based on available data at the complex or genus level.
#' It also performs name normalization (e.g., mapping `"Anopheles gambiae s.s."` to `"Anopheles gambiae"` and
#' `"Anopheles gambiae"` to `"Gambiae complex"`). A synthetic `"Jamesii complex"` row is added based on genus-level data.
#'
#' A side-effect is the use of `print()` statements during model fitting to indicate progress.
#'
#' @note
#' This function assumes that `creation_df()` and `run_stan()` are defined in the environment and return appropriately
#' formatted data and model results.
#'
#'
#' @export
Bionomics_For_Anophles_Model <- function() {


  species_complex_result_ano_model <- function(results,
                                               all = TRUE,
                                               output_dir = NULL) {

    stan_fit <- results$fit
    SP <- results$species_complex
    varname <- results$varname

    summary_stats <- rstan::summary(stan_fit)$summary
    param_names <- base::rownames(summary_stats)

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
      dplyr::filter(grepl("unlabel", name, ignore.case = TRUE))

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

    res_clean <- res_clean |> dplyr::filter(!grepl("unlabel", name, ignore.case = TRUE))

    if (all) {
      compat_repo <- read_data_file("compatibility_repo_taxonomy_v2.csv", sep = ";")

      if (!"complex" %in% names(res_clean)) {
        res_clean$complex <- NA_character_
      }

      res_clean <- res_clean |> dplyr::mutate(data = TRUE, source_flag = NA_integer_)

      for (i in seq_len(nrow(compat_repo))) {
        sp <- compat_repo$species[i]
        cx <- compat_repo$complex[i]

        if (sp != "" && !sp %in% res_clean$name) {
          complex_row <- res_clean |> dplyr::filter(name == cx, level == "complex")
          if (nrow(complex_row) > 0) {
            new_row <- complex_row |> dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE,
              source_flag = 1L,
              complex = cx
            )
            res_clean <- dplyr::bind_rows(res_clean, new_row)
            next
          }
          genus_row <- res_clean |> dplyr::filter(level == "genus")
          if (nrow(genus_row) == 1) {
            new_row <- genus_row |> dplyr::mutate(
              name = sp,
              level = "species",
              data = FALSE,
              source_flag = 3L,
              complex = cx
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
              data = FALSE,
              source_flag = 4L,
              complex = NA_character_
            )
            res_clean <- dplyr::bind_rows(res_clean, new_row)
          } else {
            warning(paste("Unable to complete complex", cx, ": genus row missing or multiple."))
          }
        }
      }
    }
    # Nettoyage du nom selon les règles demandées
    res_clean <- res_clean |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Anopheles gambiae s.s." ~ "Anopheles gambiae",
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


    idx_jamesii <- which(datafra$species_name == "Jamesii complex")
    if (length(idx_jamesii) > 1) {
       idx_to_remove <- sample(idx_jamesii, 1)
        datafra <- datafra[-idx_to_remove, ]
        }

    return(out)
  }



  varnames <- c("parous_rate", "endophagy", "endophily", "sac_rate", "HBI")

  final_df <- NULL

  name_map <- list(
    parous_rate = c("M", "M.sd"),
    endophagy = c("endophagy", "endophagy.sd"),
    endophily = c("endophily", "endophily.sd"),
    sac_rate = c("A0", "A0.sd"),
    HBI = c("Chi", "Chi.sd")
  )

  for (v in varnames) {
    resultats <- creation_df(v)
    print(paste0("Fitting model for ", v))
    fit <- run_stan(resultats)

    res <- species_complex_result_ano_model(fit)

    res <- res |>
      dplyr::mutate(
        sd = sqrt(!!rlang::sym(paste0(v, "_variance")))
      ) |>
      dplyr::select(name, !!rlang::sym(v), sd)

    new_names <- name_map[[v]]
    colnames(res) <- c("species_name", new_names)

    if (is.null(final_df)) {
      final_df <- res
    } else {
      final_df <- dplyr::left_join(final_df, res, by = "species_name")
    }
  }

  df_final <- datafra |>
      dplyr::select(-Chi.sd)

  return(final_df)
}




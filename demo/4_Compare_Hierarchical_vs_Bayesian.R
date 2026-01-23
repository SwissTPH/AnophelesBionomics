library(AnophelesBionomics)
library(dplyr)
library(ggplot2)
path_output  = "C:/Users/chamcl/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Bionomics_estimation_v5/Figures/"


obs_complex_species_table <- function(data) {
  sd_fun <- sd
  mean_fun <- mean
  sum_fun <- base::sum
  round_fun <- base::round

  data.req <- data$data.req
  varname <- data$varname
  obs <- paste0(varname, ".den")
  colors_map <- read_data_file(file = "new_palette_density_plots.csv")

  data_clean <- dplyr::mutate(
    data.req,
    is_complex = grepl("^unlabeled ", species),
    species_clean = species,
    type = dplyr::case_when(
      is_complex ~ "Complex",
      grepl("^genus_", species_clean, ignore.case = TRUE) ~ "Genus",
      TRUE ~ "Species"
    )
  ) |>
    dplyr::group_by(species_clean, type) |>
    dplyr::summarise(
      mean_value = mean_fun(value, na.rm = TRUE),
      sd_value = sd_fun(value, na.rm = TRUE),
      .groups = "drop"
    )


  summary_table <- data_clean |>
    dplyr::transmute(
      Name = species_clean,
      Type = type,
      Mean = round_fun(mean_value, 2),
      SD = round_fun(sd_value, 2)
    ) |>
    dplyr::arrange(Type, Name)

  return(summary_table)
}
#tab <- obs_complex_species_table(data)

stan_species_complex_table <- function(results, SP) {
  stan_fit <- extract_HPD_mcmc(results)

  posterior_samples <- do.call(rbind, stan_fit)

  summary_stats <- data.frame(
    mean = apply(posterior_samples, 2, mean),
    sd = apply(posterior_samples, 2, sd)
  )

  param_names <- colnames(stan_fit[[1]])
  rownames(summary_stats) <- param_names


  genus_row <- data.frame(
    name = "Anopheles",
    level = "genus",
    mean = summary_stats["p1[1]", "mean"],
    sd   = summary_stats["p1[1]", "sd"],
    stringsAsFactors = FALSE
  )

  p1_idx_all <- grep("^p1\\[", param_names)
  p1_idx <- p1_idx_all[param_names[p1_idx_all] != "p1[1]"]
  if (length(p1_idx) > 0) {
    p1_vals <- summary_stats[p1_idx, c("mean", "sd"), drop = FALSE]
    p1_df <- data.frame(
      param = param_names[p1_idx],
      mean  = p1_vals[ , "mean"],
      sd    = p1_vals[ , "sd"],
      stringsAsFactors = FALSE
    )
    complex_map <- SP |>
      dplyr::filter(!is.na(complexNb)) |>
      dplyr::distinct(complexNb, complex) |>
      dplyr::mutate(param = paste0("p1[", complexNb, "]"))
    p1_df <- p1_df |>
      dplyr::left_join(complex_map, by = "param") |>
      dplyr::rename(name = complex) |>
      dplyr::mutate(level = "complex") |>
      dplyr::select(name, level, mean, sd)
  } else {
    p1_df <- dplyr::tibble(name = character(), level = character(), mean = numeric(), sd = numeric())
  }

  p2_idx <- grep("^p2\\[", param_names)
  if (length(p2_idx) > 0) {
    p2_vals <- summary_stats[p2_idx, c("mean", "sd"), drop = FALSE]
    p2_df <- data.frame(
      param = param_names[p2_idx],
      mean  = p2_vals[ , "mean"],
      sd    = p2_vals[ , "sd"],
      stringsAsFactors = FALSE
    )
    species_map <- SP |>
      dplyr::filter(!is.na(speciesNb)) |>
      dplyr::distinct(speciesNb, species) |>
      dplyr::mutate(param = paste0("p2[", speciesNb, "]"))
    p2_df <- p2_df |>
      dplyr::left_join(species_map, by = "param") |>
      dplyr::rename(name = species) |>
      dplyr::mutate(level = "species") |>
      dplyr::select(name, level, mean, sd)
  } else {
    p2_df <- dplyr::tibble(name = character(), level = character(), mean = numeric(), sd = numeric())
  }

  res <- dplyr::bind_rows(genus_row, p1_df, p2_df) |>
    dplyr::select(name, level, mean, sd)

  species_to_complex <- SP |>
    dplyr::filter(!is.na(species), !is.na(complex)) |>
    dplyr::distinct(species, complex)

  res <- res |>
    dplyr::left_join(species_to_complex, by = c("name" = "species")) |>
    dplyr::mutate(complex = dplyr::if_else(level == "species", complex, NA_character_))

  res_clean <- res |>
    dplyr::filter(!grepl("complex", name, ignore.case = TRUE))

  final <- res_clean |>
    dplyr::select(name, level, mean, sd) |>
    dplyr::mutate(
      Type = factor(level, levels = c("species", "complex", "genus"))
    ) |>
    dplyr::arrange(Type, name) |>
    dplyr::select(Name = name, Type = level, Mean = mean, SD = sd) |>
    dplyr::filter(!is.na(Name))

  return(final)
}
#tab2 <- stan_species_complex_table(stan_fit)

merge_empirical_bayesian <- function(empirical_df, bayesian_df) {
  merged <- empirical_df |>
    dplyr::left_join(
      bayesian_df |> dplyr::select(Name, Bayesian_Mean = Mean, Bayesian_SD = SD),
      by = "Name"
    ) |>
    dplyr::rename(
      Empirical_Mean = Mean,
      Empirical_SD   = SD
    ) |>
    dplyr::mutate(
      Diff_Mean = abs(Empirical_Mean - Bayesian_Mean),
      Diff_SD   = Empirical_SD - Bayesian_SD
    ) |>
    dplyr::select(Name, Type,
                  Empirical_Mean, Bayesian_Mean, Diff_Mean,
                  Empirical_SD, Bayesian_SD, Diff_SD)

  return(merged)
}
#tab3 <- merge_empirical_bayesian(tab,tab2)

compare_empirical_bayesian_all <- function(params, path) {
  results_list <- list()

  for (param in params) {
    message("Processing parameter: ", param)

    data <- creation_df(param)
    SP <- data$species_complex |>
      dplyr::mutate(
        species = gsub("^unlabeled\\s+", "", species),
        complex = gsub("^unlabeled\\s+", "", complex)
      )

    empirical_df <- obs_complex_species_table(data)

    stan_fit <- readRDS(file.path(path, paste0(param,"_stanoutput.rds")))

    bayesian_df <- stan_species_complex_table(results = stan_fit, SP=data$species_complex)

    merged_df <- merge_empirical_bayesian(empirical_df, bayesian_df)

    merged_df <- merged_df |>
      dplyr::mutate(Parameter = param) |>
      dplyr::select(Parameter, dplyr::everything())

    merged_df <- merged_df |>
      dplyr::left_join(
        data$species_complex|> dplyr::select(species, complex),
        by = c("Name" = "species")
      )
    merged_df$Type <- "Species"
    results_list[[param]] <- merged_df
  }

  final_df <- dplyr::bind_rows(results_list)
  return(final_df)
}


plot_complex_comparison <- function(final_results) {
  complexes_multi <- final_results %>%
    group_by(complex, Parameter) %>%
    summarise(n_species = n_distinct(Name), .groups = "drop") %>%
    filter(n_species >= 2) %>%
    pull(complex) %>%
    unique()

  plot_data <- final_results %>%
    filter(Type == "Species", complex %in% complexes_multi) %>%
    select(Parameter, Name, complex, Empirical_Mean, Bayesian_Mean) %>%
    mutate(
      Parameter = recode(Parameter,
                         endophagy = "Endophagy",
                         endophily = "Endophily",
                         indoor_HBI = "Indoor HBI",
                         outdoor_HBI = "Outdoor HBI",
                         parous_rate = "Parous rate",
                         sac_rate = "Sac rate",
                         resting_duration = "Resting Duration"
      ),
      y_min = ifelse(Parameter == "Resting Duration", 0, 0),
      y_max = ifelse(Parameter == "Resting Duration", 5, 1)
    )

  plots <- plot_data %>%
    group_split(complex) %>%
    lapply(function(df) {
      df_blank <- df %>%
        group_by(Parameter) %>%
        summarise(y_min = first(y_min), y_max = first(y_max), .groups = "drop") %>%
        tidyr::pivot_longer(cols = c(y_min, y_max), names_to = "dummy", values_to = "y") %>%
        mutate(x = 1)

      ggplot(df) +
        geom_segment(aes(x = 1, xend = 2,
                         y = Empirical_Mean, yend = Bayesian_Mean,
                         color = Name),
                     size = 0.7, alpha = 0.7) +
        geom_point(aes(x = 1, y = Empirical_Mean, color = Name), size = 2) +
        geom_point(aes(x = 2, y = Bayesian_Mean, color = Name), size = 2) +
        geom_blank(data = df_blank, aes(x = x, y = y)) +  # force l'échelle par panel
        facet_wrap(~Parameter, ncol = 4, nrow = 2, scales = "free_y") + # layout : 4 + 3
        scale_x_continuous(
          breaks = c(1, 2),
          labels = c("Empirical", "Bayesian"),
          expand = c(0.2, 0.2)
        ) +
        labs(
          title = paste0("Complex: ", unique(df$complex)),
          x = NULL, y = "Mean value", col=""
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 11, face = "bold"),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "bottom"
        )
    })

  names(plots) <- unique(plot_data$complex)
  return(plots)
}

params <- c("parous_rate","indoor_HBI" ,"outdoor_HBI","sac_rate","endophagy", "endophily", "resting_duration")
final_results <- compare_empirical_bayesian_all(params, path=path_output)


plots <- plot_complex_comparison(final_results)
ggsave(plots[[2]], file=file.path(path_output, "shrinkage_gambiae.png"), width = 9, height=5)
ggsave(plots[[1]], file=file.path(path_output, "shrinkage_funestus.png"), width = 9, height=5)

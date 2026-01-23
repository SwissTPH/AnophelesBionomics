library(AnophelesBionomics)
library(dplyr)
path_output  = "C:/Users/chamcl/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Bionomics_estimation_v5/Figures/"


species_simulations_all <- function(fit, SP, varname) {

  posterior_samples <- do.call(rbind, fit)
  param_names <- colnames(posterior_samples)


  p2_idx <- grep("^p2\\[", param_names)

  species_draws <- as.data.frame(posterior_samples[, p2_idx, drop = FALSE])
  species_draws$draw <- seq_len(nrow(species_draws))

  species_draws <- tidyr::pivot_longer(
    species_draws,
    cols = -draw,
    names_to = "param",
    values_to = "value"
  )

  species_map <- SP |>
    dplyr::filter(!is.na(speciesNb)) |>
    dplyr::distinct(speciesNb, species) |>
    dplyr::mutate(param = paste0("p2[", speciesNb, "]"))

  species_draws <- species_draws |>
    dplyr::left_join(species_map, by = "param") |>
    dplyr::rename(name = species) |>
    dplyr::mutate(level = "species", data = TRUE)


  p1_idx <- grep("^p1\\[", param_names)

  complex_draws <- as.data.frame(posterior_samples[, p1_idx, drop = FALSE])
  complex_draws$draw <- seq_len(nrow(complex_draws))

  complex_draws <- tidyr::pivot_longer(
    complex_draws,
    cols = -draw,
    names_to = "param",
    values_to = "value"
  )

  complex_map <- SP |>
    dplyr::filter(!is.na(complexNb)) |>
    dplyr::distinct(complexNb, complex) |>
    dplyr::mutate(param = paste0("p1[", complexNb, "]"))

  complex_draws <- complex_draws |>
    dplyr::left_join(complex_map, by = "param") |>
    dplyr::rename(name = complex) |>
    dplyr::mutate(level = "complex", data = TRUE)

  genus_param <- "p1[1]"

  genus_draws <- as.data.frame(posterior_samples[, genus_param, drop = FALSE])
  genus_draws$draw <- seq_len(nrow(genus_draws))

  genus_draws <- tidyr::pivot_longer(
    genus_draws,
    cols = -draw,
    names_to = "param",
    values_to = "value"
  ) |>
    dplyr::mutate(
      name  = "Anopheles",
      level = "genus",
      data  = TRUE
    )


  draws_long <- dplyr::bind_rows(
    species_draws,
    complex_draws,
    genus_draws
  )


  draws_long <- draws_long |>
    dplyr::filter(!is.na(name)) |>
    dplyr::filter(!grepl("unlabeled", name, ignore.case = TRUE)) |>
    dplyr::mutate(name = gsub("_complex", "", name))


  compat_repo <- read_data_file(
    "compatibility_repo_taxonomy_v2.csv",
    sep = ";"
  )

  compat_repo_hyrcanus_complex=compat_repo %>% filter(survey=="Anopheles gambiae complex")%>%
    mutate(survey="Anopheles hyrcanus complex", complex="Hyrcanus")

  compat_repo_jamesii_complex=compat_repo %>% filter(survey=="Anopheles gambiae complex")%>%
    mutate(survey="Anopheles jamesii complex", complex="Jamesii")


  compat_repo=rbind(compat_repo, compat_repo_hyrcanus_complex,compat_repo_jamesii_complex)


  existing_names <- function(x) unique(x$name)

  for (i in seq_len(nrow(compat_repo))) {

    sp <- compat_repo$species[i]
    cx <- compat_repo$complex[i]

    if (sp != "" && !sp %in% existing_names(draws_long)) {

      complex_row <- draws_long |>
        dplyr::filter(level == "complex", name == cx)

      if (nrow(complex_row) > 0) {
        draws_long <- dplyr::bind_rows(
          draws_long,
          complex_row |>
            dplyr::mutate(
              name  = sp,
              level = "species",
              data  = FALSE
            )
        )
        next
      }

      genus_row <- draws_long |>
        dplyr::filter(level == "genus")

      if (nrow(genus_row) > 0) {
        draws_long <- dplyr::bind_rows(
          draws_long,
          genus_row |>
            dplyr::mutate(
              name  = sp,
              level = "species",
              data  = FALSE
            )
        )
      }
    }

    if (sp == "" && !cx %in% existing_names(draws_long)) {

      genus_row <- draws_long |>
        dplyr::filter(level == "genus")

      if (nrow(genus_row) > 0) {
        draws_long <- dplyr::bind_rows(
          draws_long,
          genus_row |>
            dplyr::mutate(
              name  = cx,
              level = "complex",
              data  = FALSE
            )
        )
      }
    }
  }

  draws_long <- draws_long |>
    dplyr::filter(!is.na(name))

  return(draws_long)
}












run_all_vars_get_one_df <- function(varnames, path) {

  all_draws <- list()

  for (varname in varnames) {

    data <- creation_df(varname)
    results <- readRDS(file.path(path, paste0(varname,"_stanoutput.rds")))

    stan_fit <- extract_HPD_mcmc(results)

    draws_long <- species_simulations_all(fit = stan_fit, SP = data$species_complex,varname =varname)

    draws_long$param <- varname
    all_draws[[varname]] <- draws_long
  }

  combined <- dplyr::bind_rows(all_draws)

  final_long <- combined |>
    dplyr::select(name, param, value) |>
    dplyr::rename(species = name)

  return(final_long)
}

varnames <- c("endophagy", "endophily", "indoor_HBI", "outdoor_HBI", "parous_rate", "sac_rate","resting_duration")

df_density_allparam <- run_all_vars_get_one_df(varnames = varnames, path = path_output) %>% dplyr::rename("name"=species)

saveRDS(df_density_allparam, file = file.path("inst/outputs/full_bionomics_posterior.rds"))

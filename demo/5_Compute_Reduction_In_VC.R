#################
#### library ####
#################

library(AnophelesBionomics)
library(AnophelesModel)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(sensitivity)


#################
#### LOADING ####
#################

path_plot  = path_output


setwd(".")

densite_all_param=readRDS(system.file("outputs", "full_bionomics_posterior.rds", package = "AnophelesBionomics"))

load(file.path("demo", "df_wide_IG2.RData"))

activity_patterns_withRefs_small <- readRDS(file.path("demo", "activity_patterns_withRefs_small.rds"))

compatibility_repo_taxonomy=read_data_file( "compatibility_repo_taxonomy_v2.csv", sep = ";" )

source(file.path("demo", "Reduction_In_VC_functions.R"))
###############################
# import additional parameters for model

# activity rhythms
activity_noRhythms <- get_activity_noRhythms()
activity_pattern_all_species <- get_activity_pattern_all_species()

all_param_l_kappa <- data.frame("L"=2.2, "kappa"=1.3)

vc_simul_IG2 <- run_vcc_simulation(n=200, densite_all_param=densite_all_param, df_wide_IG2=df_wide_IG2, all_param_l_kappa=all_param_l_kappa, activity_pat_all_species=activity_pattern_all_species)
vc_simul_IG2$results$name=ifelse(vc_simul_IG2$results$name=="Anopheles","GENUS", vc_simul_IG2$results$name)

saveRDS(vc_simul_IG2, file.path(path_output, "vc_simul_IG2.rds"))
#load(file.path("demo", "vc_simul_IG2_v2.RData"))

plot_vcc_results <- function(sim_results,
                             species_filter = NULL,
                             conf_level = 0.95,
                             palette_path = "demo/new_palette_density_plots.csv",
                             color = NULL,
                             sim_name = NULL) {


  if (is.null(sim_name)) {
    obj_name <- deparse(substitute(sim_results))
    print(obj_name)
    if (grepl("IG1", obj_name, ignore.case = TRUE)) {
      sim_name <- "ITN"
    } else if (grepl("IG2", obj_name, ignore.case = TRUE)) {
      sim_name <- "Interceptor G2"
    } else {
      sim_name <- "Unknown Treatment"
    }
  }

  title_text <- paste0("Mean Reduction in VC by Species for ", sim_name, " (95% CI)")

  sim_results$name[sim_results$name == "Anopheles gambiae s.s."] <- "Anopheles gambiae s.s. / coluzzii"
  palette_df <- read_csv(palette_path, show_col_types = FALSE)
  palette_df$name[palette_df$name == "Anopheles gambiae s.s."] <- "Anopheles gambiae s.s. / coluzzii"
  palette_named <- setNames(palette_df$pal, palette_df$name)

  #sim_results$name2=ifelse(sim_results$name=="GENUS", sim_results$name,
  #                        paste0("*",sim_results$name, "*"))

  df_long <- sim_results %>%
    tidyr::pivot_longer(
      cols = starts_with("sim_"),
      names_to = "sim",
      values_to = "vcc"
    )

  if (!is.null(species_filter)) {
    df_long <- df_long %>% filter(name %in% species_filter)
  }

  df_summary <- df_long %>%
    group_by(name) %>%
    summarise(
      mean_vcc = mean(vcc, na.rm = TRUE),
      sd_vcc = sd(vcc, na.rm = TRUE),
      n = sum(!is.na(vcc)),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_vcc / sqrt(n),
      ci_lower = mean_vcc - qt(1 - (1 - conf_level)/2, n - 1) * se,
      ci_upper = mean_vcc + qt(1 - (1 - conf_level)/2, n - 1) * se,
      ci_margin = qt(1 - (1 - conf_level)/2, n - 1) * se,
    ) %>%
    arrange(desc(mean_vcc))

  # Handle colors
  df_summary <- df_summary %>%
    mutate(
      color_name = case_when(
        grepl("unlabeled", name, ignore.case = TRUE) ~ "unlabeled",
        TRUE ~ name
      )
    )

  unlabeled_species <- unique(df_summary$color_name[df_summary$color_name == "unlabeled"])
  if (length(unlabeled_species) > 0) {
    unlabeled_colors <- rep("#000000", length(unlabeled_species))
    names(unlabeled_colors) <- unlabeled_species
    palette_named <- c(palette_named, unlabeled_colors)
  }

  df_summary <- df_summary %>%
    mutate(color = palette_named[color_name])

  df_summary$color[is.na(df_summary$color)] <- "#808080"

  # ---- Plot ----
  if (is.null(color)) {
    p <- ggplot(df_summary, aes(x = reorder(name, mean_vcc), y = mean_vcc, fill = name)) +
      geom_col(color = "black") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3) +
      coord_flip() +
      scale_fill_manual(values = setNames(df_summary$color, df_summary$name)) +
      labs(
        title = title_text,
        x = "Mosquito Species",
        y = "Reduction in VC"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10, face = "bold.italic"),
        axis.title.x =  element_text(size = 11)
      )
  } else {
    p <- ggplot(df_summary, aes(x = reorder(name, mean_vcc), y = mean_vcc)) +
      geom_col(fill = color, color = "black") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3) +
      coord_flip() +
      labs(
        title = title_text,
        x = "Mosquito Species",
        y = "Reduction in VC"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = ggtext::element_markdown(size = 10, face = "bold.italic"),
        axis.title.x = element_text(size = 11)
      )
  }

  print(df_summary)
  print(p)

  return(list(p = p, df_summary = df_summary))
}


mosquitoes <- c("Anopheles gambiae s.s. / coluzzii",
                "Anopheles arabiensis",
                "Anopheles funestus",
                "Anopheles merus",
                "Anopheles moucheti",
                "Anopheles nili",
                "Anopheles melas",
                "Anopheles ovengensis",
                "Anopheles stephensi",
                "Anopheles albimanus",
                "Anopheles darlingi",
                "Anopheles minimus",
                "GENUS"
)

gg <- plot_vcc_results(vc_simul_IG2$results, species_filter = mosquitoes)
ggsave(
 filename = file.path(path_plot,"reduc_vc_IG2_partial.png"),
 plot = gg$p,
 width = 811 / 96,
 height = 373 / 96,
 dpi = 96,
 units = "in"
)
gg <- plot_vcc_results(vc_simul_IG2$results, color = "#2E86C1")
ggsave(
 filename = file.path(path_plot,"reduc_vc_IG2.png"),
 plot = gg$p,
 width = 753 / 96,
 height = 1143 / 96,
 dpi = 96,
 units = "in"
)


##############
#### PRCC ####
##############


prcc_vcc_genus <- function(n, df_wide_IG2, densite_all_param, genus_name = "GENUS") {
  sp_data <- subset(densite_all_param, name == genus_name)
  if (nrow(sp_data) == 0) stop(paste("Aucune donnée trouvée pour", genus_name))

  species_name <- genus_name
  vcc_values <- numeric(n)
  error_count <- 0

  params_df <- data.frame(
    indoor_HBI = numeric(n),
    outdoor_HBI = numeric(n),
    endophagy = numeric(n),
    sac_rate = numeric(n),
    parous_rate = numeric(n),
    resting_duration = numeric(n)
  )

  for (i in 1:n) {
    indoor_HBI   <- sample(sp_data$value[sp_data$param == "indoor_HBI"], 1)
    outdoor_HBI  <- sample(sp_data$value[sp_data$param == "outdoor_HBI"], 1)
    endophagy    <- sample(sp_data$value[sp_data$param == "endophagy"], 1)
    sac_rate     <- sample(sp_data$value[sp_data$param == "sac_rate"], 1)
    parous_rate  <- sample(sp_data$value[sp_data$param == "parous_rate"], 1)
    resting_duration  <- sample(sp_data$value[sp_data$param == "resting_duration"], 1)


    params_df[i, ] <- c(indoor_HBI, outdoor_HBI, endophagy, sac_rate, parous_rate,resting_duration)
    HBI <- indoor_HBI * endophagy + outdoor_HBI * (1 - endophagy)

    df_wide <- df_wide_IG2

    ranking <- tryCatch({
      run_interv_ranking(
        HBI = HBI,
        parous_rate = parous_rate,
        sac_rate = sac_rate,
        resting_duration = resting_duration,
        df_wide = df_wide,
        activity = activity_noRhythms,
        decay = "Weibull",
        params_L_K = all_param_l_kappa,
        cov = 0.8
      )
    }, error = function(e) {
      message("Erreur détectée pour ", species_name, " (simulation ", i, "): ", e$message)
      return(NA)
    })
    if (is.list(ranking) && !is.null(ranking$impact_value)) {
      vcc <- ranking$impact_value
    } else {
      vcc <- NA
    }

    vcc_values[i] <- vcc
  }

  prcc_res <- tryCatch(
    pcc(params_df, vcc_values, rank = TRUE, nboot = 100),
    error = function(e) {
      message("Erreur pendant le calcul du PRCC : ", e$message)
      return(NULL)
    }
  )

  return(list(
    genus = genus_name,
    params = params_df,
    VCC = vcc_values,
    PRCC = prcc_res,
    n_errors = error_count
  ))
}

result_GENUS_IG2 <- prcc_vcc_genus(n = 20000, df_wide_IG2 = df_wide_IG2, densite_all_param = densite_all_param, genus_name = "Anopheles")
saveRDS(result_GENUS_IG2, file.path(path_output, "result_GENUS_IG2.rds"))


#load(file.path("demo", "result_GENUS_IG2.RData"))

plot_prcc_GENUS <- function(result_GENUS) {
  print(result_GENUS$PRCC$PRCC)

  prcc_data <- as.data.frame(result_GENUS$PRCC$PRCC)
  prcc_data$Parameters <- rownames(prcc_data)

  prcc_filtered <- prcc_data %>%
    rename(original = "original", min_ci = "min. c.i.", max_ci = "max. c.i.") %>%
    mutate(
      Parameters = recode(Parameters,
                          endophagy = "Endophagy",
                          indoor_HBI = "Indoor HBI",
                          outdoor_HBI = "Outdoor HBI",
                          parous_rate = "Parous rate",
                          sac_rate = "Sac rate",
                          resting_duration = "Resting Duration"),
      Parameters = factor(Parameters,
                          levels = c("Endophagy", "Indoor HBI", "Outdoor HBI", "Sac rate", "Parous rate", "Resting Duration"))
    )

  obj_name <- deparse(substitute(result_GENUS))
  if (grepl("IG1", obj_name, ignore.case = TRUE)) {
    sim_name <- "ITN"
  } else if (grepl("IG2", obj_name, ignore.case = TRUE)) {
    sim_name <- "Interceptor G2"
  } else {
    sim_name <- "Unknown Treatment"
  }

  title_text <- paste0("Partial Rank Correlation Coefficients (PRCC) — ", sim_name)


  colors <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7","#56B4E9")

  p <- ggplot(prcc_filtered, aes(x = Parameters, y = original, fill = Parameters)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = min_ci, ymax = max_ci), width = 0.2) +
    scale_fill_manual(values = colors) +
    ylim(-1, 1) +
    theme_minimal() +
    labs(
      title = title_text,
      y = "PRCC coefficient",
      x = "Parameters"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold")
    )

  return(p)
}

ff <- plot_prcc_GENUS(result_GENUS_IG2)
print(ff)
ggsave(
 filename = file.path(path_plot,"PRCC_IG2.png"),
 plot = ff,
 width = 563 / 96,
 height = 299 / 96,
 dpi = 96,
 units = "in"
)

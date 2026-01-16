################################
# ACTIVITY PATTERNS PER SPECIES
get_activity_noRhythms <- function(){
  activity_noRhythms = def_activity_patterns()
  activity_noRhythms$humans_in_bed=rep(1, length(activity_noRhythms$HBI))
  activity_noRhythms$humans_indoors=rep(1, length(activity_noRhythms$HBI))
  return(activity_noRhythms)
}



get_activity_pattern_all_species <- function(){

  activity_pat_new <- activity_patterns_withRefs_small %>% left_join(unique(activity_patterns %>% select(id, species)), by = "id")


  heures_ordonnees <- c(
    sprintf("%02d.00_%02d.00", 16:23, 17:24),
    sprintf("%02d.00_%02d.00", 0:6, 1:7),
    "07.00_08.00"
  )
  heures_ordonnees <- gsub("24.00", "00.00", heures_ordonnees)

  activity_pat_new <- activity_pat_new %>%
    mutate(hour = factor(hour, levels = heures_ordonnees, ordered = TRUE)) %>%
    complete(id, hour = heures_ordonnees, fill = list(value = 0))




  new_df <- compatibility_repo_taxonomy[, c("species", "complex")] %>%
    mutate(
      level = ifelse(is.na(species) | species == "", "complex", "species"),
      level2 = ifelse(is.na(species) | species == "", complex, species)
    )


  split_complex_mean <- function(my_activity_patterns, new_df) {
    my_activity_patterns <- my_activity_patterns %>%
      mutate(
        species = as.character(species),
        sampling = as.character(sampling),
        hour = as.character(hour)
      )
    new_df <- new_df %>%
      mutate(
        level = as.character(level),
        level2 = as.character(level2),
        complex = as.character(complex)
      )

    level2_list <- unique(new_df$level2)

    mean_by_hour <- function(df, species_list, sampling_type) {
      df %>%
        filter(sampling == sampling_type, species %in% species_list) %>%
        group_by(hour) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pull(value)
    }

    global_mean_by_hour <- function(df, sampling_type) {
      df %>%
        filter(sampling == sampling_type) %>%
        group_by(hour) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pull(value)
    }

    process_entity <- function(entity_name) {

      get_mean_values <- function(sampling_type) {

        df_species <- my_activity_patterns %>% filter(sampling == sampling_type)
        species_available <- unique(df_species$species)

        if (entity_name %in% species_available) {
          return(mean_by_hour(my_activity_patterns, entity_name, sampling_type))
        }

        row_entity <- new_df %>% filter(level2 == entity_name)
        if (nrow(row_entity) > 0 && !is.na(row_entity$complex[1])) {
          complex_name <- row_entity$complex[1]

          species_in_complex <- new_df %>%
            filter(complex == complex_name, level == "species") %>%
            pull(level2)

          species_with_data <- intersect(species_in_complex, species_available)
          if (length(species_with_data) > 0) {
            return(mean_by_hour(my_activity_patterns, species_with_data, sampling_type))
          }
        }

        if (entity_name %in% new_df$level2[new_df$level == "complex"]) {
          species_in_complex <- new_df %>%
            filter(complex == entity_name, level == "species") %>%
            pull(level2)

          species_with_data <- intersect(species_in_complex, species_available)
          if (length(species_with_data) > 0) {
            return(mean_by_hour(my_activity_patterns, species_with_data, sampling_type))
          }
        }

        return(global_mean_by_hour(my_activity_patterns, sampling_type))
      }

      HBI <- get_mean_values("HBI")
      HBO <- get_mean_values("HBO")

      IND <- global_mean_by_hour(my_activity_patterns, "IND")
      BED <- global_mean_by_hour(my_activity_patterns, "BED")

      list(HBI = HBI, HBO = HBO, humans_indoors = IND, humans_in_bed = BED)
    }

    output_list <- lapply(level2_list, process_entity)
    names(output_list) <- level2_list

    return(output_list)
  }

  activity_pat_new <- activity_pat_new %>%
    mutate(species = case_when(
      species %in% c("Anopheles gambiae ss", "Anopheles coluzzi", "Anopheles gambiae sl") ~ "Anopheles gambiae s.s.",
      species == "Anopheles gambiae" ~ "Gambiae",
      TRUE ~ species
    ))
  dd <- split_complex_mean(my_activity_patterns = activity_pat_new, new_df = new_df)

  return(dd)

}


get_in_out_exp_modif = function(activity_cycles, endophagy) {
  activity_cycles$HBI = activity_cycles$HBI/
    sum(activity_cycles$HBI, na.rm = TRUE) * endophagy
  activity_cycles$HBO = activity_cycles$HBO/
    sum(activity_cycles$HBO, na.rm = TRUE) * (1 - endophagy)
  activity_cycles$humans_indoors = with(activity_cycles,
                                        ifelse(is.na(humans_indoors),
                                               humans_in_bed, humans_indoors))
  activity_cycles$Exposure_Indoor_total = with(activity_cycles,
                                               HBI*humans_indoors)
  activity_cycles$Exposure_Outdoor_total = with(activity_cycles,
                                                HBO*(1-humans_indoors))

  activity_cycles$Exposure_Indoor_whileinbed = with(activity_cycles,
                                                    HBI*pmin(humans_in_bed,
                                                             humans_indoors, na.rm = TRUE))
  activity_cycles$Exposure_Outdoor_whileinbed = with(activity_cycles,
                                                     HBO*pmax(humans_in_bed-humans_indoors,
                                                              rep(0, length(humans_indoors)), na.rm = TRUE))
  Overall_total = sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE) +
    sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE)

  Exposure_Indoor_total =
    sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE)/Overall_total
  Exposure_Outdoor_total =
    sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE)/Overall_total
  Exposure_Indoor_whileinbed =
    sum(activity_cycles$Exposure_Indoor_whileinbed,
        na.rm = TRUE)/Overall_total
  Exposure_Outdoor_whileinbed =
    sum(activity_cycles$Exposure_Outdoor_whileinbed,
        na.rm = TRUE)/Overall_total

  indoor_outdoor = list(Exposure_Indoor_total, Exposure_Outdoor_total,
                        Exposure_Indoor_whileinbed,
                        Exposure_Outdoor_whileinbed)
  names(indoor_outdoor) = c('Exposure_Indoor_total', 'Exposure_Outdoor_total',
                            'Exposure_Indoor_whileinbed',
                            'Exposure_Outdoor_whileinbed')
  return(indoor_outdoor)
}

split_complex_mean <- function(my_activity_patterns) {
  heures_ordonnees <- c(
    sprintf("%02d.00_%02d.00", 16:23, 17:24),
    sprintf("%02d.00_%02d.00", 0:6, 1:7),
    "07.00_08.00"
  )
  heures_ordonnees <- gsub("24.00", "00.00", heures_ordonnees)

  process_complex <- function(complex_type) {
    # Vérifie si le complex est présent dans la colonne "complex"
    if (complex_type %in% unique(my_activity_patterns$complex)) {
      df <- my_activity_patterns %>% filter(complex == complex_type)
    } else {
      # Sinon, on cherche dans la colonne "species"
      df <- my_activity_patterns %>% filter(species == complex_type)
    }

    # HBI
    HBI <- df %>%
      filter(sampling == "HBI") %>%
      group_by(hour) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(value)

    # HBO
    HBO <- df %>%
      filter(sampling == "HBO") %>%
      group_by(hour) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(value)

    # humans indoors
    IND <- my_activity_patterns %>%
      filter(sampling == "IND") %>%
      group_by(hour) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(value)

    # humans in bed
    BED <- my_activity_patterns %>%
      filter(sampling == "BED") %>%
      group_by(hour) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(value)

    list(HBI = HBI, HBO = HBO, humans_indoors = IND, humans_in_bed = BED)
  }

  list(
    gambiae = process_complex("gambiae"),
    funestus = process_complex("funestus"),
    gambiae_ss = process_complex("gambiae_ss"),
    arabiensis = process_complex("arabiensis")

  )
}





#########################
#### Reduction in VC ####
#########################


decay_function <- function(L, kappa=NULL, datej, decay){

  if (decay %in% c("weibull" , "Weibull")){
    return( exp( -(datej/(L))^kappa * log(2) ))
  }

  if (decay %in% c("linear","Linear")){
    return( 1-datej/L)
  }

  if (decay %in% c("hill","Hill")){

    return (1/(1+(datej/L)^kappa))
  }

  if (decay %in% c("smooth-compact" , "smoothcompact")){

    return (exp(kappa - kappa/(1-(datej/L)^2)))
  }
  if (decay %in% c("none" , "no decay")){

    return (1)
  }

}

new_get_interv <- function(myproba, L, kappa, decay = NULL, npoints, model_p = NULL, name = NULL, intervention_type = "LLINs", decay_bounds = NULL) {

  if (!is.null(model_p)) {
    list_interv <- list(intervention_obj_examples$IRS_example)
    interventions_vec <- suppressMessages(
      def_interventions_effects(list_interv, model_p, num_ip_points = max(npoints, 3), verbose = FALSE)
    )
  } else {
    stop("model_p")
  }

  interv <- interventions_vec[[1]]$effects

  if (!is.null(decay)) {
    decay_factor <- decay_function(L = L, kappa = kappa, datej = 0:(npoints-1), decay = decay)
    interv$survival <- decay_factor
  }

  linear_decay_between <- function(start_val, end_val, npoints) {
    seq(start_val, end_val, length.out = npoints)
  }

  if ("Deterrency" %in% myproba) {
    det_decay <- linear_decay_between(
      decay_bounds$Deterrency["borne_sup"],
      decay_bounds$Deterrency["borne_inf"],
      npoints)
    interv$alphai[, 1] <- interv$alphai[, 2] * (1 - det_decay)}


  if ("PrePrandial" %in% myproba) {
    pre_decay <- linear_decay_between(
      decay_bounds$PrePrandial["borne_sup"],
      decay_bounds$PrePrandial["borne_inf"],
      npoints
    )
    interv$PBi[, 1] <- interv$PBi[, 2] * (1 - pre_decay)}

  if ("PostPrandial" %in% myproba) {
    post_decay <- linear_decay_between(
      decay_bounds$PostPrandial["borne_sup"],
      decay_bounds$PostPrandial["borne_inf"],
      npoints
    )
    interv$PCi[, 1] <- interv$PCi[, 2] * (1 - post_decay)}

  interv$alphai <- interv$alphai[1:max(1, npoints), ]
  interv$PBi <- interv$PBi[1:max(1, npoints), ]
  interv$PCi <- interv$PCi[1:max(1, npoints), ]
  interv$PDi <- interv$PDi[1:max(1, npoints), ]
  interv$PEi <- interv$PEi[1:max(1, npoints), ]

  result <- interventions_vec
  result[[1]]$effects <- interv
  if (!is.null(name)) {
    result[[1]]$parameterisation <- name
    result[[1]]$description <- name
  }
  return(result)
}

new_calculate_impact_interv <- function(cov, model_noRhythms,
                                        bounds_deterrency = c(borne_sup = 1, borne_inf = 1),
                                        bounds_preprandial = c(borne_sup = 1, borne_inf = 1),
                                        bounds_postprandial = c(borne_sup = 1, borne_inf = 1),
                                        decay = NULL,
                                        kappa = 2,
                                        L = 3) {

  myproba <- c("Deterrency", "PrePrandial", "PostPrandial")

  my_bounds <- list(
    Deterrency    = bounds_deterrency,
    PrePrandial   = bounds_preprandial,
    PostPrandial  = bounds_postprandial
  )

  intervention_vec_ <- new_get_interv(
    myproba = myproba,
    L=L*365,
    kappa=kappa,
    decay=decay,
    npoints = 365 * 3,
    model_p = model_noRhythms,
    name = NULL,
    intervention_type = "LLINs",
    decay_bounds = my_bounds
  )

  imp_tot <- calculate_impact(
    intervention_vec_,
    model_p = model_noRhythms,
    coverage_vec = c(0, cov),
    num_ip_points = 365 * 3,
    Nv0 = 2000
  )


  return(list(
    impact = imp_tot$interventions_vec[[1]]$effects$avg_impact[length(imp_tot$interventions_vec[[1]]$effects$avg_impact)],
    avg_vc_1 = imp_tot$interventions_vec[[1]]$effects$avg_vc[1],
    avg_vc_2 = imp_tot$interventions_vec[[1]]$effects$avg_vc[2]))
}

run_interv_ranking <- function(HBI, parous_rate, sac_rate, resting_duration, cov = 0.8,
                               df_wide = df_wide, decay = NULL, activity = NULL,
                               params_L_K) {

  ent_params <- data.frame(
    species_name = "Anopheles gambiae",
    M = parous_rate,
    M.sd = 0,
    Chi = HBI,
    A0 = sac_rate,
    A0.sd = 0,
    zeta.3 = 1,
    td = 0.33,
    tau = resting_duration,
    ts = 10,
    to = 5,
    endophily = 1,
    endophily.sd = 0,
    endophagy = 1,
    endophagy.sd = 0
  )

  model_noRhythms <- build_model_obj(
    vec_p = ent_params,
    hosts_p = def_host_params(),
    activity = activity,
    total_pop = 2000
  )

  impacts   <- numeric(nrow(df_wide))
  avg_vc_1s <- numeric(nrow(df_wide))
  avg_vc_2s <- numeric(nrow(df_wide))

  for (i in seq_len(nrow(df_wide))) {
    row <- df_wide[i, ]
    # Récupérer L et kappa correspondant à ce type de moustiquaire
    L_i     <- params_L_K$L
    kappa_i <- params_L_K$k
    det_sup  <- row$Repellent_Unwashed
    det_inf  <- row$Repellent_Washed
    pre_sup  <- row$Preprandialkilling_Unwashed
    pre_inf  <- row$Preprandialkilling_Washed
    post_sup <- row$Postprandialkilling_Unwashed
    post_inf <- row$Postprandialkilling_Washed

    res <- new_calculate_impact_interv(
      cov = cov,
      model_noRhythms = model_noRhythms,
      bounds_deterrency   = c(borne_sup = det_sup,  borne_inf = det_inf),
      bounds_preprandial  = c(borne_sup = pre_sup,  borne_inf = pre_inf),
      bounds_postprandial = c(borne_sup = post_sup, borne_inf = post_inf),
      decay = decay,
      L = L_i,
      kappa = kappa_i
    )

    impacts[i]   <- res$impact
    avg_vc_1s[i] <- res$avg_vc_1
    avg_vc_2s[i] <- res$avg_vc_2
  }

  results <- data.frame(
    netType_EHT = df_wide$netType_EHT,
    impact_value = impacts,
    avg_vc_1 = avg_vc_1s,
    avg_vc_2 = avg_vc_2s
  )

  return(results)
}




run_vcc_simulation <- function(n, densite_all_param, df_wide_IG2, all_param_l_kappa, activity_pat_all_species) {
  species_list <- unique(densite_all_param$name)
  results <- data.frame(name = species_list)
  error_log <- data.frame(species = character(), n_errors = integer(), stringsAsFactors = FALSE)

  mean_global <- list(
    HBI = Reduce("+", lapply(activity_pat_all_species, function(x) x$HBI)) / length(activity_pat_all_species),
    HBO = Reduce("+", lapply(activity_pat_all_species, function(x) x$HBO)) / length(activity_pat_all_species),
    humans_indoors = Reduce("+", lapply(activity_pat_all_species, function(x) x$humans_indoors)) / length(activity_pat_all_species),
    humans_in_bed  = Reduce("+", lapply(activity_pat_all_species, function(x) x$humans_in_bed)) / length(activity_pat_all_species)
  )

  for (species_name in species_list) {
    sp_data <- subset(densite_all_param, name == species_name)
    vcc_values <- numeric(n)
    error_count <- 0
    print(species_name)

    mean_act_pat <- if (species_name %in% names(activity_pat_all_species)) activity_pat_all_species[[species_name]] else {
      warning(paste("Profil non trouvé pour", species_name, "→ utilisation du profil moyen global"))
      mean_global
    }

    for (i in seq_len(n)) {
      indoor_HBI   <- sample(sp_data$value[sp_data$param == "indoor_HBI"], 1)
      outdoor_HBI  <- sample(sp_data$value[sp_data$param == "outdoor_HBI"], 1)
      endophagy    <- sample(sp_data$value[sp_data$param == "endophagy"], 1)
      sac_rate     <- sample(sp_data$value[sp_data$param == "sac_rate"], 1)
      parous_rate  <- sample(sp_data$value[sp_data$param == "parous_rate"], 1)
      resting_duration  <- sample(sp_data$value[sp_data$param == "resting_duration"], 1)

      HBI <- indoor_HBI * endophagy + outdoor_HBI * (1 - endophagy)
      df_wide <- df_wide_IG2
      aa <- get_in_out_exp_modif(mean_act_pat, endophagy)
      eps <- aa$Exposure_Indoor_whileinbed

      ranking <- tryCatch({
        run_interv_ranking(
          HBI = HBI,
          parous_rate = parous_rate,
          sac_rate = sac_rate,
          df_wide = df_wide,
          resting_duration = resting_duration,
          activity = activity_noRhythms,
          decay = "Weibull",
          params_L_K = all_param_l_kappa,
          cov = 0.8 * eps
        )
      }, error = function(e) {
        message("Erreur détectée pour ", species_name, " (simulation ", i, "): ", e$message)
        error_count <<- error_count + 1
        return(NA)
      })

      vcc_values[i] <- if (is.list(ranking) && !is.null(ranking$impact_value)) ranking$impact_value else NA
    }

    results[results$name == species_name, paste0("sim_", seq_len(n))] <- vcc_values

    if (error_count > 0) {
      error_log <- rbind(error_log, data.frame(species = species_name, n_errors = error_count))
    }
  }

  return(list(results = results, error_log = error_log))
}

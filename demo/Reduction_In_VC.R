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



load(system.file("demo", "densite_all_param.RData", package = "AnophelesBionomics"))
load(system.file("demo", "df_wide_assenga_IG2.RData", package = "AnophelesBionomics"))

get_activity_noRhythms <- function(){
  activity_noRhythms = def_activity_patterns()
  activity_noRhythms$humans_in_bed=rep(1, length(activity_noRhythms$HBI))
  activity_noRhythms$humans_indoors=rep(1, length(activity_noRhythms$HBI))
  return(activity_noRhythms)
}
activity_noRhythms <- get_activity_noRhythms()
get_activity_pattern_all_species <- function(){

  activity_patterns_withRefs_small <- readRDS(system.file("demo", "activity_patterns_withRefs_small.rds", package = "AnophelesBionomics"))

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


  df <- read.csv(system.file("demo", "compatibility_repo_taxonomy_v3.csv", package = "AnophelesBionomics"),
                 sep = ";",
                 header = TRUE)

  new_df <- df[, c("species", "complex")] %>%
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
dd <- get_activity_pattern_all_species()

get_L_and_kappa <- function(){

  all_ITNcov=data.frame()
  optimise_decay_param=function(param, df_ITN_cov){
    #if(param[3]<1){
    out=data.frame(
      time=df_ITN_cov%>% pull(time),
      decay_obs=df_ITN_cov  %>% pull(ITNcov)
    )%>%
      mutate(decay=exp( -(time/(param[1]))^param[2] * log(2) ),
             diff=(decay-decay_obs)^2)%>%
      summarise(diff=sum(diff))
    # } else {
    #   out=Inf
    # }
    return(out)
  }
  calculate_param=function(df_ITN_cov){
    opti=optim(c(0.448, 1.11, 0.7), optimise_decay_param, df_ITN_cov=df_ITN_cov , method = "L-BFGS-B", lower=c(0, 0, 0), upper = c(3, Inf, 1) )$par
    names(opti)=c("L", "k", "a")
    opti$netType_EHT=unique(df_ITN_cov$netType_EHT)
    return(opti)
  }
  ITN_covtrial=data.frame(arm=rep(c("Pyrethroid-only","Olyset Plus","Interceptor G2","ProNet Duo"),each=5),
                          time=rep(c(0.5, 1, 2, 2.5, 3), 4),
                          ITNcov=c(0.937, 0.841, 0.594, 0.472, 0.371,
                                   0.821, 0.493, 0.181, 0.148, 0.095,
                                   0.909, 0.789, 0.568, 0.421, 0.367,
                                   0.909, 0.789, 0.568, 0.421, 0.367),
                          RCT="2023_Martin")
  all_ITNcov=rbind(all_ITNcov, ITN_covtrial)
  all_ITNcov$setting=paste0(all_ITNcov$RCT,"_" ,all_ITNcov$arm)
  all_ITNcov <- all_ITNcov %>%
    rename(netType_EHT = setting)
  all_param_l_kappa=data.frame()
  for(i in unique(all_ITNcov$netType_EHT)){
    print(i)
    all_param_l_kappa=rbind(all_param_l_kappa, calculate_param( df_ITN_cov =all_ITNcov %>% filter(netType_EHT==i)))
  }
  all_param_l_kappa$netType_EHT <- sub("^2023_Martin_", "", all_param_l_kappa$netType_EHT)

  return(all_param_l_kappa)
}
all_param_l_kappa <- get_L_and_kappa()



##################
#### Fonction ####
##################

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
    L_i     <- params_L_K$L[params_L_K$netType_EHT == row$netType_EHT]
    kappa_i <- params_L_K$k[params_L_K$netType_EHT == row$netType_EHT]
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

run_vcc_simulation <- function(n, densite_all_param, df_wide_assenga, all_param_l_kappa, DD) {
  species_list <- unique(densite_all_param$name)
  results <- data.frame(name = species_list)
  error_log <- data.frame(species = character(), n_errors = integer(), stringsAsFactors = FALSE)

  mean_global <- list(
    HBI = Reduce("+", lapply(DD, function(x) x$HBI)) / length(DD),
    HBO = Reduce("+", lapply(DD, function(x) x$HBO)) / length(DD),
    humans_indoors = Reduce("+", lapply(DD, function(x) x$humans_indoors)) / length(DD),
    humans_in_bed  = Reduce("+", lapply(DD, function(x) x$humans_in_bed)) / length(DD)
  )

  for (species_name in species_list) {
    sp_data <- subset(densite_all_param, name == species_name)
    vcc_values <- numeric(n)
    error_count <- 0
    print(species_name)

    mean_act_pat <- if (species_name %in% names(DD)) DD[[species_name]] else {
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
      df_wide <- df_wide_assenga
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


process_densite_data <- function(densite_all_param) {

  df <- read.csv(system.file("demo", "compatibility_repo_taxonomy_v3.csv", package = "AnophelesBionomics"),
                 sep = ";",
                 header = TRUE)


  new_df <- df[, c("species", "complex")] %>%
    mutate(
      level = ifelse(is.na(species) | species == "", "complex", "species"),
      level2 = ifelse(is.na(species) | species == "", complex, species)
    )


  level2_list <- unique(new_df$level2)
  params <- unique(densite_all_param$param)

  for (p in params) {
    df_param <- densite_all_param %>% filter(param == p)

    for (lvl in level2_list) {
      lvl_type <- new_df$level[new_df$level2 == lvl][1]

      if (lvl_type == "species") {
        sp <- lvl
        if (!(sp %in% df_param$name)) {
          comp <- new_df$complex[new_df$species == sp][1]
          if (comp %in% df_param$name) {
            vals <- df_param$value[df_param$name == comp]
          } else {
            vals <- df_param$value[df_param$name == "GENUS"]
          }
          new_rows <- data.frame(name = sp, param = p, value = vals)
          densite_all_param <- bind_rows(densite_all_param, new_rows)
        }
      } else if (lvl_type == "complex") {
        comp <- lvl
        if (!(comp %in% df_param$name)) {
          vals <- df_param$value[df_param$name == "GENUS"]
          new_rows <- data.frame(name = comp, param = p, value = vals)
          densite_all_param <- bind_rows(densite_all_param, new_rows)
        }
      }
    }
  }

  densite_all_param <- densite_all_param %>%
    filter(!str_detect(name, "unlabeled"))

  new_df_unique <- new_df %>% distinct()

  densite_all_param <- densite_all_param %>%
    left_join(new_df_unique, by = c("name" = "level2")) %>%
    mutate(
      name = ifelse(!is.na(level) & level == "complex", paste0(name, " complex"), name)
    ) %>%
    select(-level) %>%
    mutate(
      name = ifelse(name %in% c("Hyrcanus", "Jamesii"), paste0(name, " complex"), name)
    )

  genus_vals <- densite_all_param %>%
    filter(name == "GENUS")

  densite_all_param <- densite_all_param %>%
    filter(!name %in% c("Hyrcanus", "Hyrcanus complex", "Jamesii", "Jamesii complex"))

  hyrcanus_rows <- genus_vals %>% mutate(name = "Hyrcanus complex")
  jamesii_rows  <- genus_vals %>% mutate(name = "Jamesii complex")

  densite_all_param <- bind_rows(densite_all_param, hyrcanus_rows, jamesii_rows)

  return(densite_all_param)
}

densite_all_param_clean <- process_densite_data(densite_all_param)

vc_simul_IG2 <- run_vcc_simulation(2, densite_all_param_clean, df_wide_assenga_IG2, all_param_l_kappa, dd)
#load(system.file("demo", "vc_simul_IG2_v2.RData", package = "AnophelesBionomics"))

plot_vcc_results <- function(sim_results,
                             species_filter = NULL,
                             conf_level = 0.95,
                             palette_path = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Input/new_palette_density_plots.csv",
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
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 11)
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
        axis.text.y = element_text(size = 10, face = "bold"),
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
#ggsave(
#  filename = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/reduc_vc_IG2_partial.png",
#  plot = gg$p,
#  width = 811 / 96,
#  height = 373 / 96,
#  dpi = 96,
#  units = "in"
#)
gg <- plot_vcc_results(vc_simul_IG2$results, color = "#2E86C1")
#ggsave(
#  filename = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/reduc_vc_IG2.png",
#  plot = gg$p,
#  width = 753 / 96,
#  height = 1143 / 96,
#  dpi = 96,
#  units = "in"
#)


##############
#### PRCC ####
##############


prcc_vcc_genus <- function(n, df_wide_assenga, densite_all_param, genus_name = "GENUS") {
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

    df_wide <- df_wide_assenga

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

result_GENUS_IG2 <- prcc_vcc_genus(n = 50, df_wide_assenga = df_wide_assenga_IG2, densite_all_param = densite_all_param, genus_name = "GENUS")
#load(system.file("demo", "result_GENUS_IG2_v2.RData", package = "AnophelesBionomics"))

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
#ggsave(
#  filename = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/PRCC_IG2.png",
#  plot = ff,
#  width = 563 / 96,
#  height = 299 / 96,
#  dpi = 96,
#  units = "in"
#)

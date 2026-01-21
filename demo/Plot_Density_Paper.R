library(AnophelesBionomics)

path_plot  = "C:/Users/chamcl/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Bionomics_estimation_v5/Figures/"
path_excel = "C:/Users/chamcl/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Bionomics_estimation_v5/Figures/"

varnames <- c("endophagy", "endophily", "indoor_HBI", "outdoor_HBI", "parous_rate", "sac_rate","resting_duration")
output_dir <- path_plot

output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (varname in varnames) {
  data <- creation_df(varname)
  run_stan_result <- run_stan(data, iter = 3000)
  species_complex_result(run_stan_result, all = TRUE, output_dir = path_excel)
  saveRDS(run_stan_result$stan_file, file = file.path(path_excel, paste0(varname,"_stanoutput.rds") ))
  sum(sapply(get_sampler_params(run_stan_result$stan_file, inc_warmup = FALSE), function(x) sum(x[, "divergent__"])))
  complex_names_temp <- switch(
    varname,
    "endophagy"     = c("Gambiae", "Funestus", "Anopheles albimanus_complex"),
    "endophily"     = NULL,
    "indoor_HBI"    = c("Gambiae", "Nili", "Anopheles stephensi_complex"),
    "outdoor_HBI"   = c("Gambiae", "Nili", "Anopheles stephensi_complex"),
    "parous_rate"   = c("Barbirostris", "Dirus", "Gambiae", "Funestus", "Nuneztovari", "Punctulatus"),
    "sac_rate"      = c("Gambiae", "Funestus", "Anopheles albimanus_complex"),
    "resting_duration"      = c("Gambiae", "Funestus", "Subpictus"),
    NULL
  )
  output_path_full <- file.path(output_dir, paste0("density_", varname, ".png"))
  p_full <- plot_density(
    stan_results = run_stan_result,
    path = output_path_full,
    unlabel = FALSE
  )
  if (!is.null(complex_names_temp)) {
    output_path_partial <- file.path(output_dir, paste0("density_", varname, "_partial.png"))
    p_partial <- plot_density(
      stan_results = run_stan_result,
      path = output_path_partial,
      unlabel = FALSE,
      complex_names = complex_names_temp
    )

  }
}

data <- creation_df("endophagy",geo = c("Africa-E"))
run_stan_result <- run_stan(data)
p <- plot_density(stan_results = run_stan_result,
                  complex_names = c("Gambiae","Funestus"),
                  #path = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/density_endophagy_africa_E_partial.png",
                  path = file.path(path_plot,"density_endophagy_africa_E_partial.png"),
                  unlabel = FALSE)
saveRDS(run_stan_result$stan_file, file = file.path(path_excel, "endophagy_EastAfrica_stanoutput.rds") )
run_stan_result$varname="endophagy_EAfrica"
species_complex_result(run_stan_result, all = TRUE, output_dir = path_excel)

data <- creation_df("endophagy",geo = c("Africa-W"))
run_stan_result <- run_stan(data)
p <- plot_density(stan_results = run_stan_result,
                  complex_names = c("Gambiae","Funestus"),
                  #path =  "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/density_endophagy_africa_W_partial.png",
                  path = file.path(path_plot,"density_endophagy_africa_W_partial.png"),
                  unlabel = FALSE)
saveRDS(run_stan_result$stan_file, file = file.path(path_excel, "endophagy_WestAfrica_stanoutput.rds") )
run_stan_result$varname="endophagy_WAfrica"
species_complex_result(run_stan_result, all = TRUE, output_dir = path_excel)

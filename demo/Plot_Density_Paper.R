library(AnophelesBionomics)

path_plot  = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output"
path_excel = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output"

varnames <- c("resting_duration","endophagy", "endophily", "indoor_HBI", "outdoor_HBI", "parous_rate", "sac_rate","resting_duration")
output_dir <- path

output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (varname in varnames) {
  data <- creation_df(varname)
  run_stan_result <- run_stan(data)
  species_complex_result(run_stan_result, all = TRUE, output_dir = path_excel)
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
                  path = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/density_endophagy_africa_E_partial.png",
                  unlabel = FALSE)

data <- creation_df("endophagy",geo = c("Africa-W"))
run_stan_result <- run_stan(data)
p <- plot_density(stan_results = run_stan_result,
                  complex_names = c("Gambiae","Funestus"),
                  path =  "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output/density_endophagy_africa_W_partial.png",
                  unlabel = FALSE)
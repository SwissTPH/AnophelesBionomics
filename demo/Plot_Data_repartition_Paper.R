library(AnophelesBionomics)
library(dplyr)
library(ggplot2)

path_plot  = "C:/Users/tarrau/switchdrive/AIM/2. Methodological development/16. Bionomics using hierarchical model/2025/Demo/Output"

multi_species_pie(seuil_prop_autres = 0.05,plot_dir = path_plot)

varnames <- c("endophagy", "endophily", "indoor_HBI", "outdoor_HBI",
              "parous_rate", "sac_rate", "resting_duration")
for (var in varnames) {
  data <- creation_df(var)
  file_path <- paste0(path_plot, var, ".png")
  p <- plot_lolipop (data, var, path = file_path)
  print(p)
}

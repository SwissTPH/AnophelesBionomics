# AnophelesBionomics

## Presentation

The **AnophelesBionomics** R package provides tools to estimate key mosquito bionomics parameters using a Bayesian hierarchical model. This model leverages taxonomic relationships—species, species complex, and genus—to strengthen inference, especially when species-level data are limited or missing.

The model estimates several important bionomics parameters: the parous rate, endophagy, endophily, human blood index (HBI),indoor HBI, outdoorHBI and sac rate. Even in the absence of specific data for a species, the hierarchical structure allows the model to draw on information from related taxa.

AnophelesBionomics supports both internal and user-provided datasets. It also includes tools for data preparation, model diagnostics, and result exploration.

The package depends on several R libraries for core functionality, including `stats`, `dplyr`, `ggplot2`, `rstan`, `plotly`, `coda`, `htmlwidgets`, `shiny`, `cowplot`, `rlang`, `stringr`, and `tidyr`.

## Installation

To install the package:

```r
# install.packages("devtools")
devtools::install_github("SwissTPH/AnophelesBionomics")
```

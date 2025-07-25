#' @title Launch Interactive Density Explorer for Stan Model
#'
#' @description
#' Starts a Shiny app to interactively explore posterior densities by species complex.
#'
#' @param stan_results A list containing the Stan model and metadata (same structure as required in `plot_density_interactive()`).
#'
#' @return Launches a Shiny application; does not return a value.
#'
#' @details
#' The app allows users to select a species complex, optionally exclude species with "unlabel" in their names, and view an interactive plot of normalized posterior densities.
#'
#'
#' @export
launch_density_app <- function(stan_results) {

  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive Density Explorer by Complex"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          inputId = "selected_complex",
          label = "Choose a species complex:",
          choices = NULL
        ),
        shiny::checkboxInput(
          inputId = "unlabel_filter",
          label = "Exclude species with 'unlabel' in the name",
          value = TRUE
        )
      ),
      shiny::mainPanel(
        plotly::plotlyOutput("density_plot")
      )
    )
  )

  server <- function(input, output, session) {
    SP <- stan_results$species_complex

    clean_complex_names <- function(x) {
      gsub("_complex$", "", x)
    }

    SP <- SP |>
      dplyr::mutate(clean_complex = clean_complex_names(complex))

    complexes <- unique(SP$clean_complex)

    shiny::updateSelectInput(session, "selected_complex", choices = complexes, selected = complexes[1])

    output$density_plot <- plotly::renderPlotly({
      req(input$selected_complex)
      selected_clean_complex <- input$selected_complex

      original_complexes <- SP |>
        dplyr::filter(clean_complex == selected_clean_complex) |>
        dplyr::pull(complex) |>
        unique()

      complex_name <- original_complexes[1]

      unlabel <- input$unlabel_filter
      stan_fit <- stan_results$fit
      nice_varname <- stan_results$nice_varname
      df <- prepare_data_density(stan_results)

      associated_species <- SP |>
        dplyr::filter(clean_complex == selected_clean_complex) |>
        dplyr::pull(species)

      df_filtered <- df |> dplyr::filter(name %in% c("GENUS", complex_name, associated_species))

      if (unlabel) {
        df_filtered <- df_filtered |> dplyr::filter(!grepl("unlabel", name, ignore.case = TRUE))
      }

      df_filtered$name <- ifelse(df_filtered$name == "GENUS", "Anopheles", df_filtered$name)

      df_filtered$level <- dplyr::case_when(
        df_filtered$name == "Anopheles" ~ "genus",
        df_filtered$name == complex_name ~ "complex",
        TRUE ~ "species"
      )

      df_filtered$name <- ifelse(df_filtered$name == complex_name, selected_clean_complex, df_filtered$name)

      line_types <- data.frame(
        level = c("genus", "complex", "species"),
        line_type = c("dotted", "longdash", "solid")
      )

      df_filtered <- dplyr::left_join(df_filtered, line_types, by = "level")

      colors_map <- read_data_file(file = "new_palette_density_plots.csv")

      if (!"Anopheles" %in% colors_map$name) {
        colors_map <- dplyr::bind_rows(data.frame(name = "Anopheles", pal = "black"), colors_map)
      }

      missing_names <- setdiff(unique(df_filtered$name), colors_map$name)
      if (length(missing_names) > 0) {
        colors_map <- dplyr::bind_rows(colors_map, data.frame(name = missing_names, pal = "#D3D3D3"))
      }

      df_filtered <- dplyr::left_join(df_filtered, colors_map, by = "name")

      est_df <- NULL
      if (!is.null(stan_fit)) {
        summary_stats <- rstan::summary(stan_fit)$summary
        param_names <- rownames(summary_stats)

        genus_est <- data.frame(
          name = "Anopheles",
          level = "genus",
          estimate = summary_stats["p1[1]", "mean"],
          stringsAsFactors = FALSE
        )

        p1_idx <- grep("^p1\\[", param_names)
        p1_values <- summary_stats[p1_idx, "mean"]
        names(p1_values) <- gsub("p1\\[|\\]", "", param_names[p1_idx])

        complex_all <- SP[SP$complexNb != 1, ]
        complex_all <- unique(complex_all[, c("complex", "complexNb")])
        complex_all <- data.frame(
          name = complex_all$complex,
          level = rep("complex", nrow(complex_all)),
          estimate = p1_values[as.character(complex_all$complexNb)],
          stringsAsFactors = FALSE
        )

        p2_idx <- grep("^p2\\[", param_names)
        p2_values <- summary_stats[p2_idx, "mean"]
        names(p2_values) <- gsub("p2\\[|\\]", "", param_names[p2_idx])

        species_all <- SP[SP$speciesNb != 0, ]
        species_all <- unique(species_all[, c("species", "speciesNb")])
        species_all <- data.frame(
          name = species_all$species,
          level = rep("species", nrow(species_all)),
          estimate = p2_values[as.character(species_all$speciesNb)],
          stringsAsFactors = FALSE
        )

        est_df <- rbind(genus_est, complex_all, species_all)
      }

      dens_data <- df_filtered |>
        dplyr::group_by(name) |>
        dplyr::summarise(
          level = dplyr::first(level),
          line_type = dplyr::first(line_type),
          pal = dplyr::first(pal),
          values = list(value),
          .groups = "drop"
        )

      dens_data_final <- do.call(rbind, lapply(seq_len(nrow(dens_data)), function(i) {
        vec <- dens_data$values[[i]]
        d <- stats::density(vec, na.rm = TRUE)
        y_norm <- d$y / max(d$y)
        data.frame(
          name = rep(dens_data$name[i], length(d$x)),
          level = rep(dens_data$level[i], length(d$x)),
          line_type = rep(dens_data$line_type[i], length(d$x)),
          pal = rep(dens_data$pal[i], length(d$x)),
          x = d$x,
          y = y_norm,
          stringsAsFactors = FALSE
        )
      }))

      if (!is.null(est_df)) {
        dens_data_final <- dplyr::left_join(dens_data_final, est_df, by = c("name", "level"))
      }

      p <- ggplot2::ggplot(dens_data_final, ggplot2::aes(
        x = x, y = y, color = name, linetype = level,
        text = paste0("Name: ", name,
                      if (!is.null(est_df)) paste0("<br>Estimate: ", round(estimate, 3)) else "")
      )) +
        ggplot2::geom_line(size = 1.2, alpha = 0.8) +
        ggplot2::scale_color_manual(values = setNames(colors_map$pal, colors_map$name)) +
        ggplot2::scale_linetype_manual(values = setNames(line_types$line_type, line_types$level)) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = nice_varname, x = "Value", y = "Normalized Density") +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
          text = ggplot2::element_text(size = 16)
        )

      plotly::ggplotly(p, tooltip = "text")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

###############################################################################
################################## 5. IRF'S PLOT ##############################
###############################################################################

dirs <- c(outer(
  c("Model_Baseline", "Model_Foreign", "Model_Narrative", "Model_Housing"),
  c("Durable", "Habitual"),
  function(m, type) file.path("4_Output_Analysis", m, type)
))

sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)


#=============================================================================#
#                          [1]  MODEL HOUSING (DURABLE)

variables_durables <- c("Unemployment", "Interest", "House Price", 
                       "DTA", "DSR", "LAI", "Savings", "Durables")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Housing/Durable/housing_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Housing/Durable",
    filename = paste0("IRF_Housing_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2, 3),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    ),
    title = paste0(
      "Housing Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Housing/Durable",
      paste0("IRF_Housing_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}



#=============================================================================#
#                          [2]  MODEL HOUSING (HABITUAL)

variables_habitual <- c("Unemployment", "Interest", "House Price", 
                       "DTA", "DSR", "LAI", "Savings", "Habituals")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Housing/Habitual/housing_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habitual,
    output_dir = "4_Output_Analysis/Model_Housing/Habitual",
    filename = paste0("IRF_Housing_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2, 3),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    ),
    title = paste0(
      "Housing Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habitual,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Housing/Habitual",
      paste0("IRF_Housing_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                          [3]  MODEL MACRO (DURABLE)

variables_durables <- c("GDP-D", "Unemployment", "CPIF", "Interest", "Durables",
 "DTA", "DSR", "LAI", "Savings", "REER")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Baseline/Durable/baseline_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Baseline/Durable",
    filename = paste0("IRF_Baseline_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Macro Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Baseline/Durable",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                          [4]  MODEL MACRO (HABITUAL)

variables_habituals <- c("GDP-D", "Unemployment", "CPIF", "Interest", "Habituals", "DTA", "DSR", "LAI", "Savings", "REER")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habituals,
    output_dir = "4_Output_Analysis/Model_Baseline/Habitual",
    filename = paste0("IRF_Baseline_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Macro Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habituals,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Baseline/Habitual",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                       [5]  MODEL MACRO DURABLE (NARRATIVE)

variables_durables <- c("GDP-D", "Unemployment", "CPIF", "Interest", 
"Durables", "DTA", "DSR", "LAI", "Savings", "REER")


for (p in 1:2) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Narrative/Durable/narrative_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Narrative/Durable",
    filename = paste0("IRF_Narrative_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Narrative Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Narrative/Durable",
      paste0("IRF_Narrative_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                       [6]  MODEL MACRO DURABLE (NARRATIVE)

variables_habituals<- c("GDP-D", "Unemployment", "CPIF", "Interest", 
"Habitual", "DTA", "DSR", "LAI", "Savings", "REER")


for (p in 1:2) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Narrative/Habitual/narrative_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habituals,
    output_dir = "4_Output_Analysis/Model_Narrative/Habitual",
    filename = paste0("IRF_Narrative_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Narrative Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habituals,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Narrative/Habitual",
      paste0("IRF_Narrative_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}



#=============================================================================#
#                       [7]  MODEL MACRO SMALL OPEN ECONOMY (DURABLE)


variables_durables <- c("FED Rate", "KIX GDP", "KIX CPI", "GDP-D", "Unemployment", "CPIF", "Interest", "Durables", "DTA", "DSR", "LAI", "Savings", "REER")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Foreign/Durable/foreign_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Foreign/Durable",
    filename = paste0("IRF_Foreign_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(7, 4), # Very Important to change or you'll plot the wrong shocks!
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Baseline Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Foreign/Durable",
      paste0("IRF_Foreign_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}





#=============================================================================#
#                       [8]  MODEL MACRO SMALL OPEN ECONOMY (HABITUAL)


variables_habitual_foreign <- c("FED Rate", "KIX GDP", "KIX CPI",
 "GDP-D", "Unemployment", "CPIF", "Interest", "Habituals", "DTA",
  "DSR", "LAI", "Savings", "REER")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Foreign/Habitual/foreign_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habitual_foreign,
    output_dir = "4_Output_Analysis/Model_Foreign/Habitual",
    filename = paste0("IRF_Foreign_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(7, 4),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Baseline Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habitual_foreign,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Foreign/Habitual",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}





















# ============================================================
# FUNCTION: Create publication-ready IRF plot
# ============================================================

plot_irf_two_shocks <- function(
    model,
    variable_names,
    output_dir = "Test",
    filename = "IRF_2shocks.png",
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = "Impulse Response Functions",
    system_label = "Durable Goods System",
    width = 11,
    height = 13,
    dpi = 300
) {

  # ----------------------------------------------------------
  # 1. Compute IRFs
  # ----------------------------------------------------------

  irf_draws <- compute_impulse_responses(
    model,
    horizon = horizon
  )

  full_dim <- dim(irf_draws)

  cat(
    "\nFull IRF dimensions:",
    paste(full_dim, collapse = " x "),
    "\n"
  )


  # ----------------------------------------------------------
  # 2. Check shock indices
  # ----------------------------------------------------------

  if (any(shock_indices > full_dim[2])) {
    stop(
      "One or more shock_indices exceed the number of shocks ",
      "in the model."
    )
  }

  if (length(shock_indices) != length(shock_labels)) {
    stop(
      "shock_indices and shock_labels must have the same length."
    )
  }


  # ----------------------------------------------------------
  # 3. Select requested shocks
  # ----------------------------------------------------------

  irf_selected <- irf_draws[
    , shock_indices, , ,
    drop = FALSE
  ]

  cat(
    "Selected IRF dimensions:",
    paste(dim(irf_selected), collapse = " x "),
    "\n"
  )


  # ----------------------------------------------------------
  # 4. Check variable names
  # ----------------------------------------------------------

  if (length(variable_names) != dim(irf_selected)[1]) {
    stop(
      "The number of variable_names (",
      length(variable_names),
      ") does not match the number of response variables (",
      dim(irf_selected)[1],
      ")."
    )
  }


  # ----------------------------------------------------------
  # 5. Calculate posterior summaries
  # ----------------------------------------------------------

  irf_list <- list()
  counter <- 1

  n_variables <- dim(irf_selected)[1]
  n_shocks    <- dim(irf_selected)[2]
  n_horizons  <- dim(irf_selected)[3]

  for (s_idx in seq_len(n_shocks)) {

    for (v_idx in seq_len(n_variables)) {

      # horizon x posterior draw
      x <- irf_selected[v_idx, s_idx, , ]

      # 95% posterior interval
      q025 <- apply(
        x,
        1,
        quantile,
        probs = 0.025,
        na.rm = TRUE
      )

      q975 <- apply(
        x,
        1,
        quantile,
        probs = 0.975,
        na.rm = TRUE
      )

      # 68% posterior interval
      q16 <- apply(
        x,
        1,
        quantile,
        probs = 0.16,
        na.rm = TRUE
      )

      q84 <- apply(
        x,
        1,
        quantile,
        probs = 0.84,
        na.rm = TRUE
      )

      # Posterior median
      q50 <- apply(
        x,
        1,
        quantile,
        probs = 0.50,
        na.rm = TRUE
      )

      irf_list[[counter]] <- data.frame(

        horizon = 0:(n_horizons - 1),

        shock = shock_labels[s_idx],

        variable = variable_names[v_idx],

        lower_95 = q025,
        upper_95 = q975,

        lower_68 = q16,
        upper_68 = q84,

        median = q50
      )

      counter <- counter + 1
    }
  }

  irf_df <- bind_rows(irf_list)


  # ----------------------------------------------------------
  # 6. Preserve ordering
  # ----------------------------------------------------------

  irf_df <- irf_df %>%
    mutate(

      variable = factor(
        variable,
        levels = variable_names
      ),

      shock = factor(
        shock,
        levels = shock_labels
      )
    )


  # ----------------------------------------------------------
  # 7. Create plot
  # ----------------------------------------------------------

  p_irf <- ggplot(
    irf_df,
    aes(
      x = horizon,
      y = median
    )
  ) +

    # 95% posterior credible interval
    geom_ribbon(
      aes(
        ymin = lower_95,
        ymax = upper_95
      ),
      fill = "#2B6CB0",
      alpha = 0.12
    ) +

    # 68% posterior credible interval
    geom_ribbon(
      aes(
        ymin = lower_68,
        ymax = upper_68
      ),
      fill = "#2B6CB0",
      alpha = 0.28
    ) +

    # Posterior median
    geom_line(
      color = "#1A365D",
      linewidth = 0.9
    ) +

    # Zero / baseline
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "#C53030",
      linewidth = 0.6
    ) +

    # Response variables x structural shocks
    facet_grid(
      variable ~ shock,
      scales = "free_y"
    ) +

    scale_x_continuous(
      breaks = seq(
        0,
        horizon,
        by = 5
      ),
      limits = c(0, horizon),
      expand = c(0.01, 0.01)
    ) +

    labs(
      title = title,

      subtitle = paste0(
        system_label,
        " | ",
        horizon,
        "-Quarter Horizon"
      ),

      x = "Horizon (Quarters)",

      y = "Response",

      caption = paste0(
        "Dark band: 68% posterior credible interval ",
        "(16th–84th percentiles). ",
        "Light band: 95% posterior credible interval ",
        "(2.5th–97.5th percentiles)."
      )
    ) +

    theme_bw(
      base_size = 11
    ) +

    theme(

      strip.background = element_rect(
        fill = "#EDF2F7"
      ),

      strip.text = element_text(
        face = "bold"
      ),

      panel.grid.minor = element_blank(),

      panel.grid.major = element_line(
        linewidth = 0.25
      ),

      axis.text = element_text(
        color = "black"
      ),

      axis.title = element_text(
        face = "bold"
      ),

      plot.title = element_text(
        face = "bold",
        size = 14
      ),

      plot.subtitle = element_text(
        size = 10
      ),

      plot.caption = element_text(
        hjust = 0,
        size = 9
      ),

      panel.spacing = unit(
        0.8,
        "lines"
      )
    )


  # ----------------------------------------------------------
  # 8. Create output directory
  # ----------------------------------------------------------

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )


  # ----------------------------------------------------------
  # 9. Save plot
  # ----------------------------------------------------------

  plot_path <- file.path(
    output_dir,
    filename
  )

  ggsave(
    plot_path,
    plot = p_irf,
    width = width,
    height = height,
    dpi = dpi
  )


  # ----------------------------------------------------------
  # 10. Report
  # ----------------------------------------------------------

  cat(
    "IRF plot successfully saved to:\n",
    plot_path,
    "\n"
  )


  # ----------------------------------------------------------
  # 11. Return useful objects
  # ----------------------------------------------------------
  #
  # Returning both the plot and data means you can later
  # modify the figure without recomputing the IRFs.

  invisible(
    list(
      plot = p_irf,
      data = irf_df,
      irfs = irf_selected,
      full_irfs = irf_draws
    )
  )
}








make_irf_summary_table <- function(
    irf_object,
    variable_names,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
) {

  # ------------------------------------------------------------
  # 1. Extract IRF array
  # ------------------------------------------------------------

  irf_array <- irf_object$irfs

  dims <- dim(irf_array)

  n_variables <- dims[1]
  n_shocks    <- dims[2]
  n_horizons  <- dims[3]

  available_horizons <- 0:(n_horizons - 1)


  # ------------------------------------------------------------
  # 2. Checks
  # ------------------------------------------------------------

  if (length(variable_names) != n_variables) {
    stop(
      "variable_names has length ", length(variable_names),
      " but the IRF contains ", n_variables,
      " response variables."
    )
  }

  if (length(shock_labels) != n_shocks) {
    stop(
      "shock_labels has length ", length(shock_labels),
      " but the IRF contains ", n_shocks,
      " shocks."
    )
  }


  # ------------------------------------------------------------
  # 3. Results container
  # ------------------------------------------------------------

  results <- list()
  counter <- 1


  # ------------------------------------------------------------
  # 4. Loop over shocks and variables
  # ------------------------------------------------------------

  for (s in seq_len(n_shocks)) {

    for (v in seq_len(n_variables)) {

      # Posterior draws:
      # horizon x posterior draw
      draws <- irf_array[v, s, , ]


      # --------------------------------------------------------
      # Posterior median at each horizon
      # --------------------------------------------------------

      median_irf <- apply(
        draws,
        1,
        median,
        na.rm = TRUE
      )


      # --------------------------------------------------------
      # 95% posterior interval
      # --------------------------------------------------------

      lower_95 <- apply(
        draws,
        1,
        quantile,
        probs = 0.025,
        na.rm = TRUE
      )

      upper_95 <- apply(
        draws,
        1,
        quantile,
        probs = 0.975,
        na.rm = TRUE
      )


      # --------------------------------------------------------
      # Impact response: Q0
      # --------------------------------------------------------

      impact_response <- median_irf[1]


      # --------------------------------------------------------
      # Peak response
      #
      # Defined as the response with the largest absolute
      # median magnitude over the IRF horizon.
      # --------------------------------------------------------

      peak_index <- which.max(
        abs(median_irf)
      )

      peak_response <- median_irf[peak_index]

      peak_horizon <- available_horizons[
        peak_index
      ]


      # --------------------------------------------------------
      # 95% CI at the peak
      # --------------------------------------------------------

      peak_lower_95 <- lower_95[
        peak_index
      ]

      peak_upper_95 <- upper_95[
        peak_index
      ]


      # --------------------------------------------------------
      # Does the 95% credible interval exclude zero?
      # --------------------------------------------------------

      peak_significant <-
        peak_lower_95 > 0 ||
        peak_upper_95 < 0


      significance_marker <-
        ifelse(
          peak_significant,
          "*",
          ""
        )


      # --------------------------------------------------------
      # Store result
      # --------------------------------------------------------

      results[[counter]] <- data.frame(

        Variable = variable_names[v],

        Shock = shock_labels[s],

        Impact_Q0 = impact_response,

        Peak_Response = peak_response,

        Peak_Horizon = peak_horizon,

        Peak_95_Significant = significance_marker,

        stringsAsFactors = FALSE

      )

      counter <- counter + 1
    }
  }


  # ------------------------------------------------------------
  # 5. Combine results
  # ------------------------------------------------------------

  summary_table <- dplyr::bind_rows(
    results
  )


  # ------------------------------------------------------------
  # 6. Return table
  # ------------------------------------------------------------

  return(summary_table)
}

make_irf_summary_table <- function(
    irf_object,
    variable_names,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    horizons_to_report = c(0, 10, 20, 30)
) {

  # Use the selected IRFs returned by plot_irf_two_shocks()
  irf_array <- irf_object$irfs

  dims <- dim(irf_array)

  n_variables <- dims[1]
  n_shocks    <- dims[2]
  n_horizons  <- dims[3]
  n_draws     <- dims[4]

  # Basic checks
  if (length(variable_names) != n_variables) {
    stop(
      "variable_names has length ", length(variable_names),
      " but the IRF object contains ", n_variables,
      " response variables."
    )
  }

  if (length(shock_labels) != n_shocks) {
    stop(
      "shock_labels has length ", length(shock_labels),
      " but the IRF object contains ", n_shocks,
      " shocks."
    )
  }

  available_horizons <- 0:(n_horizons - 1)

  if (any(!horizons_to_report %in% available_horizons)) {
    stop(
      "One or more requested horizons are outside the IRF horizon."
    )
  }

  results <- list()
  counter <- 1

  for (s in seq_len(n_shocks)) {

    for (v in seq_len(n_variables)) {

      # Extract posterior draws for this response/shock
      draws <- irf_array[v, s, , ]

      # Posterior median at every horizon
      median_irf <- apply(
        draws,
        1,
        median,
        na.rm = TRUE
      )

      # 95% posterior interval
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

      # --------------------------------------------------
      # Peak = largest absolute median response
      # --------------------------------------------------

      peak_index <- which.max(abs(median_irf))

      peak_horizon <- available_horizons[peak_index]
      peak_response <- median_irf[peak_index]

      peak_lower_95 <- lower_95[peak_index]
      peak_upper_95 <- upper_95[peak_index]

      peak_significant <- (
        peak_lower_95 > 0 ||
        peak_upper_95 < 0
      )

      # --------------------------------------------------
      # Responses at selected horizons
      # --------------------------------------------------

      horizon_values <- median_irf[
        match(horizons_to_report, available_horizons)
      ]

      # --------------------------------------------------
      # Store results
      # --------------------------------------------------

      results[[counter]] <- data.frame(

        Variable = variable_names[v],

        Shock = shock_labels[s],

        Impact_Q0 = horizon_values[
          horizons_to_report == 0
        ],

        Peak_Response = peak_response,

        Peak_Horizon = peak_horizon,

        Peak_Lower_95 = peak_lower_95,

        Peak_Upper_95 = peak_upper_95,

        Peak_95_CI_Excludes_Zero =
          ifelse(peak_significant, "Yes", "No"),

        stringsAsFactors = FALSE

      )

      # Add requested horizon columns
      for (h in horizons_to_report) {

        column_name <- paste0("Response_Q", h)

        results[[counter]][[column_name]] <-
          median_irf[
            match(h, available_horizons)
          ]
      }

      counter <- counter + 1
    }
  }

  summary_table <- dplyr::bind_rows(results)

  # Reorder columns
  summary_table <- summary_table %>%
    dplyr::select(
      Variable,
      Shock,
      Impact_Q0,
      Peak_Response,
      Peak_Horizon,
      Peak_Lower_95,
      Peak_Upper_95,
      Peak_95_CI_Excludes_Zero,
      dplyr::starts_with("Response_Q")
    )

  return(summary_table)
}





















get_irf_median_df <- function(
    irf_obj,
    response_var,
    shock_var,
    label_name = "Baseline",
    target_cols = NULL
) {

  # ----------------------------------------------------------
  # 1. Extract IRF array
  # ----------------------------------------------------------

  arr <- if (is.array(irf_obj)) {

    irf_obj

  } else if (!is.null(irf_obj$posterior$irf)) {

    irf_obj$posterior$irf

  } else if (!is.null(irf_obj$irf)) {

    irf_obj$irf

  } else {

    stop(
      "Unrecognized IRF object. ",
      "Pass the output from compute_impulse_responses()."
    )
  }

  if (length(dim(arr)) != 4) {
    stop(
      "IRF array must have dimensions ",
      "[response x shock x horizon x draws]."
    )
  }

  # ----------------------------------------------------------
  # 2. Variable names
  # ----------------------------------------------------------

  if (is.null(target_cols)) {

    target_cols <- dimnames(arr)[[1]]

  }

  if (is.null(target_cols)) {

    stop(
      "target_cols must be supplied because the IRF ",
      "array has no variable names."
    )

  }

  # ----------------------------------------------------------
  # 3. Resolve response index
  # ----------------------------------------------------------

  if (is.numeric(response_var)) {

    r_idx <- response_var

  } else {

    r_idx <- match(
      response_var,
      target_cols
    )

  }

  # ----------------------------------------------------------
  # 4. Resolve shock index
  # ----------------------------------------------------------

  if (is.numeric(shock_var)) {

    s_idx <- shock_var

  } else {

    s_idx <- match(
      shock_var,
      target_cols
    )

  }

  if (
    is.na(r_idx) ||
    r_idx < 1 ||
    r_idx > dim(arr)[1]
  ) {

    stop(
      "Invalid response_var."
    )

  }

  if (
    is.na(s_idx) ||
    s_idx < 1 ||
    s_idx > dim(arr)[2]
  ) {

    stop(
      "Invalid shock_var."
    )

  }

  # ----------------------------------------------------------
  # 5. Extract posterior draws
  # ----------------------------------------------------------

  sub_mat <- arr[
    r_idx,
    s_idx,
    ,
    ,
    drop = FALSE
  ]

  sub_mat <- matrix(
    sub_mat,
    nrow = dim(arr)[3],
    ncol = dim(arr)[4]
  )

  # ----------------------------------------------------------
  # 6. Posterior summaries
  # ----------------------------------------------------------

  data.frame(

    Horizon = 0:(nrow(sub_mat) - 1),

    Mean = apply(
      sub_mat,
      1,
      mean,
      na.rm = TRUE
    ),

    Median = apply(
      sub_mat,
      1,
      median,
      na.rm = TRUE
    ),

    Lower_95 = apply(
      sub_mat,
      1,
      quantile,
      probs = 0.025,
      na.rm = TRUE
    ),

    Lower_68 = apply(
      sub_mat,
      1,
      quantile,
      probs = 0.16,
      na.rm = TRUE
    ),

    Upper_68 = apply(
      sub_mat,
      1,
      quantile,
      probs = 0.84,
      na.rm = TRUE
    ),

    Upper_95 = apply(
      sub_mat,
      1,
      quantile,
      probs = 0.975,
      na.rm = TRUE
    ),

    Specification = label_name,

    stringsAsFactors = FALSE
  )
}

















# ============================================================
# FUNCTION: Create publication-ready IRF plot
# ============================================================

plot_irf_two_shocks_test <- function(
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









###############################################################################
###############################  0b.FUNCTIONS  ################################
###############################################################################

#=========================# FUNCTION USED IN DATA CREATION
#Functions in order of appearance with purpuose.e
# (1) download_pxweb
# (2) get_riksbank_series
#=========================# FUNCTIONS FOR MODEL DIAGNOSTICS
# (3) check_posterior_stability
#=========================# FUNCTIONS FOR IRFS
# (4) plot_irf_two_shocks
# (5) make_irf_summary_table

#============================================================================#
#                     [1] FUNCTION USED IN DATA CREATION
#============================================================================#

## (1) download_pxweb
# Purpose: SCB PXWEB downloader, helps to streamline the download 
#from SCB APIs using the pwxweb query.

download_pxweb <- function(url, query){

  pxq <- pxweb_query(query) # Converts list into a valid PX-Web query object.
  pxweb_get_data( # Executes the HTTP call to SCB and retrieves the data
    url = url,  # API URL
    query = pxq,
    column.name.type = "text",
    variable.value.type = "text"
  )
}


#------------------------------------------------------------------------------#

## (2) get_riksbank_series
# Purpose: Helps to streamline the download from Riksbanken API
# originally I downloaded multiple items from Riksbanken (deposit_rate, lending_rate)
# so it was necessary then, but not strictly necessary now.
get_riksbank_series <- function(series_id,
                                from = "1995-01-01",
                                to = Sys.Date()) {

  url <- paste0(
    "https://api.riksbank.se/swea/v1/Observations/",
    series_id, "/", from, "/", to
  )

  response <- httr::GET(url)
  httr::stop_for_status(response)

  # Explicitly using jsonlite::fromJSON guarantees row-bind behavior
  raw_text <- httr::content(response, "text", encoding = "UTF-8")
  parsed   <- jsonlite::fromJSON(raw_text, simplifyVector = TRUE)

  # Explicitly bind list elements into rows instead of relying on generic as.data.frame
  data.frame(parsed)
}


#============================================================================#
#                     [2] FUNCTIONS FOR MODEL DIAGNOSTICS
#============================================================================#
# Functions here were originally intend  to avoid large codes chain in the 
# model estimation, ease extractions, and quickly inform me if I needed to 
# adjust things for a better model output, instead of looking it myself.
# However, they assumed the same number of variables in their run, which
# lead to confusionn when later model where computed. I therefore decided to
# cut it down to just one.

## (3) check_posterior_stability 
# Purpose: This function allows to compute the stability of posteriors within
# a model.

# Helper function to get max modulus (spectral radius) for a fitted posterior object
check_posterior_stability <- function(posterior_obj, p) {
  # Extract autoregressive matrices: dim is (N x K x S)
  # K = (N * p) + intercepts/trends
  A_draws <- posterior_obj$posterior$A
  N <- dim(A_draws)[1]
  
  apply(A_draws, 3, function(A_mat) {
    # Extract only VAR lag parameters (first N*p columns)
    A_lags <- A_mat[, 1:(N * p)]
    
    # Construct Companion Matrix
    if (p == 1) {
      companion <- A_lags
    } else {
      top_block <- A_lags
      bottom_block <- cbind(diag(N * (p - 1)), matrix(0, nrow = N * (p - 1), ncol = N))
      companion <- rbind(top_block, bottom_block)
    }
    
    # Return maximum absolute eigenvalue
    max(abs(eigen(companion, only.values = TRUE)$values))
  })
}


#============================================================================#
#                     [4] FUNCTIONS FOR IRF
#============================================================================#
# Functions here are here to ease the way in which information about
# IRFs is extracted. It was not strictly necssary but I found that not
# having the same plot repeated over again saves on line of code.

## (4) Plot_Irf_Two_Shocks
# Purpose: This function allows to plot just one of the response in the IRF of
# the Bsvarsign package. Bsvarsign plots the variable as:
# Variable * Variable * Type of Shock. This means that a 10 variable system with
# two shocks will be 10 * 10 * 2. That's 200 plot within the same image! 
# R will not allow to compute this. This functions allow to plot one or more shocks.
# or rather one column out of the 20.

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



## (5) make_irf_summary_table
# Purpose: This function allows to summarize the most important result
# out of the IRF for the results section

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
      " but the IRF contains ", n_variables, " response variables."
    )
  }

  if (length(shock_labels) != n_shocks) {
    stop(
      "shock_labels has length ", length(shock_labels),
      " but the IRF contains ", n_shocks, " shocks."
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
      # 68% posterior interval
      # --------------------------------------------------------
      lower_68 <- apply(
        draws,
        1,
        quantile,
        probs = 0.16,
        na.rm = TRUE
      )
      upper_68 <- apply(
        draws,
        1,
        quantile,
        probs = 0.84,
        na.rm = TRUE
      )
      # --------------------------------------------------------
      # 68% CI at the peak
      # --------------------------------------------------------
      peak_lower_68 <- lower_68[
        peak_index
      ]
      peak_upper_68 <- upper_68[
        peak_index
      ]
      # --------------------------------------------------------
      # Does the 68% credible interval exclude zero?
      # --------------------------------------------------------
      peak_significant <-
        peak_lower_68 > 0 ||
        peak_upper_68 < 0

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
        Peak_68_Significant = significance_marker,
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

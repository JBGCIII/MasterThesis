###############################################################################
################################# 0.FUNCTIONS #################################
###############################################################################

#=========================# FUNCTION USED IN DATA CREATION
#Functions in order of appearance with purpuose.e
# (1) download_pxweb
# (2) get_riksbank_series
#=========================# FUNCTIONS FOR MODEL DIAGNOSTICS
# (3) export_hyperparameters
# (4) compute_and_export_ess
#=========================# FUNCTIONS FOR IRFS
# (5) export_bvar_results_to_csv
# (6) get_irf_median_df
# (7) plot_single_irf
# (8) plot_bvar_irf
#=========================# FUNCTIONS FOR FEVD (Separate Script)
# (9) compute_fevd_bsvarSIGN_safe
# (10) plot_fevd_one_shock
#=========================# FUNCTIONS FOR HD (Separate Script)
# (11) compute_hd_bsvarSIGN_safe

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
# Functions here are mostly to avoid large codes chain in the model estimation,
# ease extractions, and quickly inform me if I needed to adjust things for a
# better model output, instead of looking it myself. I felt it was really
# usefull.

## (3) export_hyperparameters
# Purpose: This function allows to easily extract hyperparamaters for a
# estimated model for diagnostics.

export_hyperparameters <- function(estimation_obj, target_cols, file_path = "Output/hyperparameters.csv") {
  
  # Ensure target directory exists
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  hyper_names <- c(
    paste0("Psi_", target_cols),
    "lambda (Overall Tightness)",
    "alpha (Lag Decay Exponent)",
    "a_0 (Gamma Shape)",
    "mu (Sum-of-Coeff Weight)",
    "mu_mu (Prior Mean mu)",
    "sigma_mu (Prior SD mu)",
    "delta (Unit-Root Weight)"
  )
  
  hyper_draws <- estimation_obj$posterior$hyper
  
  hyper_summary <- t(apply(hyper_draws, 1, function(x) c(
    Mean   = mean(x),
    SD     = sd(x),
    q025   = as.numeric(quantile(x, 0.025)),
    Median = median(x),
    q975   = as.numeric(quantile(x, 0.975))
  )))
  
  hyper_df <- data.frame(
    Hyperparameter = hyper_names,
    round(hyper_summary, 4),
    check.names = FALSE
  )
  
  write.csv(hyper_df, file_path, row.names = FALSE)
  message("Hyperparameters successfully exported to: ", file_path)
  return(invisible(hyper_df))
}









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
# Functions here are here to ease the way in which IRF are extracted. It
# was not strictly necssary but I found that not having the same plot repeated
# over again saves on line of code.


## (5) export_bvar_results_to_csv
# Purpose: This allows to export the IRF to CSV for analysis
# Note, I did not export the full posterior since it made
# a files in the GBs.

export_bvar_results_to_csv <- function(bvar_array, var_names, 
file_name, shock_names = var_names) {
  
  # 1. Automatically handle raw arrays or package list objects
  if (is.array(bvar_array)) {
    arr <- bvar_array
  } else if (!is.null(bvar_array$posterior$irf)) {
    arr <- bvar_array$posterior$irf
  } else if (!is.null(bvar_array$irf)) {
    arr <- bvar_array$irf
  } else {
    arr <- bvar_array[,,,]
  }
  
  dims <- dim(arr)
  N <- dims[1]
  H <- dims[3]
  
  summary_list <- list()
  idx <- 1
  
  # 2. Iterate over all variable-shock pairs and compute quantiles
  for (i in 1:N) {         # Response Variable
    for (j in 1:N) {       # Shock Source
      sub_mat <- arr[i, j, , ] # Matrix of size [Horizon x Draws]
      
      summary_list[[idx]] <- data.frame(
        Variable = var_names[i],
        Shock    = shock_names[j],
        Horizon  = 1:H,
        Mean     = apply(sub_mat, 1, mean, na.rm = TRUE),
        Median   = apply(sub_mat, 1, median, na.rm = TRUE),
        Lower_90 = apply(sub_mat, 1, quantile, probs = 0.05, na.rm = TRUE),
        Lower_68 = apply(sub_mat, 1, quantile, probs = 0.16, na.rm = TRUE),
        Upper_68 = apply(sub_mat, 1, quantile, probs = 0.84, na.rm = TRUE),
        Upper_90 = apply(sub_mat, 1, quantile, probs = 0.95, na.rm = TRUE)
      )
      idx <- idx + 1
    }
  }
  
  # 3. Combine list into a single Data Frame
  df_summary <- do.call(rbind, summary_list)
  
  # 4. Write to CSV file
  write.csv(df_summary, file = file_name, row.names = FALSE)
  message(paste("Successfully saved summary to:", file_name))
  
  return(df_summary)
}

#------------------------------------------------------------------------------#

## (6) get_irf_median_df
# Purpose: This function allows to easily extract median IRF for a specific 
# response variable & shock. Unfourtanely the bvarsign nor the BVAR
# allow to export a single row from the IRF and instead plot all 9X9.

get_irf_median_df <- function(irf_obj, 
                              response_var, 
                              shock_var, 
                              label_name = "Baseline", 
                              target_cols = NULL) {
  
  # 1. Extract raw 4D IRF array [N x N x Horizon x Draws]
  arr <- if (is.array(irf_obj)) {
    irf_obj
  } else if (!is.null(irf_obj$posterior$irf)) {
    irf_obj$posterior$irf
  } else if (!is.null(irf_obj$irf)) {
    irf_obj$irf
  } else {
    stop("Unrecognized IRF object structure. Pass output from compute_impulse_responses().")
  }
  
  # 2. Try extracting variable names from array dimnames if target_cols is NULL
  if (is.null(target_cols) && !is.null(dimnames(arr)[[1]])) {
    target_cols <- dimnames(arr)[[1]]
  }
  
  # 3. Resolve Response Index
  r_idx <- if (is.numeric(response_var)) {
    response_var
  } else if (!is.null(target_cols)) {
    which(target_cols == response_var)
  } else {
    stop("target_cols must be provided if response_var is passed as a string character.")
  }
  
  # 4. Resolve Shock Index
  s_idx <- if (is.numeric(shock_var)) {
    shock_var
  } else if (!is.null(target_cols)) {
    which(target_cols == shock_var)
  } else {
    stop("target_cols must be provided if shock_var is passed as a string character.")
  }
  
  if (length(r_idx) == 0 || r_idx > dim(arr)[1]) stop("Invalid response_var index/name.")
  if (length(s_idx) == 0 || s_idx > dim(arr)[2]) stop("Invalid shock_var index/name.")
  
  # 5. Extract subset slice [Horizon x Draws]
  sub_mat <- arr[r_idx, s_idx, , ]
  
  # Ensure 2D matrix structure if Horizon = 1
  if (is.null(dim(sub_mat))) {
    sub_mat <- matrix(sub_mat, nrow = 1)
  }
  
  # 6. Compute posterior medians and 68% credible intervals
  med   <- apply(sub_mat, 1, median, na.rm = TRUE)
  lower <- apply(sub_mat, 1, quantile, probs = 0.16, na.rm = TRUE)
  upper <- apply(sub_mat, 1, quantile, probs = 0.84, na.rm = TRUE)
  
  data.frame(
    Horizon       = 1:nrow(sub_mat),
    Median        = med,
    Lower_68      = lower,
    Upper_68      = upper,
    Specification = label_name,
    stringsAsFactors = FALSE
  )
}

#------------------------------------------------------------------------------#

## (7) plot_single_irf
# This is a function I decided to make in order to export a single IRF and not
# the entire 9x9 array. Of course, I later realized it was far less work to
# export the 9X1 (Variable * Shock) but having the ability the export a single
# IRF is still nice and will be used in the thesis to extract a particular
# interesting result.


plot_single_irf <- function(irf_df, 
                            response_name = "Variable", 
                            shock_name = "Structural Shock", 
                            output_path = NULL, 
                            width = 3200, 
                            height = 2600, 
                            res = 300) {
  
  library(ggplot2)
  
  # Build ggplot object
  p <- ggplot(irf_df, aes(x = Horizon, y = Median)) +
    geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "steelblue", alpha = 0.3) +
    geom_line(color = "darkblue", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
    labs(
      title    = paste("Response of", response_name),
      subtitle = paste("Shock:", shock_name),
      x        = "Horizon (Periods)",
      y        = "Percentage Points"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray30", size = 12),
      panel.grid.minor = element_blank()
    )
  
  # Export image if output_path is specified
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
    
    png(output_path, width = width, height = height, res = res)
    print(p) # Explicitly print ggplot to graphics device
    dev.off()
    
    message("IRF plot saved to: ", output_path)
  }
  
  return(p)
}

#------------------------------------------------------------------------------#
## (8) plot_single_shock_IRFS

plot_bvar_irf <- function(csv_path, 
                          shock_name = "policy_rate", 
                          target_cols = NULL, 
                          title = NULL,
                          subtitle = "68% and 90% Posterior Credible Intervals",
                          base_size = 11) {
  
  # 1. Load data
  irf_data <- read.csv(csv_path)
  
  # 2. Filter by shock
  policy_irf <- irf_data %>%
    filter(Shock == shock_name)
  
  if (nrow(policy_irf) == 0) {
    stop(paste0("No data found for Shock: '", shock_name, "' in ", csv_path))
  }
  
  # 3. Handle Variable factor ordering
  if (!is.null(target_cols)) {
    policy_irf <- policy_irf %>%
      mutate(Variable = factor(Variable, levels = target_cols))
  } else {
    policy_irf <- policy_irf %>%
      mutate(Variable = as.factor(Variable))
  }
  
  # 4. Generate Plot Title if not specified
  if (is.null(title)) {
    title <- paste("Impulse Response Functions:", shock_name, "Shock")
  }
  
  # 5. Build ggplot
  p <- ggplot(policy_irf, aes(x = Horizon)) +
    # 90% and 68% Credible Intervals
    geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "firebrick", alpha = 0.2) +
    geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "firebrick", alpha = 0.4) +
    # Zero reference line
    geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
    # Point estimates (Median and Mean)
    geom_line(aes(y = Median), color = "darkred", linewidth = 0.8) +
    geom_line(aes(y = Mean), color = "black", linetype = "dashed", linewidth = 0.5) +
    # Facet setup
    facet_wrap(~ Variable, ncol = 3, scales = "free_y") +
    scale_x_continuous(breaks = c(1, seq(4, max(policy_irf$Horizon, na.rm = TRUE), by = 4))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Horizon (Quarters)",
      y = "Response",
      caption = "Solid line: Median | Dashed line: Mean | Gray line: Zero response"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 9),
      panel.spacing = unit(1, "lines")
    )
  
  return(p)
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
























export_bvar_results_to_csv <- function(
    irf_array,
    var_names,
    file_name,
    shock_names = var_names
) {

  # Check dimensions
  if (!is.array(irf_array) || length(dim(irf_array)) != 4) {
    stop("irf_array must be a 4D array: [response x shock x horizon x draws].")
  }

  dims <- dim(irf_array)

  N <- dims[1]
  H <- dims[3]

  if (length(var_names) != N) {
    stop("Length of var_names must equal the number of response variables.")
  }

  if (length(shock_names) != dims[2]) {
    stop("Length of shock_names must equal the number of shocks.")
  }

  summary_list <- list()
  idx <- 1

  for (i in seq_len(N)) {

    for (j in seq_len(dims[2])) {

      sub_mat <- irf_array[i, j, , , drop = FALSE]

      sub_mat <- matrix(
        sub_mat,
        nrow = H,
        ncol = dims[4]
      )

      summary_list[[idx]] <- data.frame(

        Variable = var_names[i],

        Shock = shock_names[j],

        Horizon = 0:(H - 1),

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
        )
      )

      idx <- idx + 1
    }
  }

  df_summary <- do.call(
    rbind,
    summary_list
  )

  write.csv(
    df_summary,
    file = file_name,
    row.names = FALSE
  )

  message(
    "Successfully saved IRF summary to: ",
    file_name
  )

  return(df_summary)
}


























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
















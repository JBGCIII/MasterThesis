#####################################0. FUNCTIONS #######################################
##       

#Functions in order of appearance with purpuose.e
# (1) download_pxweb
# (2) get_riksbank_series
# (3) compute_fevd_from_irf
# (4)export_bvar_results_to_csv
# (5) get_irf_median_df

# ========================== 1a_Data_Set_Creation.R ====================================#

## (1) download_pxweb
# Purpose: SCB PXWEB downloader, helps to streamline the download from SCB APIs using
# the pwxweb query.
download_pxweb <- function(url, query){

  pxq <- pxweb_query(query) # Converts list into a valid PX-Web query object.
  pxweb_get_data( # Executes the HTTP call to SCB and retrieves the data
    url = url,  # API URL
    query = pxq,
    column.name.type = "text",
    variable.value.type = "text"
  )
}


## (2) get_riksbank_series
# Purpose: Helps to streamline the download from Riksbanken API
# originally I downloaded multiple items from Riksbanken (deposit_rate, lending_rate)
# so it was necessary then, but not strictly necessary now.

# Riksbank SWEA downloader
start_date <- "1995-01-01"
get_riksbank_series <- function(series_id,
                                from = "1995-01-01",  # Self-contained default
                                to = Sys.Date()) {

  url <- paste0(
    "https://api.riksbank.se/swea/v1/Observations/",
    series_id,
    "/",
    from,
    "/",
    to
  )

  response <- GET(url)

  stop_for_status(response)

  fromJSON(
    content(
      response,
      "text",
      encoding = "UTF-8"
    )
  ) |>
    as.data.frame()

}


# ========================== 2_BVARSIGN.R====================================#

#This is where functions were necessary. Unfortanately the BVARSIGN package
#encounters issues when dealing with large calculations and presents the 
#following message  "C:\Program Files\R\R-4.5.2\bin\R.exe '--no-save', 
#'--no-restore'" terminated with exit code: -1073741819. when using
# FEVD and historical decomposition.
# This is not surprising considering we are dealing with 4D objects that can
# reach in the millions of allocated values.
# E.G. 9 Rows * 9 Columns * 20 Time Periods * 30 000 Draws = ~19.5 Million.
# The IRF however still works, and the following allows to increase the draws
# while still being able to get the FEVD values.


## (3) compute_fevd_from_irf
# Purpose: Used instead of compute_variance_decompositions() as it crashed.
# The purpouse is to compute FEVD manually by auto extracting the
# arrays. Due to multiple FEVD being necessar, having the whole
# script will clutter the script so I decided to create
# a function.

compute_fevd_from_irf <- function(irf_obj) {
  
  # 1. Automatically extract the 4D numeric array
  if (is.array(irf_obj)) {
    irf_array <- irf_obj
  } else if (is.list(irf_obj) && !is.null(irf_obj$posterior$irf)) {
    irf_array <- irf_obj$posterior$irf
  } else if (is.list(irf_obj) && !is.null(irf_obj$irf)) {
    irf_array <- irf_obj$irf
  } else if (inherits(irf_obj, "PosteriorIRF")) {
    # If returned as S3 class with array inside
    irf_array <- irf_obj[,,,]
  } else {
    stop("Could not extract 4D array from irf_obj. Run str(irf_updated) to check its class.")
  }

  dims <- dim(irf_array)
  N <- dims[1]
  H <- dims[3]
  S <- dims[4]
  
  message(paste("Processing FEVD for N =", N, "variables over H =", H, "horizons across S =", S, "draws..."))

  # 2. Square responses (variance per shock per draw)
  irf_sq <- irf_array^2
  
  # 3. Compute cumulative sum over horizons
  fevd <- array(0, dim = c(N, N, H, S))
  
  for (s in 1:S) {
    for (i in 1:N) {         # Variable
      for (j in 1:N) {       # Shock
        fevd[i, j, , s] <- cumsum(irf_sq[i, j, , s])
      }
      
      # 4. Normalize across all shocks at each horizon step
      for (h in 1:H) {
        total_var <- sum(fevd[i, , h, s])
        if (!is.na(total_var) && total_var > 0) {
          fevd[i, , h, s] <- fevd[i, , h, s] / total_var
        }
      }
    }
  }
  
  dimnames(fevd) <- dimnames(irf_array)
  return(fevd)
}



## (4) export_bvar_results_to_csv
# Purpose: This allows to export the IRF & FEVD to CSV for analysis
# Note, I did not export the full posterior since it made
# a files in the GBs.

export_bvar_results_to_csv <- function(bvar_array, var_names, file_name, shock_names = var_names) {
  
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


## (5) get_irf_median_df
# Purpose: This function allows to easily extract median IRF for a specific 
# response variable & shock. Unfourtanely the bvarsign nor the BVAR
# allow to export a single row from the IRF and instead plot all 9X9.

get_irf_median_df <- function(irf_obj, response_var, shock_var, label_name, target_cols) {
  arr <- if (is.array(irf_obj)) irf_obj else irf_obj$posterior$irf
  
  r_idx <- which(target_cols == response_var)
  s_idx <- which(target_cols == shock_var)
  
  if (length(r_idx) == 0 || length(s_idx) == 0) {
    stop("Variable or shock name not found in target_cols.")
  }
  
  sub_mat <- arr[r_idx, s_idx, , ]
  med <- apply(sub_mat, 1, median, na.rm = TRUE)
  
  data.frame(
    Horizon = 1:dim(arr)[3],
    Median = med,
    Specification = label_name
  )
}





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








check_mcmc_convergence <- function(estimation_obj, 
                                    var_idx, 
                                    lag_idx = var_idx, 
                                    var_names = NULL, 
                                    output_path = "Output/MCMC_Diagnostics.png") {
  
  # Ensure target output directory exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  # Extract autoregressive parameter matrix B [N, N*p + 1, Draws]
  b_draws <- estimation_obj$posterior$B
  
  # Extract chain for selected parameter
  param_chain <- b_draws[var_idx, lag_idx, ]
  
  # Resolve variable labels for plot headers
  dep_name <- if (!is.null(var_names)) var_names[var_idx] else paste("Var", var_idx)
  lag_name <- if (!is.null(var_names)) var_names[lag_idx] else paste("Var", lag_idx)
  param_title <- paste0("Equation: ", dep_name, " | Lag: ", lag_name)
  
  # ----------------------------------------------------------------------------
  # 1. Export Diagnostic Plot Matrix (Trace + ACF)
  # ----------------------------------------------------------------------------
  png(output_path, width = 3200, height = 2600, res = 300)
  on.exit(dev.off(), add = TRUE) # Ensures graphics device closes safely on error
  
  par(mfrow = c(2, 1), mar = c(4.5, 4.5, 3, 1), family = "sans")
  
  # Trace Plot
  plot(param_chain, type = "l", col = "#1F4E78", lwd = 1.2,
       main = paste("MCMC Trace Plot -", param_title),
       xlab = "MCMC Iteration", ylab = "Draw Value", grid())
  abline(h = mean(param_chain), col = "firebrick", lty = 2, lwd = 1.5)
  
  # ACF Plot
  acf(param_chain, main = paste("Autocorrelation Function (ACF) -", param_title),
      col = "#1F4E78", lwd = 2)
  
  par(mfrow = c(1, 1)) # Reset grid
  
  # ----------------------------------------------------------------------------
  # 2. Compute Convergence Metrics via coda
  # ----------------------------------------------------------------------------
  mcmc_chain <- coda::mcmc(param_chain)
  
  ess_val    <- coda::effectiveSize(mcmc_chain)
  geweke_obj <- coda::geweke.diag(mcmc_chain)
  geweke_z   <- as.numeric(geweke_obj$z)
  
  # Evaluate convergence threshold (|Z| < 1.96)
  convergence_pass <- abs(geweke_z) < 1.96
  
  metrics_df <- data.frame(
    Parameter     = param_title,
    ESS           = round(as.numeric(ess_val), 2),
    Geweke_Zscore = round(geweke_z, 4),
    Converged_5pct = convergence_pass
  )
  
  message("Diagnostics exported to: ", output_path)
  return(metrics_df)
}











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





















compute_fevd_from_irf2 <- function(irf_obj) {
  
  # 1. Extract 4D numeric array [Variable, Shock, Horizon, Draw]
  if (is.array(irf_obj)) {
    irf_array <- irf_obj
  } else if (is.list(irf_obj) && !is.null(irf_obj$posterior$irf)) {
    irf_array <- irf_obj$posterior$irf
  } else if (is.list(irf_obj) && !is.null(irf_obj$irf)) {
    irf_array <- irf_obj$irf
  } else if (inherits(irf_obj, "PosteriorIRF")) {
    irf_array <- irf_obj[,,,]
  } else {
    stop("Could not extract 4D array from irf_obj.")
  }

  dims <- dim(irf_array)
  N <- dims[1]
  H <- dims[3]
  S <- dims[4]
  
  message(paste("Processing FEVD for N =", N, "variables over H =", H, "horizons across S =", S, "draws..."))

  # 2. Square impulse responses (squared structural impacts)
  irf_sq <- irf_array^2
  
  # 3. Compute cumulative variance across horizons (dim 3)
  # Apply cumulative sum along the horizon dimension for each variable, shock, and draw
  fevd_cum <- apply(irf_sq, c(1, 2, 4), cumsum)
  
  # Re-order dimensions back to [Variable, Shock, Horizon, Draw]
  fevd_cum <- aperm(fevd_cum, c(2, 3, 1, 4))

  # 4. Normalize by total variance across all shocks (sum over dim 2)
  fevd <- array(0, dim = c(N, N, H, S))
  
  for (h in 1:H) {
    # Total variance for each variable per draw at horizon h
    total_var <- apply(fevd_cum[,, h, , drop = FALSE], c(1, 4), sum)
    
    for (j in 1:N) { # Loop over shocks to normalize
      fevd[, j, h, ] <- fevd_cum[, j, h, ] / total_var
    }
  }
  
  dimnames(fevd) <- dimnames(irf_array)
  return(fevd)
}
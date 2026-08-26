###############################################################################
################################# 0.FUNCTIONS #################################
###############################################################################

#Functions in order of appearance with purpuose.e
# (1) download_pxweb
# (2) get_riksbank_series
#=========================#
# (3) export_hyperparameters
# (4) compute_and_export_ess
#=========================#
# (5) compute_fevd_from_irf [Very Important]
# (6) export_bvar_results_to_csv
# (7) get_irf_median_df
# (8) plot_single_irf

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


#------------------------------------------------------------------------------#

## (4) compute_and_export_ess
# Purpose: This function allows to easily extract effective sample size from
# a model and export it as CSV. Gives you insight right away. That way you
# don't need to find it yourself while looking trough the code.

compute_and_export_ess <- function(model_obj, 
                                   lambda_idx = 10, 
                                   alpha_idx = 11, 
                                   file_path = NULL) {
  
  # 1. Safely extract hyperparameter posterior chains
  if (is.null(model_obj$posterior$hyper)) {
    stop("The model object does not contain 'posterior$hyper'. Check the object structure.")
  }
  
  posterior_lambda <- model_obj$posterior$hyper[lambda_idx, ]
  posterior_alpha  <- model_obj$posterior$hyper[alpha_idx, ]
  
  # 2. Compute ESS using coda
  ess_lambda <- coda::effectiveSize(coda::as.mcmc(posterior_lambda))
  ess_alpha  <- coda::effectiveSize(coda::as.mcmc(posterior_alpha))
  
  # 3. Create summary data frame
  ess_summary <- data.frame(
    Parameter = c("Lambda", "Alpha"),
    ESS       = c(round(as.numeric(ess_lambda), 1), 
                  round(as.numeric(ess_alpha), 1))
  )
  
  # 4. Optional CSV export
  if (!is.null(file_path)) {
    # Automatically create output folder structure if it doesn't exist
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    write.csv(ess_summary, file = file_path, row.names = FALSE)
    message(paste("ESS diagnostics saved to:", file_path))
  }
  
  return(ess_summary)
}


#============================================================================#
#                     [3] FUNCTIONS USED IN BVARSIGN
#============================================================================#

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

#------------------------------------------------------------------------------#

## (5) compute_fevd_from_irf
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
  
  message(paste("Processing FEVD for N =", N, "variables over H =", 
  H, "horizons across S =", S, "draws..."))

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

#------------------------------------------------------------------------------#

## (6) export_bvar_results_to_csv
# Purpose: This allows to export the IRF & FEVD to CSV for analysis
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


#------------------------------------------------------------------------------#

## (6) export_bvar_results_to_csv
# Purpose: Allows to compute the historical decomposition since the BVARSING
# command creashes.
compute_hd_batched <- function(posterior_obj, horizon = NULL, batch_size = 1000) {
  
  if (is.null(horizon)) horizon <- nrow(posterior_obj$specify$data)

  # Fetch total draws available
  irf_all   <- compute_impulse_responses(posterior_obj, horizon = horizon)
  shock_all <- compute_structural_shocks(posterior_obj)
  
  N     <- dim(irf_all)[1]
  T_obs <- dim(shock_all)[2]
  S     <- dim(irf_all)[4]
  
  num_batches <- ceiling(S / batch_size)
  cat(sprintf("Processing %d draws across %d batches...\n", S, num_batches))
  
  # Allocate memory ONLY for summary statistics (negligible memory footprint)
  hd_mean <- array(0, dim = c(N, N, T_obs))
  
  # Accumulators for quantile estimates across batches
  for (b in 1:num_batches) {
    idx_start <- (b - 1) * batch_size + 1
    idx_end   <- min(b * batch_size, S)
    batch_indices <- idx_start:idx_end
    b_size <- length(batch_indices)
    
    # Pre-allocate array ONLY for current batch
    hd_batch <- array(0, dim = c(N, N, T_obs, b_size))
    
    for (s_idx in 1:b_size) {
      s <- batch_indices[s_idx]
      for (t in 1:T_obs) {
        for (j in 1:t) {
          irf_mat    <- irf_all[, , j, s]
          shock_diag <- diag(shock_all[, t - j + 1, s])
          hd_batch[, , t, s_idx] <- hd_batch[, , t, s_idx] + (irf_mat %*% shock_diag)
        }
      }
    }
    
    
    # Accumulate running mean
    hd_mean <- hd_mean + apply(hd_batch, c(1, 2, 3), sum)
    
    # Clean up RAM immediately
    rm(hd_batch)
    gc(verbose = FALSE)
    cat(sprintf("Batch %d/%d completed.\n", b, num_batches))
  }
  
  # Final scaling
  hd_mean <- hd_mean / S
  
  var_names <- colnames(posterior_obj$specify$data)
  dimnames(hd_mean) <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  
  return(hd_mean)
}

#------------------------------------------------------------------------------#

## (8) export_bvar_results_to_csv
# Purpose: Allows to save the historical function.

save_hd_batched_to_csv <- function(hd_matrix, file_path = "hd_summary_results.csv", dates = NULL) {
  # Get dimensions for 3D array [N_vars x N_shocks x T_obs]
  N_vars   <- dim(hd_matrix)[1]
  N_shocks <- dim(hd_matrix)[2]
  T_obs    <- dim(hd_matrix)[3]
  
  var_names   <- dimnames(hd_matrix)$Variable
  shock_names <- dimnames(hd_matrix)$Shock
  
  if (is.null(var_names))   var_names   <- paste0("Var_", 1:N_vars)
  if (is.null(shock_names)) shock_names <- paste0("Shock_", 1:N_shocks)
  if (is.null(dates))       dates       <- 1:T_obs
  
  export_list <- list()
  counter <- 1
  
  for (v in 1:N_vars) {
    for (t in 1:T_obs) {
      row_data <- data.frame(
        date            = dates[t],
        target_variable = var_names[v]
      )
      
      # Add column for each shock contribution
      for (k in 1:N_shocks) {
        s_name <- shock_names[k]
        row_data[[paste0("shock_", s_name, "_mean")]] <- hd_matrix[v, k, t]
      }
      
      # Add overall decomposed total
      row_data$total_decomposed_mean <- sum(hd_matrix[v, , t])
      
      export_list[[counter]] <- row_data
      counter <- counter + 1
    }
  }
  
  final_df <- do.call(rbind, export_list)
  write.csv(final_df, file = file_path, row.names = FALSE)
  
  message("Successfully exported HD summary results to ", file_path)
  return(final_df)
}


#============================================================================#
#                     [4] FUNCTIONS FOR IRF
#============================================================================#
# Functions here are here to ease the way in which IRF are extracted. It
# was not strictly necssary but I found that not having the same plot repeated
# over again saves on line of code.

#------------------------------------------------------------------------------#

## (7) get_irf_median_df
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

## (8) plot_single_irf
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

























compute_hd_batched2 <- function(posterior_obj, horizon = NULL,
                                batch_size = 1000,
                                tmp_dir = "hd_batches_tmp") {
 
  if (is.null(horizon)) horizon <- nrow(posterior_obj$specify$data)
 
  irf_all   <- compute_impulse_responses(posterior_obj, horizon = horizon)
  shock_all <- compute_structural_shocks(posterior_obj)
 
  N     <- dim(irf_all)[1]
  T_obs <- dim(shock_all)[2]
  S     <- dim(irf_all)[4]
 
  num_batches <- ceiling(S / batch_size)
  cat(sprintf("Processing %d draws across %d batches...\n", S, num_batches))
 
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir)
 
  hd_mean <- array(0, dim = c(N, N, T_obs))
 
  for (b in 1:num_batches) {
    idx_start <- (b - 1) * batch_size + 1
    idx_end   <- min(b * batch_size, S)
    batch_indices <- idx_start:idx_end
    b_size <- length(batch_indices)
 
    hd_batch <- array(0, dim = c(N, N, T_obs, b_size))
 
    for (s_idx in 1:b_size) {
      s <- batch_indices[s_idx]
      for (t in 1:T_obs) {
        for (j in 1:t) {
          irf_mat    <- irf_all[, , j, s]
          shock_diag <- diag(shock_all[, t - j + 1, s])
          hd_batch[, , t, s_idx] <- hd_batch[, , t, s_idx] + (irf_mat %*% shock_diag)
        }
      }
    }
 
    # Accumulate running sum for the mean (cheap, in-memory)
    hd_mean <- hd_mean + apply(hd_batch, c(1, 2, 3), sum)
 
    # Save the batch itself to disk so quantiles can be computed
    # after all batches are done, without holding everything in RAM
    saveRDS(hd_batch, file = file.path(tmp_dir, sprintf("hd_batch_%03d.rds", b)))
 
    rm(hd_batch)
    gc(verbose = FALSE)
    cat(sprintf("Batch %d/%d completed.\n", b, num_batches))
  }
 
  hd_mean <- hd_mean / S
 
  # ---- Reload batches to compute quantiles ----
  cat("Reloading batches to compute quantiles...\n")
  batch_files <- file.path(tmp_dir, sprintf("hd_batch_%03d.rds", 1:num_batches))
  hd_all <- do.call(abind::abind, c(lapply(batch_files, readRDS), along = 4))
 
  hd_median <- apply(hd_all, c(1, 2, 3), median)
  hd_lo90   <- apply(hd_all, c(1, 2, 3), quantile, probs = 0.05)
  hd_hi90   <- apply(hd_all, c(1, 2, 3), quantile, probs = 0.95)
  hd_lo68   <- apply(hd_all, c(1, 2, 3), quantile, probs = 0.16)
  hd_hi68   <- apply(hd_all, c(1, 2, 3), quantile, probs = 0.84)
 
  rm(hd_all)
  gc(verbose = FALSE)
  file.remove(batch_files)
  unlink(tmp_dir, recursive = TRUE)
 
  var_names <- colnames(posterior_obj$specify$data)
  for (arr in list(hd_mean, hd_median, hd_lo90, hd_hi90, hd_lo68, hd_hi68)) {
    dimnames(arr) <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  }
  dimnames(hd_mean)   <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  dimnames(hd_median) <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  dimnames(hd_lo90)   <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  dimnames(hd_hi90)   <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  dimnames(hd_lo68)   <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
  dimnames(hd_hi68)   <- list(Variable = var_names, Shock = var_names, Time = 1:T_obs)
 
  return(list(
    mean   = hd_mean,
    median = hd_median,
    lo90   = hd_lo90,
    hi90   = hd_hi90,
    lo68   = hd_lo68,
    hi68   = hd_hi68
  ))
}
 
 
## save_hd_batched_to_csv (updated)
# Purpose: Exports mean + median + 90%/68% credible bands for the
# historical decomposition to a single CSV, one row per
# (target_variable, date), with columns per shock per statistic.
 
save_hd_batched_to_csv <- function(hd_result, file_path = "hd_summary_results.csv",
                                    dates = NULL) {
 
  hd_mean   <- hd_result$mean
  hd_median <- hd_result$median
  hd_lo90   <- hd_result$lo90
  hd_hi90   <- hd_result$hi90
  hd_lo68   <- hd_result$lo68
  hd_hi68   <- hd_result$hi68
 
  N_vars   <- dim(hd_mean)[1]
  N_shocks <- dim(hd_mean)[2]
  T_obs    <- dim(hd_mean)[3]
 
  var_names   <- dimnames(hd_mean)$Variable
  shock_names <- dimnames(hd_mean)$Shock
 
  if (is.null(var_names))   var_names   <- paste0("Var_", 1:N_vars)
  if (is.null(shock_names)) shock_names <- paste0("Shock_", 1:N_shocks)
  if (is.null(dates))       dates       <- 1:T_obs
 
  export_list <- list()
  counter <- 1
 
  for (v in 1:N_vars) {
    for (t in 1:T_obs) {
      row_data <- data.frame(
        date            = dates[t],
        target_variable = var_names[v]
      )
 
      for (k in 1:N_shocks) {
        s_name <- shock_names[k]
        row_data[[paste0("shock_", s_name, "_mean")]]   <- hd_mean[v, k, t]
        row_data[[paste0("shock_", s_name, "_median")]] <- hd_median[v, k, t]
        row_data[[paste0("shock_", s_name, "_lo90")]]   <- hd_lo90[v, k, t]
        row_data[[paste0("shock_", s_name, "_hi90")]]   <- hd_hi90[v, k, t]
        row_data[[paste0("shock_", s_name, "_lo68")]]   <- hd_lo68[v, k, t]
        row_data[[paste0("shock_", s_name, "_hi68")]]   <- hd_hi68[v, k, t]
      }
 
      row_data$total_decomposed_mean   <- sum(hd_mean[v, , t])
      row_data$total_decomposed_median <- sum(hd_median[v, , t])
 
      export_list[[counter]] <- row_data
      counter <- counter + 1
    }
  }
 
  final_df <- do.call(rbind, export_list)
  write.csv(final_df, file = file_path, row.names = FALSE)
 
  message("Successfully exported HD summary results to ", file_path)
  return(final_df)
}
 





















library(tidyverse)

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

###############################################################################
################################### FEVD FUNCTION #############################
###############################################################################
## Purpose:
# Computes posterior forecast error variance decompositions from standardised
# posterior impulse responses.

## Reason
# Bsvars and Bsvarsign FEVD function crashes as
# "C:\Program Files\R\R-4.5.2\bin\R.exe '--no-save',--no-restore'" terminated
# with exit code: -1073741819, above a certain treshold.
# This appears to be a known issues for the package
#(https://github.com/bsvars/bsvarSIGNs/issues/47), even if said to be
# resolved, it still occurs in the new versions.

# The following things were done before creating a function.
# 1) Ensuring no NAs were present in the data set.
# 2) Deleting the Bsvars and Bsvarsign packages and installing directly
# from GitHub.¨
# 3) Run the program natively on R and Rstudio instead of visual studio
# code, and it still crashes.
# 4) Lowering the ammount of horizon and draws.
#------------------------------------------------------------------------------#
# a. Draws = 1000, 10 Horizon (Works)
# b. Draws = 1000, 20 Horizon (Works)
# c. Draws = 1000, 30 Horizon (Works)
#------------------------------------------------------------------------------#
# At this level of draws, the model diagnositc show poor diagnostics
# in terms of Alpha and lambda traces.
# a. Draws = 4000, 10 Horizon (Works)
# b. Draws = 4000, 20 Horizon (Works)
# c. Draws = 4000, 30 Horizon (Crashes)
#------------------------------------------------------------------------------#
# At this level of draws, the model diagnositc show better but still
# somewhat sluggish mixing and  stair-step pattern,
# a. Draws = 10000, 10 Horizon (Works)
# b. Draws = 10000, 15 Horizon (Works)
# c. Draws = 10000, 30 Horizon (Crashes)
#------------------------------------------------------------------------------#
# At this level of draws, the model diagnostics show strongs and stable
# diagnostic with a viable effective sample size, however it crasehs even at
# 10 horizons.

# After further diagnosis the problem appears to be within either the package.
# Or RAM usage. You might have access to a supercomputer or not. A program
# should run no matter the machine.
# [P.S. One of the great things I learned troug this is that you can save the
# model and don't need to re-run it. Which is great.]


compute_fevd_bsvarSIGN_safe <- function(irf_obj) {

  # 1. Extract the IRF array
  if (is.array(irf_obj)) {
    irf_array <- irf_obj
  } else if (inherits(irf_obj, "PosteriorIR")) {
    irf_array <- irf_obj[, , , ]
  } else if (!is.null(irf_obj$posterior$irf)) {
    irf_array <- irf_obj$posterior$irf
  } else if (!is.null(irf_obj$irf)) {
    irf_array <- irf_obj$irf
  } else {
    stop("Could not extract a 4D IRF array from irf_obj.")
  }

  # 2. Check dimensions
  dims <- dim(irf_array)
  if (length(dims) != 4) {
    stop(
      "IRF object must have four dimensions: ",
      "Variable * Shock * Horizon * Draw."
    )
  }
  N <- dims[1]
  H <- dims[3]
  S <- dims[4]

  # 3. Informative message
  message(
    "Computing FEVD: ",
    N, " variables * ",
    H, " horizons * ",
    S, " posterior draws."
  )

  # 4. Allocate output
  fevd <- array(
    NA_real_,
    dim = c(N, N, H, S),
    dimnames = dimnames(irf_array)
  )

  # 5. Compute FEVD draw by draw
  for (s in seq_len(S)) {
    # Extract one posterior draw
    irf_s <- irf_array[, , , s]
    # Square impulse responses
    sq <- irf_s^2
    # Cumulative squared impulse responses
    cum_sq <- array(
      0,
      dim = c(N, N, H)
    )
    cum_sq[, , 1] <- sq[, , 1]
    if (H > 1) {
      for (h in 2:H) {
        cum_sq[, , h] <-
          cum_sq[, , h - 1] + sq[, , h]
      }
    }

    # 6. Normalize each horizon
    for (h in seq_len(H)) {
      total_var <- rowSums(
        cum_sq[, , h]
      )

      fevd[, , h, s] <-
        sweep(
          cum_sq[, , h],
          1,
          total_var,
          "/"
        ) * 100  #FEVDs are reported as percentages, consistent with the
                 # bsvarSIGNs output.
    }

    # 7. Progress message
    if (s %% 500 == 0 || s == S) {
      message(
        "Completed ",
        s,
        " / ",
        S,
        " draws"
      )

    }
  }

  # 8. Give output the same class as package FEVD
  class(fevd) <- "PosteriorFEVD"
  return(fevd)
}

## Important:
# bsvarSIGNs::compute_variance_decompositions() internally
# uses standardise = TRUE and returns FEVDs in percentages
# (0-100).
#
## Validation:
# The implementation was validated against the native
# bsvarSIGNs FEVD routine using the same posterior draws.
# For 1,000 draws and horizon 10, the maximum absolute
# difference between the package result and this implementation
# was 5.68e-14.


# ============================================================
# FEVD PLOT FUNCTION
# ============================================================

plot_fevd <- function(
    fevd,
    response,
    shock,
    var_names,
    shock_names = var_names,
    horizon_start = 0,
    horizon_end = NULL
) {
  # 1.Check dimensions
  dims <- dim(fevd)
  if (length(dims) != 4) {
    stop("FEVD object must be a 4D array.")
  }
  N <- dims[1]
  H <- dims[3]
  S <- dims[4]
  if (length(var_names) != N) {
    stop("Length of var_names does not match number of variables.")
  }
  if (length(shock_names) != N) {
    stop("Length of shock_names does not match number of shocks.")
  }
  if (response < 1 || response > N) {
    stop("Invalid response variable index.")
  }
  if (shock < 1 || shock > N) {
    stop("Invalid shock index.")
  }
  # 2.Horizon handling
  horizons <- horizon_start:(H - 1)
  if (is.null(horizon_end)) {
    horizon_end <- H - 1
  }
  keep <- horizons >= horizon_start &
          horizons <= horizon_end

  horizons <- horizons[keep]

  # 3.Extract posterior draws
  draws <- fevd[
    response,
    shock,
    keep,
    ,
    drop = FALSE
  ]

  draws <- matrix(
    draws,
    nrow = length(horizons),
    ncol = S
  )

  # 4.Posterior summaries
  median_fevd <- apply(
    draws,
    1,
    median,
    na.rm = TRUE
  )

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

  lower_90 <- apply(
    draws,
    1,
    quantile,
    probs = 0.05,
    na.rm = TRUE
  )

  upper_90 <- apply(
    draws,
    1,
    quantile,
    probs = 0.95,
    na.rm = TRUE
  )

  # 5.Plot data

  plot_df <- data.frame(
    Horizon = horizons,
    Median = median_fevd,
    Lower_68 = lower_68,
    Upper_68 = upper_68,
    Lower_90 = lower_90,
    Upper_90 = upper_90
  )

  # 6. Plot
  p <- ggplot(
    plot_df,
    aes(x = Horizon, y = Median)
  ) +

    geom_ribbon(
      aes(
        ymin = Lower_90,
        ymax = Upper_90
      ),
      alpha = 0.15
    ) +

    geom_ribbon(
      aes(
        ymin = Lower_68,
        ymax = Upper_68
      ),
      alpha = 0.25
    ) +

    geom_line(
      linewidth = 0.8
    ) +

    labs(
      title = paste0(
        "FEVD: ",
        var_names[response],
        " — ",
        shock_names[shock],
        " shock"
      ),
      x = "Horizon",
      y = "Variance contribution (%)"
    ) +

    scale_x_continuous(
      breaks = pretty(horizons)
    ) +

    theme_minimal(base_size = 12)

  return(p)
}











plot_fevd_one_shock <- function(
    fevd,
    shock,
    var_names,
    shock_names = var_names,
    horizon_start = 0,
    horizon_end = NULL
) {

  # ----------------------------------------------------------
  # Check dimensions
  # ----------------------------------------------------------

  dims <- dim(fevd)

  if (length(dims) != 4) {
    stop("FEVD object must be a 4D array.")
  }

  N <- dims[1]
  H <- dims[3]
  S <- dims[4]

  if (length(var_names) != N) {
    stop("Length of var_names does not match FEVD.")
  }

  if (length(shock_names) != N) {
    stop("Length of shock_names does not match FEVD.")
  }

  if (shock < 1 || shock > N) {
    stop("Invalid shock index.")
  }

  # ----------------------------------------------------------
  # Horizons
  # ----------------------------------------------------------

  if (is.null(horizon_end)) {
    horizon_end <- H - 1
  }

  horizons <- horizon_start:horizon_end

  keep <- horizons + 1 <= H

  horizons <- horizons[keep]


  # ----------------------------------------------------------
  # Build plotting data
  # ----------------------------------------------------------

  plot_list <- vector("list", N)

  for (i in seq_len(N)) {

    draws <- fevd[
      i,
      shock,
      horizons + 1,
      ,
      drop = FALSE
    ]

    draws <- matrix(
      draws,
      nrow = length(horizons),
      ncol = S
    )

    plot_list[[i]] <- data.frame(
      Variable = var_names[i],
      Horizon = horizons,

      Median = apply(
        draws,
        1,
        median,
        na.rm = TRUE
      ),

      Lower_68 = apply(
        draws,
        1,
        quantile,
        probs = 0.16,
        na.rm = TRUE
      ),

      Upper_68 = apply(
        draws,
        1,
        quantile,
        probs = 0.84,
        na.rm = TRUE
      ),

      Lower_90 = apply(
        draws,
        1,
        quantile,
        probs = 0.05,
        na.rm = TRUE
      ),

      Upper_90 = apply(
        draws,
        1,
        quantile,
        probs = 0.95,
        na.rm = TRUE
      )
    )
  }

  plot_df <- dplyr::bind_rows(plot_list)


  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------

  ggplot(
    plot_df,
    aes(
      x = Horizon,
      y = Median
    )
  ) +

    geom_ribbon(
      aes(
        ymin = Lower_90,
        ymax = Upper_90
      ),
      alpha = 0.15
    ) +

    geom_ribbon(
      aes(
        ymin = Lower_68,
        ymax = Upper_68
      ),
      alpha = 0.25
    ) +

    geom_line(
      linewidth = 0.7
    ) +

    facet_wrap(
      ~ Variable,
      ncol = 3,
      scales = "free_y"
    ) +

    labs(
      title = paste0(
        "FEVD of a ",
        shock_names[shock],
        " shock"
      ),
      subtitle = paste0(
        "Posterior median with 68% and 90% credible intervals"
      ),
      x = "Horizon",
      y = "Variance contribution (%)"
    ) +

    theme_minimal(
      base_size = 11
    ) +

    theme(
      strip.text = element_text(
        face = "bold"
      ),
      plot.title = element_text(
        face = "bold"
      )
    )
}
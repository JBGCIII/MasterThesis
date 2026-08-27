###############################################################################
############################# HISTORICAL DECOMPOSITION#########################
###############################################################################

# ============================================================
# HISTORICAL DECOMPOSITION FOR bsvarSIGNs
# ============================================================
#
# Computes posterior historical decompositions while avoiding
# the native bsvarSIGNs FEVD/HD wrapper that may crash R on
# Windows with large posterior samples.
#
# Structural shocks and IRFs are calculated using the
# bsvarSIGNs package methods.
#
# Output:
#   Variable x Shock x Time x Posterior draw
#
# ============================================================

compute_hd_bsvarSIGN_safe <- function(
    posterior,
    show_progress = TRUE
) {

  # ----------------------------------------------------------
  # 1. Basic information
  # ----------------------------------------------------------

  posterior_Theta0 <- posterior$posterior$Theta0
  posterior_A <- posterior$posterior$A

  posterior_At <- aperm(
    posterior_A,
    c(2, 1, 3)
  )

  T <- ncol(
    posterior$last_draw$data_matrices$Y
  )

  p <- posterior$last_draw$p

  N <- dim(posterior_A)[1]
  S <- dim(posterior_A)[3]


  # ----------------------------------------------------------
  # 2. Determine standardisation
  # ----------------------------------------------------------

  standardise <- TRUE

  sign_irf <-
    posterior$last_draw$identification$sign_irf

  if (
    any(diag(sign_irf[, , 1]) == 0) &&
    !is.na(
      any(diag(sign_irf[, , 1]) == 0)
    )
  ) {

    standardise <- FALSE

    message(
      "standardise set to FALSE because ",
      "zero restrictions are present on the diagonal."
    )
  }


  # ----------------------------------------------------------
  # 3. Structural shocks
  #
  # Use the package's own public method.
  # ----------------------------------------------------------

  message(
    "Computing structural shocks..."
  )

  shocks <- compute_structural_shocks(
    posterior
  )


  # ----------------------------------------------------------
  # 4. Standardised impulse responses
  #
  # Use the package's public method.
  # ----------------------------------------------------------

  message(
    "Computing impulse responses..."
  )

  ir <- compute_impulse_responses(
    posterior,
    horizon = T - 1,
    standardise = standardise
  )


  # ----------------------------------------------------------
  # 5. Extract arrays
  # ----------------------------------------------------------

  shocks_array <- as.array(
    shocks
  )

  ir_array <- as.array(
    ir
  )


  # ----------------------------------------------------------
  # 6. Check dimensions
  # ----------------------------------------------------------

  if (
    !identical(
      dim(shocks_array),
      c(N, T, S)
    )
  ) {

    stop(
      "Unexpected structural shock dimensions: ",
      paste(dim(shocks_array), collapse = " x ")
    )
  }

  if (
    !identical(
      dim(ir_array),
      c(N, N, T, S)
    )
  ) {

    stop(
      "Unexpected IRF dimensions: ",
      paste(dim(ir_array), collapse = " x ")
    )
  }


  # ----------------------------------------------------------
  # 7. Allocate HD
  # ----------------------------------------------------------

  hd <- array(
    0,
    dim = c(N, N, T, S)
  )


  # ----------------------------------------------------------
  # 8. Historical decomposition
  #
  # For each draw:
  #
  #   HD[i,j,t] =
  #       sum_h IRF[i,j,h] *
  #               shock[j,t-h+1]
  #
  # ----------------------------------------------------------

  for (s in seq_len(S)) {

    ir_s <- ir_array[, , , s]

    shocks_s <- shocks_array[, , s]


    for (t in seq_len(T)) {

      max_h <- min(t, T)

      for (h in seq_len(max_h)) {

        shock_time <- t - h + 1

        hd[, , t, s] <-
          hd[, , t, s] +
          ir_s[, , h] %*%
          diag(
            shocks_s[, shock_time],
            nrow = N
          )
      }
    }


    # Progress
    if (
      show_progress &&
      (
        s %% 500 == 0 ||
        s == S
      )
    ) {

      message(
        "Completed ",
        s,
        " / ",
        S,
        " draws"
      )
    }
  }


  # ----------------------------------------------------------
  # 9. Class
  # ----------------------------------------------------------

  class(hd) <- "PosteriorHD"

  return(hd)
}




compute_hd_bsvarSIGN_safe_2 <- function(
    posterior,
    show_progress = TRUE
) {

  # ----------------------------------------------------------
  # 1. Basic information
  # ----------------------------------------------------------

  posterior_Theta0 <- posterior$posterior$Theta0
  posterior_A <- posterior$posterior$A

  posterior_At <- aperm(
    posterior_A,
    c(2, 1, 3)
  )

  T <- ncol(
    posterior$last_draw$data_matrices$Y
  )

  p <- posterior$last_draw$p

  N <- dim(posterior_A)[1]
  S <- dim(posterior_A)[3]


  # ----------------------------------------------------------
  # 2. Determine standardisation
  # ----------------------------------------------------------

  standardise <- TRUE

  sign_irf <-
    posterior$last_draw$identification$sign_irf

  if (
    any(diag(sign_irf[, , 1]) == 0) &&
    !is.na(
      any(diag(sign_irf[, , 1]) == 0)
    )
  ) {

    standardise <- FALSE

    message(
      "standardise set to FALSE because ",
      "zero restrictions are present on the diagonal."
    )
  }


  # ----------------------------------------------------------
  # 3. Structural shocks
  #
  # Use the package's own public method.
  # ----------------------------------------------------------

  message(
    "Computing structural shocks..."
  )

  shocks <- compute_structural_shocks(
    posterior
  )


  # ----------------------------------------------------------
  # 4. Standardised impulse responses
  #
  # Use the package's public method.
  # ----------------------------------------------------------

  message(
    "Computing impulse responses..."
  )

  ir <- compute_impulse_responses(
    posterior,
    horizon = T - 1,
    standardise = standardise
  )


  # ----------------------------------------------------------
  # 5. Extract arrays
  # ----------------------------------------------------------

  shocks_array <- as.array(
    shocks
  )

  ir_array <- as.array(
    ir
  )


  # ----------------------------------------------------------
  # 6. Check dimensions
  # ----------------------------------------------------------

  if (
    !identical(
      dim(shocks_array),
      c(N, T, S)
    )
  ) {

    stop(
      "Unexpected structural shock dimensions: ",
      paste(dim(shocks_array), collapse = " x ")
    )
  }

  if (
    !identical(
      dim(ir_array),
      c(N, N, T, S)
    )
  ) {

    stop(
      "Unexpected IRF dimensions: ",
      paste(dim(ir_array), collapse = " x ")
    )
  }


  # ----------------------------------------------------------
  # 7. Allocate HD
  # ----------------------------------------------------------

  hd <- array(
    0,
    dim = c(N, N, T, S)
  )


  # ----------------------------------------------------------
  # 8. Historical decomposition
  #
  # For each draw:
  #
  #   HD[i,j,t] =
  #       sum_h IRF[i,j,h] *
  #               shock[j,t-h+1]
  #
  # ----------------------------------------------------------

  for (s in seq_len(S)) {

    ir_s <- ir_array[, , , s]

    shocks_s <- shocks_array[, , s]


    for (t in seq_len(T)) {

      max_h <- min(t, T)

      for (h in seq_len(max_h)) {

        shock_time <- t - h + 1

        hd[, , t, s] <-
          hd[, , t, s] +
          ir_s[, , h] %*%
          diag(
            shocks_s[, shock_time],
            nrow = N
          )
      }
    }


    # Progress
    if (
      show_progress &&
      (
        s %% 500 == 0 ||
        s == S
      )
    ) {

      message(
        "Completed ",
        s,
        " / ",
        S,
        " draws"
      )
    }
  }


  # ----------------------------------------------------------
  # 9. Class
  # ----------------------------------------------------------

  class(hd) <- "PosteriorHD"

  return(hd)
}




compute_hd_bsvarSIGN_fast <- function(posterior, show_progress = TRUE) {
  
  # ----------------------------------------------------------
  # 1. Setup & Information
  # ----------------------------------------------------------
  posterior_A <- posterior$posterior$A
  Y           <- posterior$last_draw$data_matrices$Y # [N x T]
  N           <- dim(posterior_A)[1]
  T_len       <- ncol(Y)
  S           <- dim(posterior_A)[3]
  
  standardise <- TRUE
  sign_irf    <- posterior$last_draw$identification$sign_irf
  if (any(diag(sign_irf[, , 1]) == 0, na.rm = TRUE)) standardise <- FALSE
  
  # ----------------------------------------------------------
  # 2. Structural Shocks & IRFs
  # ----------------------------------------------------------
  if (show_progress) message("Computing structural shocks and impulse responses...")
  
  shocks_array <- as.array(compute_structural_shocks(posterior))                      # [N x T x S]
  ir_array     <- as.array(compute_impulse_responses(posterior, horizon = T_len - 1, 
                                                    standardise = standardise))       # [N x N x T x S]
  
  hd <- array(0, dim = c(N, N, T_len, S))
  
  # ----------------------------------------------------------
  # 3. Optimized Double Loop (Draws x Time)
  # ----------------------------------------------------------
  if (show_progress) message("Running fast HD engine...")
  
  for (s in seq_len(S)) {
    ir_s     <- ir_array[, , , s]    # [N x N x T]
    shocks_s <- shocks_array[, , s]  # [N x T]
    
    for (t in seq_len(T_len)) {
      # Loop over past shocks up to current time t
      for (h in 1:t) {
        # Fast matrix-vector scaling via outer product / elementwise multiplication
        # Avoids sweep() overhead completely
        hd[, , t, s] <- hd[, , t, s] + ir_s[, , h] * matrix(shocks_s[, t - h + 1], 
                                                           nrow = N, ncol = N, byrow = TRUE)
      }
    }
    
    if (show_progress && (s %% 500 == 0 || s == S)) {
      message(sprintf("Completed %d / %d draws", s, S))
    }
  }
  
  # ----------------------------------------------------------
  # 4. Extract Base Component (Initial Conditions)
  # ----------------------------------------------------------
  hd_mean        <- apply(hd, c(1, 2, 3), mean)       # [N x N x T]
  hd_sum_shocks  <- apply(hd_mean, c(1, 3), sum)      # [N x T]
  base_component <- Y - hd_sum_shocks                  # [N x T]
  
  out <- list(
    hd_array       = hd,
    hd_mean        = hd_mean,
    base_component = base_component,
    actual_data    = Y
  )
  
  class(out) <- "PosteriorHD"
  return(out)
}




compute_hd_bsvarSIGN_safe_test <- function(posterior, show_progress = TRUE) {
  
  # ----------------------------------------------------------
  # 1. Information & Setup
  # ----------------------------------------------------------
  posterior_A <- posterior$posterior$A
  
  # Extract actual fitted data matrix (N x T)
  Y <- posterior$last_draw$data_matrices$Y
  N <- dim(posterior_A)[1]
  T_len <- ncol(Y)
  S <- dim(posterior_A)[3]
  
  # ----------------------------------------------------------
  # 2. Determine Standardisation
  # ----------------------------------------------------------
  standardise <- TRUE
  sign_irf <- posterior$last_draw$identification$sign_irf
  
  if (any(diag(sign_irf[, , 1]) == 0, na.rm = TRUE)) {
    standardise <- FALSE
  }
  
  # ----------------------------------------------------------
  # 3. Structural Shocks & IRFs
  # ----------------------------------------------------------
  if (show_progress) message("Computing structural shocks and impulse responses...")
  
  shocks_array <- as.array(compute_structural_shocks(posterior)) # [N x T x S]
  ir_array     <- as.array(compute_impulse_responses(posterior, horizon = T_len - 1, standardise = standardise)) # [N x N x T x S]
  
  # Allocate HD array: [Response_Var, Shock_Var, Time, Draw]
  hd <- array(0, dim = c(N, N, T_len, S))
  
  # ----------------------------------------------------------
  # 4. Vectorized Historical Decomposition Loop
  # ----------------------------------------------------------
  for (s in seq_len(S)) {
    ir_s     <- ir_array[, , , s]     # [N x N x Horizon]
    shocks_s <- shocks_array[, , s]   # [N x Time]
    
    for (t in seq_len(T_len)) {
      # Vectorized convolution: sum_h ( IRF_h * Shock_{t-h+1} )
      for (h in 1:t) {
        shock_time <- t - h + 1
        # Multiply each column of IRF by the corresponding structural shock at shock_time
        hd[, , t, s] <- hd[, , t, s] + sweep(ir_s[, , h], 2, shocks_s[, shock_time], `*`)
      }
    }
    
    if (show_progress && (s %% 500 == 0 || s == S)) {
      message(sprintf("Completed %d / %d draws", s, S))
    }
  }
  
  # ----------------------------------------------------------
  # 5. Extract Base Component (Initial Conditions + Deterministic)
  # ----------------------------------------------------------
  # Posterior mean of structural contributions: [N x N x T]
  hd_mean <- apply(hd, c(1, 2, 3), mean)
  
  # Sum shock contributions across all shocks for each variable: [N x T]
  hd_sum_shocks <- apply(hd_mean, c(1, 3), sum)
  
  # Initial conditions = Actual Y - Sum of Shock Contributions
  base_component <- Y - hd_sum_shocks
  
  # Package results into a structured list
  out <- list(
    hd_array       = hd,             # Raw 4D array [N, N, T, S]
    hd_mean        = hd_mean,        # Posterior mean [N, N, T]
    base_component = base_component, # Initial conditions / baseline trend [N, T]
    actual_data    = Y               # Realized data [N, T]
  )
  
  class(out) <- "PosteriorHD"
  return(out)
}
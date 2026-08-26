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
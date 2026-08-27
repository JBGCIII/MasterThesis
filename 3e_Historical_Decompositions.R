





plot_hd_response(
  hd_array = hd_manual_test,
  posterior = run_bsvar_sign_model_01,
  response = "gdp_growth"
)



getS3method(
  "compute_historical_decompositions",
  "PosteriorBSVARSIGN"
)

methods(compute_historical_decompositions)



run_bsvar_sign_model_01 <- readRDS(
  "run_bsvar_sign_model_1000.rds"
)


hd_pkg_test <- compute_historical_decompositions(
  run_bsvar_sign_model_01,
  show_progress = FALSE
)


hd_manual_test <- compute_hd_bsvarSIGN_safe(
  run_bsvar_sign_model_01,
  show_progress = TRUE
)


saveRDS(
  hd_manual_test,
  "hd_manual_test.rds"
)



hd_manual_test <- readRDS(
  "hd_manual_test.rds"
)



run_bsvar_sign_model_01 <- readRDS(
  "run_bsvar_sign_model_1000.rds"
)



# Column of interest. Note how Debt to Income is left out.
var_names <- c(
  "gdp_growth",
  "consumption_growth",
  "cpi_growth",
  "saving_rate",
  "policy_rate",
  "interest_burden",
  "debt_growth",
  "real_house_price_growth",
  "asset_liability_ratio"
)







var_names <- dimnames(
  run_bsvar_sign_model_01$last_draw$data_matrices$Y
)[[1]]

response_number <- which(var_names == "gdp_growth")


hd_gdp <- hd_manual_test[
  response_number,
  ,,
]

hd_gdp_mean <- apply(
  hd_gdp,
  c(1, 2),
  mean
)

hd_plot <- as.data.frame(
  as.table(hd_gdp_mean)
)

names(hd_plot) <- c(
  "Shock",
  "Time",
  "Contribution"
)

hd_plot$Shock <- var_names[
  as.integer(hd_plot$Shock)
]

hd_plot$Time <- as.integer(hd_plot$Time)



ggplot(
  hd_plot,
  aes(
    x = Time,
    y = Contribution,
    fill = Shock
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Historical Decomposition of GDP Growth",
    subtitle = "Posterior mean contributions of structural shocks",
    x = NULL,
    y = "Contribution to GDP growth",
    fill = "Structural shock"
  ) +
  theme_minimal()







response_number <- 1


library(dplyr)
library(tidyr)
library(ggplot2)

# Extract the HD for one response variable
# Dimensions: shock x time x draws

hd_gdp <- hd_manual_test[
  response_number,
  ,,
]

# Posterior mean across draws
hd_gdp_mean <- apply(
  hd_gdp,
  c(1, 2),
  mean
)

# Convert to data frame
hd_plot <- as.data.frame(
  as.table(hd_gdp_mean)
)

names(hd_plot) <- c(
  "Shock",
  "Time",
  "Contribution"
)

hd_plot$Shock <- var_names[
  as.integer(hd_plot$Shock)
]

hd_plot$Time <- as.integer(
  hd_plot$Time
)









ggplot(
  hd_plot,
  aes(
    x = Time,
    y = Contribution,
    fill = Shock
  )
) +
  geom_col(
    position = "stack"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = paste(
      "Historical Decomposition of",
      var_names[response_number]
    ),
    subtitle = "Posterior mean contribution of structural shocks",
    x = NULL,
    y = "Contribution",
    fill = "Structural shock"
  ) +
  theme_minimal()





plot_hd_response(
  hd_array = hd_manual_test,
  posterior = run_bsvar_sign_model_01,
  response = "saving_rate"
)







bob











# ============================================================
# FUNCTION: Plot Historical Decomposition for One Variable
# ============================================================
#
# Purpose:
#   Plots the posterior mean historical decomposition of one
#   response variable, showing the contributions of all
#   structural shocks over time.
#
# Input:
#   hd_array    = PosteriorHD array
#                 [response, shock, time, draw]
#
#   posterior   = BVAR posterior object, used to obtain the
#                 correct variable names
#
#   response    = name of response variable
#
# Output:
#   ggplot object
#
# ============================================================

plot_hd_response <- function(
    hd_array,
    posterior,
    response = "gdp_growth"
) {

  # ----------------------------------------------------------
  # 1. Get variable names directly from the BVAR
  # ----------------------------------------------------------

  var_names <- dimnames(
    posterior$last_draw$data_matrices$Y
  )[[1]]

  if (is.null(var_names)) {
    stop("Could not find variable names in the BVAR object.")
  }


  # ----------------------------------------------------------
  # 2. Check that requested response exists
  # ----------------------------------------------------------

  response_number <- match(
    response,
    var_names
  )

  if (is.na(response_number)) {
    stop(
      paste0(
        "Response variable '",
        response,
        "' not found.\n\nAvailable variables:\n",
        paste(var_names, collapse = "\n")
      )
    )
  }


  # ----------------------------------------------------------
  # 3. Check HD dimensions
  # ----------------------------------------------------------

  hd_dims <- dim(hd_array)

  if (length(hd_dims) != 4) {
    stop(
      "HD array must have four dimensions: ",
      "[response, shock, time, draw]."
    )
  }

  N <- hd_dims[1]
  S <- hd_dims[2]
  T <- hd_dims[3]
  D <- hd_dims[4]

  if (N != length(var_names)) {
    stop(
      "Number of variables in HD does not match ",
      "number of variable names in the BVAR."
    )
  }

  if (S != N) {
    stop(
      "Expected one structural shock per variable."
    )
  }


  # ----------------------------------------------------------
  # 4. Extract selected response variable
  # ----------------------------------------------------------
  #
  # Result:
  #   [shock, time, draw]

  hd_response <- hd_array[
    response_number,
    , ,
  ]


  # ----------------------------------------------------------
  # 5. Posterior mean
  # ----------------------------------------------------------

  hd_mean <- apply(
    hd_response,
    c(1, 2),
    mean,
    na.rm = TRUE
  )


  # ----------------------------------------------------------
  # 6. Construct plotting data
  # ----------------------------------------------------------

  hd_plot <- data.frame(

    Shock = rep(
      var_names[seq_len(S)],
      times = T
    ),

    Time = rep(
      seq_len(T),
      each = S
    ),

    Contribution = as.vector(
      hd_mean
    )
  )


  # ----------------------------------------------------------
  # 7. Plot
  # ----------------------------------------------------------

  p <- ggplot2::ggplot(
    hd_plot,
    ggplot2::aes(
      x = Time,
      y = Contribution,
      fill = Shock
    )
  ) +

    ggplot2::geom_col(
      width = 0.9
    ) +

    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +

    ggplot2::labs(
      title = paste(
        "Historical Decomposition of",
        response
      ),

      subtitle = paste(
        "Posterior mean across",
        format(D, big.mark = ","),
        "draws"
      ),

      x = NULL,

      y = "Contribution",

      fill = "Structural shock"
    ) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "right"
    )


  # ----------------------------------------------------------
  # 8. Return plot
  # ----------------------------------------------------------

  return(p)
}






# Get all variable names from your model
var_names <- dimnames(bvar_fit$last_draw$data_matrices$Y)[[1]]

# Generate a list of ggplot objects for every variable
all_hd_plots <- lapply(var_names, function(var) {
  plot_hd_response(
    hd_array  = hd_results,
    posterior = bvar_fit,
    response  = var
  )
})

# Name the list elements by variable name
names(all_hd_plots) <- var_names




# 1. Run the function for a specific variable
p_saving <- plot_hd_response(
  hd_array  = hd_results,   # Your 4D HD array [N, S, T, D]
  posterior = bvar_fit,     # Your estimated BVAR posterior object
  response  = "saving_rate" # The response variable you want to plot
)

# 2. Display the plot
print(p_saving)


plot_hd_response <- function(
    hd_array,
    posterior,
    response = "gdp_growth"
) {

  # ----------------------------------------------------------
  # 1. Get variable names directly from the BVAR
  # ----------------------------------------------------------
  var_names <- dimnames(
    posterior$last_draw$data_matrices$Y
  )[[1]]

  if (is.null(var_names)) {
    stop("Could not find variable names in the BVAR object.")
  }

  # ----------------------------------------------------------
  # 2. Check that requested response exists
  # ----------------------------------------------------------
  response_number <- match(response, var_names)

  if (is.na(response_number)) {
    stop(
      paste0(
        "Response variable '", response, "' not found.\n\nAvailable variables:\n",
        paste(var_names, collapse = "\n")
      )
    )
  }

  # ----------------------------------------------------------
  # 3. Check HD dimensions
  # ----------------------------------------------------------
  hd_dims <- dim(hd_array)

  if (length(hd_dims) != 4) {
    stop("HD array must have four dimensions: [response, shock, time, draw].")
  }

  N <- hd_dims[1]
  S <- hd_dims[2]
  T <- hd_dims[3]
  D <- hd_dims[4]

  if (N != length(var_names)) {
    stop("Number of variables in HD does not match number of variable names in the BVAR.")
  }

  if (S != N) {
    stop("Expected one structural shock per variable.")
  }

  # ----------------------------------------------------------
  # 4. Extract selected response variable & calculate mean
  # ----------------------------------------------------------
  # Result: [shock, time, draw]
  hd_response <- hd_array[response_number, , , ]

  # Posterior mean matrix [S, T]
  hd_mean <- apply(hd_response, c(1, 2), mean, na.rm = TRUE)

  # ----------------------------------------------------------
  # 5. Construct plotting data safely
  # ----------------------------------------------------------
  # Transposing hd_mean to [T, S] ensures standard row-major flattening matches
  # Time (each = S) and Shock (times = T)
  hd_plot <- data.frame(
    Time = rep(seq_len(T), each = S),
    Shock = rep(var_names[seq_len(S)], times = T),
    Contribution = as.vector(t(hd_mean))
  )

  # Turn Shock into a factor to preserve original variable ordering in the legend
  hd_plot$Shock <- factor(hd_plot$Shock, levels = var_names)

  # ----------------------------------------------------------
  # 6. Plot
  # ----------------------------------------------------------
  p <- ggplot2::ggplot(
    hd_plot,
    ggplot2::aes(
      x = Time,
      y = Contribution,
      fill = Shock
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = FALSE),
      width = 0.9
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = paste("Historical Decomposition of", response),
      subtitle = paste("Posterior mean across", format(D, big.mark = ","), "draws"),
      x = NULL,
      y = "Contribution",
      fill = "Structural shock"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right"
    )

  return(p)
}













# ============================================================
# FUNCTION: Plot Historical Decomposition for One Variable
# ============================================================
#
# Purpose:
#   Plots the posterior mean historical decomposition of one
#   response variable, showing the contributions of all
#   structural shocks over time.
#
# Input:
#   hd_array    = PosteriorHD array
#                 [response, shock, time, draw]
#
#   posterior   = BVAR posterior object, used to obtain the
#                 correct variable names
#
#   response    = name of response variable
#
# Output:
#   ggplot object
#
# ============================================================

plot_hd_response <- function(
    hd_array,
    posterior,
    response = "gdp_growth"
) {

  # ----------------------------------------------------------
  # 1. Get variable names directly from the BVAR
  # ----------------------------------------------------------

  var_names <- dimnames(
    posterior$last_draw$data_matrices$Y
  )[[1]]

  if (is.null(var_names)) {
    stop("Could not find variable names in the BVAR object.")
  }


  # ----------------------------------------------------------
  # 2. Check that requested response exists
  # ----------------------------------------------------------

  response_number <- match(
    response,
    var_names
  )

  if (is.na(response_number)) {
    stop(
      paste0(
        "Response variable '",
        response,
        "' not found.\n\nAvailable variables:\n",
        paste(var_names, collapse = "\n")
      )
    )
  }


  # ----------------------------------------------------------
  # 3. Check HD dimensions
  # ----------------------------------------------------------

  hd_dims <- dim(hd_array)

  if (length(hd_dims) != 4) {
    stop(
      "HD array must have four dimensions: ",
      "[response, shock, time, draw]."
    )
  }

  N <- hd_dims[1]
  S <- hd_dims[2]
  T <- hd_dims[3]
  D <- hd_dims[4]

  if (N != length(var_names)) {
    stop(
      "Number of variables in HD does not match ",
      "number of variable names in the BVAR."
    )
  }

  if (S != N) {
    stop(
      "Expected one structural shock per variable."
    )
  }


  # ----------------------------------------------------------
  # 4. Extract selected response variable
  # ----------------------------------------------------------
  #
  # Result:
  #   [shock, time, draw]

  hd_response <- hd_array[
    response_number,
    , ,
  ]


  # ----------------------------------------------------------
  # 5. Posterior mean
  # ----------------------------------------------------------

  hd_mean <- apply(
    hd_response,
    c(1, 2),
    mean,
    na.rm = TRUE
  )


  # ----------------------------------------------------------
  # 6. Construct plotting data
  # ----------------------------------------------------------

  hd_plot <- data.frame(

    Shock = rep(
      var_names[seq_len(S)],
      times = T
    ),

    Time = rep(
      seq_len(T),
      each = S
    ),

    Contribution = as.vector(
      hd_mean
    )
  )


  # ----------------------------------------------------------
  # 7. Plot
  # ----------------------------------------------------------

  p <- ggplot2::ggplot(
    hd_plot,
    ggplot2::aes(
      x = Time,
      y = Contribution,
      fill = Shock
    )
  ) +

    ggplot2::geom_col(
      width = 0.9
    ) +

    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +

    ggplot2::labs(
      title = paste(
        "Historical Decomposition of",
        response
      ),

      subtitle = paste(
        "Posterior mean across",
        format(D, big.mark = ","),
        "draws"
      ),

      x = NULL,

      y = "Contribution",

      fill = "Structural shock"
    ) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "right"
    )


  # ----------------------------------------------------------
  # 8. Return plot
  # ----------------------------------------------------------

  return(p)
}
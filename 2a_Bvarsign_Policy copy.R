###############################################################################
################ 2.BAYESIAN ESTIMATION WITH SIGN RESTRICTION  #################
###############################################################################

# Directories
dir.create(
  "Analysis", 
  showWarnings = FALSE
)

dir.create(
  "Analysis/BVAR_Sign",
  showWarnings = FALSE
)

dir.create(
  "Analysis/BVAR_Sign/Model_01",
  showWarnings = FALSE
)


dir.create(
  "Analysis/BVAR_Sign/Model_01/IRF",
  showWarnings = FALSE
)



#============================================================================#
#                              [1] Matrix Set Up
#============================================================================#

# Load Seasonally and Outlier Adjusted Data.
bvar_data <- read.csv(
  "Processed_Data/1e_final_data_seasonal_outliers_adjusted.csv"
)

# Column of interest. Note how Debt to Income is left out.
target_cols <- c(
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

# Create a Matrix.
bvar_matrix_policy_shock <- as.matrix(
  bvar_data[, target_cols]
)

# Variable Indices. 
gdp_idx <- which(target_cols == "gdp_growth")
consumption_idx <- which(target_cols == "consumption_growth")
cpi_idx <- which(target_cols == "cpi_growth")
saving_idx <- which(target_cols == "saving_rate")
policy_idx <- which(target_cols == "policy_rate")
interest_burden_idx <- which(target_cols == "interest_burden")
debt_idx <- which(target_cols == "debt_growth")
house_idx <- which(target_cols == "real_house_price_growth")
asset_liability_idx <- which(target_cols == "asset_liability_ratio")


# Number of Rows and Columns (Variable X Shock)
N <- ncol(bvar_matrix_policy_shock) # nolint
H_total <- 20 # Time Periods (5 years)


#============================================================================#
#                              [2] Sing Restriction Set UP
#============================================================================#

## [Model 1: Policy Rate Shock]
sign_irf_policy <- array(
  NA, # ensures no sign restrictions 
  dim = c(N, N, H_total) # Variables Columns X Shock Columns X Time Periods
)

#How does the Swedish Economy react to a prolonged monetary policy contraciton?
H_restrict_policy <- 2 # How long the restriction will apply.

# [Model 1] Monetary policy shock
for (h in 1:H_restrict_policy) { # For all the entries in the matrices
  # when restriction applies, apply the following
  # sign restriction.

  sign_irf_policy[policy_idx, policy_idx, h] <-  1  # Positive shock in policy
  sign_irf_policy[cpi_idx, policy_idx, h] <-     -1 # Negative shock in CPI
  sign_irf_policy[interest_burden_idx, policy_idx, h] <-  1 # Positive shock in Interest.
  sign_irf_policy[debt_idx, policy_idx, h] <-    -1 # Negative shock in debt
}


#============================================================================#
#                              [3] Model Specs
#============================================================================#

# bsvarSIGNs convention:
#TRUE = Random Walk (Mean = 1)
#FALSE = White Noise (Mean = 0)
# [stationary] an N logical vector - its element set to FALSE sets the
# prior mean for the autoregressive parameters of the Nth equation to
# the white noise process, otherwise to random walk."
# Source: https://bsvars.org/bsvarSIGNs/reference/specify_bsvarSIGN.html


is_random_walk <- c(
  FALSE,  # gdp_growth 
  FALSE,  # consumption_growth
  FALSE,  # cpi_growth
  TRUE,   # saving_rate
  TRUE,   # policy_rate
  TRUE,   # interest_burden
  TRUE,  # debt_growth
  FALSE,  # real_house_price_growth
  TRUE    # asset_liability_ratio
)

# Use multi-core execution (detect available cores)
# This allows to run the model faster. You need to have one
# removed, otherwise you might not be able to run your machine
# Mine was seven and it took quite a while. I hope your is not
# Less than that.
n_cores <- max(1, parallel::detectCores() - 1)

spec_01 <- specify_bsvarSIGN$new(
  data         = bvar_matrix_policy_shock,
  p            = 4,
  sign_irf     = sign_irf_policy,
  stationary   = is_random_walk,
  hyper_lambda = TRUE,  # Estimate GLP overall shrinkage
  hyper_mu     = TRUE,  # Estimate sum-of-coefficients dummy prior
  hyper_delta  = TRUE,  # Estimate single-unit-root dummy prior
  hyper_psi    = TRUE,  # Estimate scale hyperparameters
  mc.cores     = n_cores
)

#============================================================================#
#                              [4] Model Run
#============================================================================#

# Optimal hyperparameter starting points using Adaptive Metropolis
set.seed(321)
spec_01$estimate_hyper(S = 20000, burn_in = 10000)

run_bsvar_sign_model_01_10000 <- estimate(
  spec_01,
  S = 10000,
  thin = 5 # Reduces memory overhead and post-processing steps directly
)

#15:42


run_bsvar_sign_model_01 <- readRDS("run_bsvar_sign_model_01.rds")

run_bsvar_sign_model_01 <- readRDS("run_bsvar_sign_model_01.rds")

irf_1000_30 <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = 30
)


irf_test <- irf_20[, , , 1]

dim(irf_test)



run_bsvar_sign_model_02 <- readRDS(
  "run_bsvar_sign_model_01_10000.rds"
)


irf_30 <- compute_impulse_responses(
  run_bsvar_sign_model_02,
  horizon = 30
)


saveRDS(
  irf_30,
  "IRF_10000_horizon30.rds"
)


dim(irf_30)
object.size(irf_30)


irf_test <- irf_30[, , , 1]

dim(irf_test)




sq <- irf_test^2

cum_sq <- apply(
  sq,
  c(1, 2),
  cumsum
)

dim(cum_sq)



fevd_test <- array(
  NA_real_,
  dim = c(9, 9, 31)
)

for (h in 1:31) {

  total_var <- rowSums(cum_sq[, , h])

  fevd_test[, , h] <-
    sweep(
      cum_sq[, , h],
      1,
      total_var,
      "/"
    )
}

fevd_manual <- compute_fevd_from_irf2(irf_1000_30)

fevd_package <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 30
)




cum_sq <- array(
  0,
  dim = dim(sq)
)

cum_sq[, , 1] <- sq[, , 1]

if (dim(sq)[3] > 1) {
  for (h in 2:dim(sq)[3]) {
    cum_sq[, , h] <- cum_sq[, , h - 1] + sq[, , h]
  }
}


dim(irf_test)
dim(sq)
str(irf_test)
str(sq)


cum_sq <- array(
  0,
  dim = dim(sq)
)

cum_sq[, , 1] <- sq[, , 1]

for (h in 2:dim(sq)[3]) {
  cum_sq[, , h] <- cum_sq[, , h - 1] + sq[, , h]
}

dim(cum_sq)



fevd_test <- array(
  NA_real_,
  dim = dim(cum_sq)
)

for (h in 1:dim(cum_sq)[3]) {

  total_var <- rowSums(cum_sq[, , h])

  fevd_test[, , h] <- sweep(
    cum_sq[, , h],
    1,
    total_var,
    "/"
  )
}


dim(fevd_test)

round(
  rowSums(fevd_test[, , 31]),
  10
)


max(
  abs(
    apply(fevd_test, c(1, 3), sum) - 1
  )
)







compute_fevd_from_irf_safe <- function(irf_obj) {

  # Extract the IRF array
  if (is.array(irf_obj)) {
    irf_array <- irf_obj
  } else if (inherits(irf_obj, "PosteriorIR")) {
    irf_array <- irf_obj[,,,]
  } else if (!is.null(irf_obj$posterior$irf)) {
    irf_array <- irf_obj$posterior$irf
  } else if (!is.null(irf_obj$irf)) {
    irf_array <- irf_obj$irf
  } else {
    stop("Could not extract 4D IRF array.")
  }

  dims <- dim(irf_array)

  N <- dims[1]
  H <- dims[3]
  S <- dims[4]

  message(
    "Computing FEVD: ",
    N, " variables × ",
    H, " horizons × ",
    S, " draws"
  )

  # Allocate final result
  fevd <- array(
    NA_real_,
    dim = c(N, N, H, S),
    dimnames = dimnames(irf_array)
  )

  # Process each posterior draw separately
  for (s in seq_len(S)) {

    # Extract one draw
    irf_s <- irf_array[, , , s]

    # Squared IRFs
    sq <- irf_s^2

    # Cumulative squared responses
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

    # Normalize at each horizon
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
        )
    }

    # Progress indicator
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

  class(fevd) <- "PosteriorFEVD"

  return(fevd)
}



dim(irf_30)

irf_30 <- readRDS("IRF_10000_horizon30.rds")

fevd_10000 <- compute_fevd_from_irf_safe(irf_30)



saveRDS(
  fevd_10000,
  "FEVD_10000_horizon30_manual.rds"
)


dim(fevd_10000)



max(
  abs(
    apply(
      fevd_10000,
      c(1, 3, 4),
      sum
    ) - 1
  )
)


saveRDS(
  fevd_10000,
  "FEVD_10000_horizon30_manual.rds"
)

file.info("FEVD_10000_horizon30_manual.rds")$size


fevd_pkg_test <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 10
)

irf_pkg_test <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = 10
)

fevd_manual_test <- compute_fevd_from_irf_safe(
  irf_pkg_test
)

max(
  abs(
    fevd_pkg_test - fevd_manual_test
  ),
  na.rm = TRUE
)

irf_test <- irf_20[, , , 1]



dim(fevd_pkg_test)
dim(fevd_manual_test)
fevd_pkg_test[1, 1, , 1]
fevd_manual_test[1, 1, , 1]

range(fevd_pkg_test, na.rm = TRUE)
range(fevd_manual_test, na.rm = TRUE)

sum(fevd_pkg_test[, , 11, 1])
rowSums(fevd_pkg_test[, , 11, 1])

rowSums(fevd_manual_test[, , 11])


fevd_manual_pct <- fevd_manual_test * 100

fevd_pkg_test[1, 1, , 1]
fevd_manual_pct[1, 1, , 1]

getS3method(
  "compute_impulse_responses",
  "PosteriorBSVARSIGN"
)




irf_1000_std <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = 10,
  standardise = TRUE
)


fevd_manual_std <- compute_fevd_from_irf_safe(
  irf_1000_std
)

fevd_manual_std_pct <- fevd_manual_std * 100

max(
  abs(
    fevd_pkg_test - fevd_manual_std_pct
  ),
  na.rm = TRUE
)




irf_10000_std <- compute_impulse_responses(
  run_bsvar_sign_model_02,
  horizon = 30,
  standardise = TRUE
)

saveRDS(
  irf_10000_std,
  "IRF_10000_horizon30_standardised.rds"
)

dim(irf_10000_std)


fevd_10000 <- compute_fevd_from_irf_safe(
  irf_10000_std
)

fevd_10000_pct <- fevd_10000 * 100


saveRDS(
  fevd_10000_pct,
  "FEVD_10000_horizon30.rds"
)


max(
  abs(
    apply(
      fevd_10000_pct,
      c(1, 3, 4),
      sum
    ) - 100
  )
)


dim(irf_test)



dim(run_bsvar_sign_model_01_10000$posterior$A)
dim(run_bsvar_sign_model_01_10000$posterior$Theta0)
dim(run_bsvar_sign_model_01_10000$posterior$shocks)


gc()

test_fevd_30 <- compute_variance_decompositions(
  run_bsvar_sign_model_01_10000,
  horizon = 20
)

test_fevd_30



run_bsvar_sign_model_02 <- readRDS(
  "run_bsvar_sign_model_01_10000.rds"
)




run_bsvar_sign_model_01 <- readRDS("run_bsvar_sign_model_01.rds")





irf_1000_30 <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = 30
)





test_fevd_10 <- compute_variance_decompositions(
  run_bsvar_sign_model_02,
  horizon = 20
)


#The FEVD C++ routine is the thing crashing.





posterior_irf = .Call(
    `_bsvarSIGNs_bsvarSIGNs_ir`,
    posterior_A,
    posterior_Theta0,
    horizon,
    p,
    standardise
)

qqq = .Call(
    `_bsvarSIGNs_bsvarSIGNs_fevd`,
    posterior_irf
)


run_bsvar_sign_model_02$posterior$ess


saveRDS(run_bsvar_sign_model_01_10000, "run_bsvar_sign_model_01_10000.rds")
save.image("BVAR_workspace_10000.RData")





run_bsvar_sign_model_02 <- readRDS("run_bsvar_sign_model_01.rds")

object.size(run_bsvar_sign_model_02)


class(run_bsvar_sign_model_01)
str(run_bsvar_sign_model_01)
names(run_bsvar_sign_model_01)

print(run_bsvar_sign_model_01)


names(run_bsvar_sign_model_01$posterior)

str(run_bsvar_sign_model_01$posterior, max.level = 2)

object.size(run_bsvar_sign_model_01$posterior)


saveRDS(
  run_bsvar_sign_model_01,
  "run_bsvar_sign_model_01_1000.rds"
)

file.info("run_bsvar_sign_model_01_1000.rds")$size



Theta0 <- run_bsvar_sign_model_01$posterior$Theta0

dim(Theta0)
object.size(Theta0)


shocks <- run_bsvar_sign_model_01$posterior$shocks

dim(shocks)
object.size(shocks)


packageVersion("bsvarSIGNs")
packageVersion("bsvars")

find("compute_variance_decompositions")

methods(class = "PosteriorBSVARSIGN")

run_bsvar_sign_model_01 <- readRDS("run_bsvar_sign_model_01_1000.rds")

packageVersion("Rcpp")
packageVersion("RcppArmadillo")
packageVersion("RcppParallel")

sessionInfo()


methods(compute_variance_decompositions)

getS3method(
  "compute_variance_decompositions",
  "PosteriorBSVARSIGN"
)






posterior_Theta0 <- run_bsvar_sign_model_01$posterior$Theta0

posterior_A <- run_bsvar_sign_model_01$posterior$A
posterior_A <- aperm(posterior_A, c(2, 1, 3))

N <- dim(posterior_A)[2]
p <- run_bsvar_sign_model_01$last_draw$p
S <- dim(posterior_A)[3]

standardise <- TRUE

if (
  any(
    diag(run_bsvar_sign_model_01$last_draw$identification$sign_irf[, , 1]) == 0
  ) &
  !is.na(
    any(
      diag(run_bsvar_sign_model_01$last_draw$identification$sign_irf[, , 1]) == 0
    )
  )
) {
  standardise <- FALSE
}


posterior_irf <- .Call(
  `_bsvarSIGNs_bsvarSIGNs_ir`,
  posterior_A,
  posterior_Theta0,
  5,
  p,
  standardise
)


test_irf <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = 5
)

test_fevd <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 5
)


test_fevd_10 <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 10
)


test_fevd_20 <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 20
)


test_fevd_30 <- compute_variance_decompositions(
  run_bsvar_sign_model_01,
  horizon = 30
)


object.size(test_fevd_30)



#============================================================================#
#                              [5] Model Diagnostics
#============================================================================#

# Trace Plot

# Let's find Alpha and Lamba
# str(spec$prior)
#Row Index     Parameter    DescriptionRows
#1 to 3$       mu (1-3)     Minnesota prior shrinkage parameters for means.
#Rows 4 to 6   delta (1-3)  Persistence / autoregressive shrinkage parameters.
#Rows 7 to 9   psi(1-3)     Scale parameters for individual equations.
#Row 10$       lambda       Overall Minnesota prior tightness hyperparameter.
#Row 11$       alpha        Lag decay hyperparameter.
#Rows 12 to 16 Additional   Dummy observation weights.

#------------------------------------------------------------------------------#
# Alpha
png(
"Analysis/BVAR_Sign/Model_01/Alpha_Traces.png",
  width  = 3200,
  height = 2600,
  res    = 300
)

plot(run_bsvar_sign_model_01$posterior$hyper[11, ], 
  type = "l",
  ylab = "alpha",
  col = "#000000"
)

dev.off()

# Lambda
png(
  "Analysis/BVAR_Sign/Model_01/Lambda_Traces.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
plot(run_bsvar_sign_model_01$posterior$hyper[10, ], 
  type = "l", 
  ylab = "lambda", 
  col = "#000000")
dev.off()

#------------------------------------------------------------------------------#

# ACF for Alpha
png(
  "Analysis/BVAR_Sign/Model_01/ACF_Alpha.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
acf(run_bsvar_sign_model_01$posterior$hyper[11, ]
, main = "ACF - Lambda")

dev.off()


# ACF for Lambda to check chain mixing
png(
  "Analysis/BVAR_Sign/Model_01/ACF_Lambda.png",
  width  = 3200,
  height = 2600,
  res    = 300
)
acf(run_bsvar_sign_model_01$posterior$hyper[10, ]
, main = "ACF - Lambda")

dev.off()



#------------------------------------------------------------------------------#
# Density Plot

# Alpha
png(
"Analysis/BVAR_Sign/Model_01/Density_Alpha.png",
 width  = 3200,
 height = 2600,
 res    = 300
)

plot(
  density(run_bsvar_sign_model_01$posterior$hyper[11, ]), 
  main = "Posterior Density of Alpha", 
  xlab = "alpha", 
  col = "#000000",
  lwd = 2
)
dev.off()

# Lambda
png(
"Analysis/BVAR_Sign/Model_01/Density_Lamba.png",
 width  = 3200,
 height = 2600,
 res    = 300
)

plot(
  density(run_bsvar_sign_model_01$posterior$hyper[10, ]), 
  main = "Posterior Density of Lambda", 
  xlab = "lambda", 
  col = "#000000",
  lwd = 2
)
dev.off()

#------------------------------------------------------------------------------#
# Export Hyperparamaters

export_hyperparameters(
  estimation_obj = run_bsvar_sign_model_01,
  target_cols    = target_cols,
  file_path      = "Analysis/BVAR_Sign/Model_01/hyperparameter_summary.csv"
)

#------------------------------------------------------------------------------#
# Effective Sample Size

ess_results <- compute_and_export_ess(
  model_obj = run_bsvar_sign_model_01,
  file_path = "Analysis/BVAR_Sign/Model_01/ESS_Diagnostics_Model_01.csv")

#============================================================================#
#                         [5] Historical Decomposition
#============================================================================#

compute_hd_bsvarSIGN

hd_results <- compute_hd_bsvarSIGN(
  posterior_obj = run_bsvar_sign_model_01,
  batch_size    = 200
)



save_hd_batched_to_csv(
  hd_results = hd_results, 
  file_path  = "Analysis/BVAR_Sign/Model_01/hd_summary_results.csv"
)


hd_draws <- compute_hd_batched(run_bsvar_sign_model_01)

# Explicitly pass your dataset's row count as the horizon
hd_draws <- compute_hd_batched(
  posterior_obj = run_bsvar_sign_model_01, 
  horizon       = nrow(bvar_matrix_policy_shock)
)

# Run 01 | Draws = 1000 | Start Time 21:39 | End Time 21:39.

# Assign actual target variable names before running the export function
var_names <- target_cols  # c("gdp_growth", "consumption_growth", ...)

dimnames(hd_draws) <- list(
  Variable = var_names,
  Shock    = var_names,
  Time     = 1:dim(hd_draws)[3]
)

# Export the 3D output from compute_hd_batched
hd_df <- save_hd_batched_to_csv(
  hd_matrix = hd_draws,
  file_path = "Output/1e_historical_decomposition_means.csv",
  dates     = bvar_data$date
)

# 1. Load dataset
df <- read_csv("Output/1e_historical_decomposition_means.csv")

# 2. Reshape data into long format for ggplot stacked bars
df_long <- df %>%
  filter(target_variable == "gdp_growth") %>%
  pivot_longer(
    cols = starts_with("shock_"),
    names_to = "shock_name",
    values_to = "contribution"
  ) %>%
  mutate(shock_name = str_remove(shock_name, "shock_"))

# 3. Plot Historical Decomposition
ggplot(df_long, aes(x = date)) +
  geom_col(aes(y = contribution, fill = shock_name), position = "stack", alpha = 0.85) +
  geom_line(aes(y = total_decomposed_mean, color = "Total Decomposed Mean"), size = 1) +
  scale_color_manual(values = c("Total Decomposed Mean" = "black")) +
  theme_minimal() +
  labs(
    title = "Historical Decomposition: GDP Growth",
    x = "Time Period (t)",
    y = "Contribution",
    fill = "Structural Shock",
    color = ""
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )





# Run historical decomposition safely
hd_results <- compute_hd_bsvarSIGN(
  posterior_obj = run_bsvar_sign_model_01,
  batch_size    = 200
)


hd_results <- compute_hd_bsvarSIGN(
  posterior_obj = run_bsvar_sign_model_01,
  batch_size    = 200
)



# Export to CSV
save_hd_batched_to_csv(
  hd_results, 
  file_path = "Analysis/BVAR_Sign/Model_01/hd_summary_results.csv"
)


# Run streaming HD computation
hd_results <- compute_hd_bsvarSIGN(
  posterior_obj = run_bsvar_sign_model_01,
  batch_size    = 200
)

# Export directly to CSV
save_hd_batched_to_csv(
  hd_results, 
  file_path = "Analysis/BVAR_Sign/Model_01/hd_summary_results.csv"
)











#============================================================================#
#                              [6] IRF
#============================================================================#

Sys.setenv(OMP_NUM_THREADS = "1")

irf_updated <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = H_total
)


irf_updated <- compute_impulse_responses(
  run_bsvar_sign_model_02,
  horizon = 30
)


str(irf_updated, max.level = 2)
object.size(irf_updated)
class(irf_updated)

hist <- compute_historical_decompositions(run_bsvar_sign_model_01)



fevd_cpp <- getNativeSymbolInfo(
  "_bsvarSIGNs_bsvarSIGNs_fevd",
  PACKAGE = "bsvarSIGNs"
)

fevd_cpp

fevd_cpp <- getNativeSymbolInfo(
  "_bsvarSIGNs_bsvarSIGNs_fevd",
  PACKAGE = "bsvarSIGNs"
)

fevd_cpp

irf_updated <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = H_total
)

# Export IRF Results to CSV
irf_df <- export_bvar_results_to_csv(
  bvar_array = irf_updated,
  var_names  = target_cols,
  file_name  = "Analysis/BVAR_Sign/Model_01/IRF_Summary_Results.csv"
)






irf_20 <- compute_impulse_responses(
  run_bsvar_sign_model_02,
  horizon = 20
)

dim(irf_20)
object.size(irf_20)


fevd_cpp_result <- .Call(
  fevd_cpp,
  irf_20
)


fevd_10000 <- compute_variance_decompositions(
  run_bsvar_sign_model_02,
  horizon = 19
)

# 1. Load IRF CSV data
irf_data <- read.csv("Analysis/BVAR_Sign/Model_01/IRF_Summary_Results.csv")


policy_irf <- irf_data %>%
  filter(Shock == "policy_rate") %>%
  mutate(Variable = factor(Variable, levels = target_cols))

# 3. Plot IRF with clean scales and zero baseline
ggplot(policy_irf, aes(x = Horizon)) +
  # 90% and 68% Credible Intervals
  geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "firebrick", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "firebrick", alpha = 0.4) +
  # Zero reference line to mark sign direction
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
  # Point estimates (Median and Mean)
  geom_line(aes(y = Median), color = "darkred", linewidth = 0.8) +
  geom_line(aes(y = Mean), color = "black", linetype = "dashed", linewidth = 0.5) +
  # Facet setup
  facet_wrap(~ Variable, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = c(1, seq(4, max(policy_irf$Horizon), by = 4))) +
  labs(
    title = "Impulse Response Functions: Policy Rate Shock",
    subtitle = "68% and 90% Posterior Credible Intervals",
    x = "Horizon (Quarters)",
    y = "Response",
    caption = "Solid line: Median | Dashed line: Mean | Gray line: Zero response"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9),
    panel.spacing = unit(1, "lines")
  )

#------------------------------------------------------------------------------#
# Policy Shock on Gdp Growth

# 1. Extract IRF data frame
df_gdp_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "gdp_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_gdp_policy,
  response_name = "GDP Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_GDP.png"
)

#------------------------------------------------------------------------------#
# Policy Shock on Consumption Growth

# 1. Extract IRF data frame
df_consumption_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "consumption_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_consumption_policy,
  response_name = "Consumption Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Consumption.png"
)

#------------------------------------------------------------------------------#
# Policy Shock on Inflation

# 1. Extract IRF data frame
df_cpi_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "cpi_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_cpi_policy,
  response_name = "CPI",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_CPI.png"
)

#------------------------------------------------------------------------------#
# Policy Shock on Savings Rate

# 1. Extract IRF data frame
df_saving_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "saving_rate", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_saving_policy,
  response_name = "Savings Rate",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Savings.png"
)

#------------------------------------------------------------------------------#
# Interest Burden

# 1. Extract IRF data frame
df_interest_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "interest_burden", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_interest_policy,
  response_name = "Interest Burden",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Interest.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on Debt Growth

# 1. Extract IRF data frame
df_debt_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "debt_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_debt_policy,
  response_name = "Debt Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Debt_Growth.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on House Price Growth

# 1. Extract IRF data frame
df_house_price_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "real_house_price_growth", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_house_price_policy,
  response_name = "Real House Price Growth",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Real_House_Growth.png"
)


#------------------------------------------------------------------------------#
# Policy Shock on Household's Wealth

# 1. Extract IRF data frame
df_wealth_policy <- get_irf_median_df(
  irf_obj      = irf_updated, 
  response_var = "asset_liability_ratio", 
  shock_var    = "policy_rate", 
  label_name   = "Baseline Model",
  target_cols  = target_cols
)

# 2. Render and save PNG
plot_single_irf(
  irf_df        = df_wealth_policy,
  response_name = "Asset to Liability Ratio",
  shock_name    = "Monetary Policy Shock",
  output_path   = "Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_on_Wealth.png"
)


#============================================================================#
#                              [7] FEVD
#============================================================================#

# Simply pass 'irf_updated' directly into the function
fevd_updated <- compute_fevd_from_irf(irf_updated)

# Export FEVD Results to CSV
fevd_df <- export_bvar_results_to_csv(
  bvar_array = fevd_updated,
  var_names  = target_cols,
  file_name  = "Analysis/BVAR_Sign/Model_01/FEVD_Summary_Results.csv"
)

# 1. Load data
fevd_data <- read.csv("Analysis/BVAR_Sign/Model_01/FEVD_Summary_Results.csv")


policy_data <- fevd_data %>%
  filter(Shock == "policy_rate") %>%
  mutate(Variable = factor(Variable, levels = target_cols))

# 3. Plot with clean X-axis breaks
ggplot(policy_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "firebrick", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "firebrick", alpha = 0.4) +
  geom_line(aes(y = Median), color = "darkred", linewidth = 0.8) +
  geom_line(aes(y = Mean), color = "black", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ Variable, ncol = 3, scales = "free_y") +
  # Format Y-axis percentages cleanly
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Fix X-axis overlap by showing ticks every 4 periods (e.g., 1, 4, 8, 12, 16, 20)
  scale_x_continuous(breaks = c(1, seq(4, max(policy_data$Horizon), by = 4))) +
  labs(
    title = "FEVD: Response of All 9 Variables to Policy Rate Shock",
    subtitle = "68% and 90% Credible Intervals",
    x = "Horizon (Quarters)",
    y = "Variance Share",
    caption = "Solid line: Median | Dashed line: Mean"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9),
    panel.spacing = unit(1, "lines") # Adds breathing room between facets
  )








  # 1. Load data
df <- read_csv("hd_summary_results.csv")

# 2. Set metric ('mean' or 'median') and target variable
metric <- "mean" 
target_var <- "Var_1"

# 3. Reshape shock data into long format for ggplot
df_long <- df %>%
  filter(target_variable == target_var) %>%
  select(date, matches(paste0("^shock_.*_", metric, "$")), paste0("total_decomposed_", metric)) %>%
  pivot_longer(
    cols = starts_with("shock_"),
    names_to = "shock",
    values_to = "value"
  ) %>%
  mutate(
    # Clean shock names for legend (e.g., 'shock_Shock_1_mean' -> 'Shock 1')
    shock = str_replace(shock, "shock_", ""),
    shock = str_replace(shock, paste0("_", metric), ""),
    shock = str_replace_all(shock, "_", " ")
  )

# Dynamically name the total column for plotting
total_col_name <- paste0("total_decomposed_", metric)

# 4. Plot historical decomposition
ggplot() +
  # Stacked bars for shock contributions
  geom_col(
    data = df_long,
    aes(x = date, y = value, fill = shock),
    position = "stack",
    width = 0.8
  ) +
  # Black line for total decomposed series
  geom_line(
    data = filter(df, target_variable == target_var),
    aes(x = date, y = .data[[total_col_name]], color = "Total Decomposed"),
    size = 1
  ) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) +
  # Color & Aesthetic formatting
  scale_fill_brewer(palette = "Set3", name = "Shocks") +
  scale_color_manual(values = c("Total Decomposed" = "black"), name = "") +
  labs(
    title = paste("Historical Decomposition for", target_var),
    subtitle = paste("Metric:", str_to_title(metric)),
    x = "Date",
    y = "Contribution"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

















library(tidyverse)

# 1. Load data
df <- read_csv("hd_summary_results.csv")

# 2. Set metric ('mean' or 'median') and target variable
metric <- "mean" 
target_var <- "Var_1"

# 3. Reshape shock data into long format for ggplot
df_long <- df %>%
  filter(target_variable == target_var) %>%
  select(date, matches(paste0("^shock_.*_", metric, "$")), paste0("total_decomposed_", metric)) %>%
  pivot_longer(
    cols = starts_with("shock_"),
    names_to = "shock",
    values_to = "value"
  ) %>%
  mutate(
    # Clean shock names for legend (e.g., 'shock_Shock_1_mean' -> 'Shock 1')
    shock = str_replace(shock, "shock_", ""),
    shock = str_replace(shock, paste0("_", metric), ""),
    shock = str_replace_all(shock, "_", " ")
  )

# Dynamically name the total column for plotting
total_col_name <- paste0("total_decomposed_", metric)


  # 4. Plot historical decomposition
ggplot() +
  # Stacked bars for shock contributions
  geom_col(
    data = df_long,
    aes(x = date, y = value, fill = shock),
    position = "stack",
    width = 0.8
  ) +
  # Black line for total decomposed series
  geom_line(
    data = filter(df, target_variable == target_var),
    aes(x = date, y = .data[[total_col_name]], color = "Total Decomposed"),
    linewidth = 1
  ) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  # Color & Aesthetic formatting
  scale_fill_brewer(palette = "Set3", name = "Shocks") +
  scale_color_manual(values = c("Total Decomposed" = "black"), name = "") +
  labs(
    title = paste("Historical Decomposition for", target_var),
    subtitle = paste("Metric:", str_to_title(metric)),
    x = "Date",
    y = "Contribution"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
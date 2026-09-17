###############################################################################
############################# CONSUMPTION HABITUAL ############################
###############################################################################
# 1. DATA

bvar_data <- read.csv("2b_seasonally_adjusted_data.csv")

model_ts <- ts(
  bvar_data,
  start = c(1996, 1),
  frequency = 4
)

# ============================================================
# 2. VARIABLES
# ============================================================

domestic_cols <- c(
  "gdp_se_log",
  "unemployment_rate",
  "cpi_log",
  "policy_rate",
  "cons_habitual_log",
  "debt_to_asset_ratio",
  "dsr_value",
  "liquid_asset_to_income_ratio",
  "saving_rate"
)
domestic_data <- as.matrix(model_ts[, domestic_cols])

N_dom <- ncol(domestic_data)


# ============================================================
# 4. INDICES
# ============================================================

gdp_idx <- "gdp_se_log"
unemp_idx <- "unemployment_rate"
cpi_idx <- "cpi_log"
policy_idx <- "policy_rate"
cons_idx <- "cons_habitual_log"
dta_idx <- "debt_to_asset_ratio"
dsr_idx <- "dsr_value"
liquid_idx <- "liquid_asset_to_income_ratio"
saving_idx <- "saving_rate"

c(
  gdp = gdp_idx,
  unemployment = unemp_idx,
  cpi = cpi_idx,
  policy = policy_idx,
  consumption = cons_idx,
  dta = dta_idx,
  dsr = dsr_idx,
  liquid = liquid_idx,
  saving = saving_idx
)


# ============================================================
# 5. SIGN RESTRICTIONS
# ============================================================

H_total <- 10
N_dom   <- length(domestic_cols)  # 9

# Create array with named dimensions
sign_irf <- array(
  NA_real_,
  dim = c(N_dom, N_dom, H_total),
  dimnames = list(
    variable = domestic_cols,  # Rows
    shock    = domestic_cols,  # Columns (or paste0("shock_", 1:N_dom))
    horizon  = NULL
  )
)

# Now character indexing works cleanly!
mp_shock    <- 1  # or "policy_rate"
macro_shock <- 2


# Constrain restrictions for horizons 1 through 3 (Quarters 0, 1, 2)
for (h in 1:3) {
  # 1. Monetary Policy Shock (Rate UP, GDP DOWN, CPI DOWN, Unemployment UP)
  sign_irf[policy_idx, mp_shock, h] <-  1
  sign_irf[gdp_idx,    mp_shock, h] <- -1
  sign_irf[cpi_idx,    mp_shock, h] <- -1
  sign_irf[unemp_idx,  mp_shock, h] <-  1

  # 2. Adverse Macro / Demand Shock (GDP DOWN, Unemployment UP, Policy Rate DOWN)
  sign_irf[gdp_idx,    macro_shock, h] <- -1
  sign_irf[unemp_idx,  macro_shock, h] <-  1
  sign_irf[policy_idx, macro_shock, h] <- -1  # Endogenous rate cut response
}


is_stationary <- c(  
  # Domestic Variables (9)
  TRUE,  # gdp_se_log (Non-Stationary I(1))
  TRUE, # unemployment_rate (Stationary I(0) based on ADF/KPSS drift spec)
  TRUE,  # cpi_log (Non-Stationary I(1))
  TRUE,  # policy_rate (Non-Stationary I(1))
  TRUE,  # target_cons [cons_habitual_log / cons_durable_log] (Non-Stationary I(1))
  TRUE,  # debt_to_asset_ratio (Non-Stationary I(1))
  TRUE,  # dsr_value (Non-Stationary / Conflicting I(1))
  TRUE,  # liquid_asset_to_income_ratio (Non-Stationary I(1))
  TRUE   # saving_rate (Non-Stationary I(1))
)

# quoted directly from package
#"stationary an N logical vector - its element set to FALSE sets the prior mean for the autoregressive
#parameters of the Nth equation to the white noise process, otherwise to random walk."

# ============================================================
# 4. SPECIFY AND ESTIMATE MODEL (p = 2)
# ============================================================
p <- 2
raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
effective_covid_idx <- raw_covid_idx - p

spec_baseline <- specify_bsvarSIGN$new(
  data         = domestic_data,          # 9 domestic variables
  p            = p,                      # p = 2 lags
  sign_irf     = sign_irf,               # 13 x 13 x 20 array
  stationary   = is_stationary,          # Length 13 vector
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  # Kept FALSE for numeric stability
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = effective_covid_idx,   # Lenza & Primiceri scaling
  mc.cores     = 1
)

set.seed(12345)


set.seed(12345)

cat("Estimating hyperparameters...\n")
spec_baseline$estimate_hyper(S = 5000, burn_in = 1000)

cat("Running Gibbs sampler estimation...\n")
posterior_habitual <- estimate(spec_baseline, S = 4000, thin = 1)





rho <- check_posterior_stability(posterior_obj, p)

mean(rho < 1)
quantile(rho, c(.01, .05, .50, .95, .99))
max(rho)




library(dplyr)
library(ggplot2)

irf_list <- list()
counter  <- 1

# Extract using the existing irf_sum object
for (s_idx in 1:length(irf_sum)) {
  s_name   <- names(irf_sum)[s_idx]
  var_list <- irf_sum[[s_idx]]
  
  for (v_idx in 1:length(var_list)) {
    v_name <- domestic_cols[v_idx]
    mat    <- var_list[[v_idx]]
    df     <- as.data.frame(mat)
    
    # Col 1 = mean, Col 2 = sd
    # Construct 68% credible interval (Mean +/- 1 SD) so median sits dead center
    df_clean <- data.frame(
      horizon  = 0:(nrow(df) - 1),
      shock    = s_name,
      variable = v_name,
      lower    = df[, 1] - df[, 2], # 16th percentile equivalent
      median   = df[, 1],          # Central estimate (Mean/Median)
      upper    = df[, 1] + df[, 2]  # 84th percentile equivalent
    )
    
    irf_list[[counter]] <- df_clean
    counter <- counter + 1
  }
}

# 1. Combine flat list into single data frame
irf_df <- bind_rows(irf_list)

# 2. Filter for identified shocks and standardize labels
irf_two_shocks <- irf_df %>% 
  filter(shock %in% c("shock1", "shock2")) %>% 
  mutate(
    shock_label = factor(
      ifelse(shock == "shock1", "Monetary Policy Shock", "Adverse Macro Shock"),
      levels = c("Monetary Policy Shock", "Adverse Macro Shock")
    ),
    variable = factor(variable, levels = domestic_cols)
  )

# 3. Plot clean light theme grid
p_irf <- ggplot(irf_two_shocks, aes(x = horizon, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "#2B6CB0") +
  geom_line(color = "#1A365D", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C53030", linewidth = 0.6) +
  facet_grid(variable ~ shock_label, scales = "free_y") +
  labs(
    title = "Impulse Response Functions (Swedish Economy)",
    subtitle = "68% Credible Intervals (Mean ± 1 SD) | Pure 9-Variable Domestic System",
    x = "Horizon (Quarters)",
    y = "Response"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#EDF2F7"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 4. Save plot
ggsave("IRF_2shocks_fixed.png", plot = p_irf, width = 11, height = 13, dpi = 300)
cat("Plot successfully regenerated and saved to IRF_2shocks_fixed.png\n")





raw_covid_idx <- 








# 1. Adverse Macro Shock (Shock 2) in 2008Q4 is strictly negative
narrative_gfc <- specify_narrative(
  start   = which(bvar_data$quarter == "2008Q4"),
  periods = 1,
  type    = "S",
  sign    = -1,
  shock   = 2,     # Shock 2: Adverse Macro Shock
  var     = NA     # NA because type = "S" (applies to structural shock directly)
)

# 2. Monetary Policy Shock (Shock 1) in 2022Q2 is strictly positive
narrative_mp <- specify_narrative(
  start   = which(bvar_data$quarter == "2022Q2"),
  periods = 1,
  type    = "S",
  sign    = 1,
  shock   = 1,     # Shock 1: Monetary Policy Shock
  var     = NA     # NA for type = "S"
)

# Combine narratives into a list
narrative_list <- list(narrative_gfc, narrative_mp)





# Specify the model with sign AND narrative restrictions
spec_narrative <- specify_bsvarSIGN$new(
  data           = domestic_data,
  p              = 2,
  sign_irf       = sign_irf,             # Your existing sign restrictions matrix
  sign_narrative = narrative_list,       # Pass narrative restrictions here
  hyper_lambda   = TRUE
)

# Run MCMC sampler
posterior_narrative <- estimate(spec_narrative, S = 5000)

# Compute IRFs from the narrative-constrained posterior
irf_draws_narrative <- compute_impulse_responses(posterior_narrative, horizon = 20)
irf_sum_narrative   <- summary(irf_draws_narrative)



posterior_test <- estimate(spec_narrative, S = 100, show_progress = TRUE)




# Compute IRFs from the narrative-constrained posterior
irf_draws_narrative <- compute_impulse_responses(posterior_test, horizon = 20)
irf_sum_narrative   <- summary(irf_draws_narrative)






irf_list <- list()
counter  <- 1

for (s_idx in 1:length(irf_sum_narrative)) {
  s_name   <- names(irf_sum_narrative)[s_idx]
  var_list <- irf_sum_narrative[[s_idx]]
  
  for (v_idx in 1:length(var_list)) {
    v_name <- domestic_cols[v_idx]
    mat    <- var_list[[v_idx]]
    df     <- as.data.frame(mat)
    
    # Col 1 = mean, Col 2 = sd
    df_clean <- data.frame(
      horizon  = 0:(nrow(df) - 1),
      shock    = s_name,
      variable = v_name,
      lower    = df[, 1] - df[, 2], # Mean - 1 SD
      median   = df[, 1],          # Mean / Central estimate
      upper    = df[, 1] + df[, 2]  # Mean + 1 SD
    )
    
    irf_list[[counter]] <- df_clean
    counter <- counter + 1
  }
}

irf_narrative_df <- bind_rows(irf_list) %>% 
  filter(shock %in% c("shock1", "shock2")) %>% 
  mutate(
    shock_label = factor(
      ifelse(shock == "shock1", "Monetary Policy Shock", "Adverse Macro Shock"),
      levels = c("Monetary Policy Shock", "Adverse Macro Shock")
    ),
    variable = factor(variable, levels = domestic_cols)
  )

p_narrative <- ggplot(irf_narrative_df, aes(x = horizon, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "#2B6CB0") +
  geom_line(color = "#1A365D", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C53030", linewidth = 0.6) +
  facet_grid(variable ~ shock_label, scales = "free_y") +
  labs(
    title = "Narrative-Constrained Impulse Response Functions (Sweden)",
    subtitle = "68% Credible Intervals (Mean ± 1 SD) | Native Narrative & Sign Restrictions",
    x = "Horizon (Quarters)",
    y = "Response"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#EDF2F7"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("IRF_narrative_native.png", plot = p_narrative, width = 11, height = 13, dpi = 300)
cat("Native Narrative IRF plot saved as 'IRF_narrative_native.png'\n")
















# Final Production Run with S = 500 Draws
posterior_narrative <- estimate(spec_narrative, S = 500, show_progress = TRUE)

# Compute IRFs and extract summary
irf_draws_narrative <- compute_impulse_responses(posterior_narrative, horizon = 20)
irf_sum_narrative   <- summary(irf_draws_narrative)

# Process dataframe
irf_list <- list()
counter  <- 1

for (s_idx in 1:length(irf_sum_narrative)) {
  s_name   <- names(irf_sum_narrative)[s_idx]
  var_list <- irf_sum_narrative[[s_idx]]
  
  for (v_idx in 1:length(var_list)) {
    v_name <- domestic_cols[v_idx]
    mat    <- var_list[[v_idx]]
    df     <- as.data.frame(mat)
    
    # Col 1 = Mean, Col 2 = SD
    df_clean <- data.frame(
      horizon  = 0:(nrow(df) - 1),
      shock    = s_name,
      variable = v_name,
      lower    = df[, 1] - df[, 2],
      median   = df[, 1],
      upper    = df[, 1] + df[, 2]
    )
    
    irf_list[[counter]] <- df_clean
    counter <- counter + 1
  }
}

irf_narrative_df <- bind_rows(irf_list) %>% 
  filter(shock %in% c("shock1", "shock2")) %>% 
  mutate(
    shock_label = factor(
      ifelse(shock == "shock1", "Monetary Policy Shock", "Adverse Macro Shock"),
      levels = c("Monetary Policy Shock", "Adverse Macro Shock")
    ),
    variable = factor(variable, levels = domestic_cols)
  )

# Final High-Res Plot
p_narrative_final <- ggplot(irf_narrative_df, aes(x = horizon, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "#2B6CB0") +
  geom_line(color = "#1A365D", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C53030", linewidth = 0.6) +
  facet_grid(variable ~ shock_label, scales = "free_y") +
  labs(
    title = "Narrative-Constrained Impulse Response Functions (Sweden)",
    subtitle = "68% Credible Intervals (Mean ± 1 SD) | Joint 9-Variable Domestic System",
    x = "Horizon (Quarters)",
    y = "Response"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#EDF2F7"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("IRF_FINAL_narrative_500.png", plot = p_narrative_final, width = 11, height = 13, dpi = 300)
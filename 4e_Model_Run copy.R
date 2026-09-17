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

foreign_cols <- c(
  "fed_funds_rate",
  "kix_gdp_log",
  "kix_real_log"
)

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

foreign_data  <- as.matrix(model_ts[, foreign_cols])
domestic_data <- as.matrix(model_ts[, domestic_cols])

N_for <- ncol(foreign_data)
N_dom <- ncol(domestic_data)
N_sys <- N_for + N_dom


# ============================================================
# 4. INDICES
# ============================================================

gdp_idx <- N_for + which(domestic_cols == "gdp_se_log")
unemp_idx <- N_for + which(domestic_cols == "unemployment_rate")
cpi_idx <- N_for + which(domestic_cols == "cpi_log")
policy_idx <- N_for + which(domestic_cols == "policy_rate")
cons_idx <- N_for + which(domestic_cols == "cons_habitual_log")
dta_idx <- N_for + which(domestic_cols == "debt_to_asset_ratio")
dsr_idx <- N_for + which(domestic_cols == "dsr_value")
liquid_idx <- N_for + which(
  domestic_cols == "liquid_asset_to_income_ratio"
)
saving_idx <- N_for + which(domestic_cols == "saving_rate")

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

sign_irf <- array(
  NA_real_,
  dim = c(N_sys, N_sys, H_total)
)


# Monetary policy shock
mp_shock    <- 1
demand_shock <- 2

# --- Shock 1: Monetary Policy Shock (Tightening) ---
sign_irf[policy_idx, mp_shock, 1] <-  1  # Policy rate rises
sign_irf[gdp_idx,    mp_shock, 1] <- -1  # Output drops
sign_irf[cpi_idx,    mp_shock, 1] <- -1  # Inflation drops
sign_irf[unemp_idx,  mp_shock, 1] <-  1  # Unemployment rises

# --- Shock 2: Adverse Macro Demand Shock ---
sign_irf[gdp_idx,    demand_shock, 1] <- -1  # Output drops
sign_irf[unemp_idx,  demand_shock, 1] <-  1  # Unemployment rises
sign_irf[policy_idx, demand_shock, 1] <- -1  # Policy rate CUTS (Separating sign!)
sign_irf[cpi_idx,    demand_shock, 1] <- -1  # Inflation drops


is_stationary <- c(
  # Foreign Variables (4)
  TRUE,  # fed_funds_rate (Non-Stationary I(1))
  TRUE,  # kix_gdp_log (Non-Stationary I(1))
  FALSE, # kix_real_log (Stationary I(0))
  
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
p <- 1
raw_covid_idx <- which(bvar_data$quarter == "2020Q2")
effective_covid_idx <- raw_covid_idx - p

spec_baseline <- specify_bsvarSIGN$new(
  data         = domestic_data,          # 9 domestic variables
  p            = p,                      # p = 2 lags
  sign_irf     = sign_irf,               # 13 x 13 x 20 array
  foreign      = foreign_data,           # 4 foreign variables
  stationary   = is_stationary,          # Length 13 vector
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  # Kept FALSE for numeric stability
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = effective_covid_idx,   # Lenza & Primiceri scaling
  mc.cores     = 1
)


set.seed(12345)


spec_baseline$estimate_hyper(S = 5000, burn_in = 1000)

posterior_habitual <- estimate(spec_baseline, S = 4000, thin = 1)









spec_baseline$estimate_hyper(S = 20000, burn_in = 1000)


cat(r"(
Things will take a while from now. Go, grab coffee or something.
The model should be saved and run by the time you come back.

      .-~~-.
    ,|`-__-'|
    ||      |     COFFEE OR TEA
    `|      |
      `-__-'

Saving model I found was a good way to prevent me from rerunning things.
It also allowed to move models to other script, which is useful for diagnostics
when comparing multiple models.
)")

cat("Running Gibbs sampler estimation...\n")
posterior_habitual <- estimate(spec_baseline, S = 18000, thin = 1)




Q <- posterior_habitual$posterior$Q

Q_signature <- apply(
  matrix(Q, nrow = N_sys * N_sys),
  2,
  paste0,
  collapse = "|"
)

length(unique(Q_signature))




2


# 1. Compute impulse responses
irf_draws <- compute_impulse_responses(posterior_habitual, horizon = 20)

# 2. Get the summary list (68% credible intervals)
irf_sum <- summary(irf_draws, probs = c(0.16, 0.5, 0.84))

# 3. Bind the list into a single flat data frame
irf_df <- bind_rows(
  lapply(names(irf_sum), function(s_name) {
    bind_rows(
      lapply(names(irf_sum[[s_name]]), function(v_name) {
        df <- as.data.frame(irf_sum[[s_name]][[v_name]])
        df$shock    <- s_name
        df$variable <- v_name
        df$horizon  <- 0:(nrow(df) - 1)
        return(df)
      })
    )
  })
)

# Check column names of the new flat data frame
head(irf_df)





plot(irf_draws)



pdf("IRF_plots.png", width = 12, height = 10)
plot(irf_draws)
dev.off()
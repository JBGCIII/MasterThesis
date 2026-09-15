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

mp_shock <- 1
macro_shock <- 2

# Monetary policy shock
sign_irf[policy_idx, mp_shock, 1] <-  1
sign_irf[gdp_idx,    mp_shock, 1] <- -1
sign_irf[cpi_idx,    mp_shock, 1] <- -1
sign_irf[unemp_idx,  mp_shock, 1] <-  1

# Adverse macro shock
sign_irf[gdp_idx,    macro_shock, 1] <- -1
sign_irf[unemp_idx,  macro_shock, 1] <-  1


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






posterior_habitual$posterior$ess





posterior_habitual$posterior$ess
str(posterior_habitual$posterior)
names(posterior_habitual$posterior)



 coda::effectiveSize(coda::as.mcmc(posterior_lambda))


summary(posterior_habitual)




# Extract posterior draws of A
draws_A <- posterior_habitual$posterior$A

# Reshape from a multi-way array to a 2D matrix (Iterations x Parameters)
# Assuming dimensions are [equations, coefficients, iterations]
dim_A   <- dim(draws_A)
mat_A   <- matrix(draws_A, nrow = dim_A[3], ncol = dim_A[1] * dim_A[2])

# Now coda can compute the ESS for every coefficient
ess_A   <- coda::effectiveSize(coda::as.mcmc(mat_A))
summary(ess_A) # Inspect min, mean, and max ESS





class(posterior_habitual)
class(posterior_habitual$posterior)
methods(class = class(posterior_habitual))
methods(class = class(posterior_habitual$posterior))
posterior_habitual$posterior$ess


getAnywhere("estimate")


system.file(package = "bsvarSIGNs")


grep(
  "ess",
  list.files(
    system.file(package = "bsvarSIGNs"),
    recursive = TRUE,
    full.names = TRUE
  ),
  value = TRUE
)

# 1. Inspect the internal structure
str(posterior_habitual$posterior$hyper)

# 2. Check row names or variable names (if it's a matrix or data frame)
rownames(posterior_habitual$posterior$hyper)

# 3. Check column names (if parameters are saved across columns)
colnames(posterior_habitual$posterior$hyper)

# 4. Check element names (if hyper is a list)
names(posterior_habitual$posterior$hyper)


prior$nu

prior <- spec_baseline$get_prior()
names(prior)
prior$S


library(coda)

# 1. Extract posterior draws (e.g., structural matrix B or VAR coefficients A)
# Dimensions are usually: [parameters x draws]
draws_mat <- t(fit_signs$posterior$B) 

# 2. Convert to an mcmc object
mcmc_draws <- as.mcmc(draws_mat)

# 3. Run diagnostics instantly
geweke.diag(mcmc_draws)        # Geweke z-scores
effectiveSize(mcmc_draws)      # Effective Sample Size (ESS)
raftery.diag(mcmc_draws)       # Sample size requirements


library(bayesplot)

# Convert transposed draws to matrix
draws_mat <- t(fit_signs$posterior$B) 

# Plot MCMC trace plots or overlays
mcmc_trace(draws_mat)
mcmc_dens_overlay(draws_mat)
mcmc_acf(draws_mat)







post <- posterior_habitual$posterior

dim(post$A)
dim(post$B)
dim(post$Q)
dim(post$Sigma)
dim(post$Theta0)
dim(post$shocks)
dim(post$sigma)

post$ess


compute_impulse_responses


getAnywhere(compute_impulse_responses)

?compute_impulse_responses



irf_habitual <- compute_impulse_responses(
  posterior_habitual,
  horizon = 20,
  standardise = FALSE
)


class(irf_habitual)
dim(irf_habitual)
attributes(irf_habitual)



mp_impact <- irf_habitual[, mp_shock, 1, ]

summary(mp_impact[policy_idx, ])
summary(mp_impact[gdp_idx, ])
summary(mp_impact[cpi_idx, ])
summary(mp_impact[unemp_idx, ])


c(
  policy_positive = mean(mp_impact[policy_idx, ] > 0),
  gdp_negative    = mean(mp_impact[gdp_idx, ] < 0),
  cpi_negative    = mean(mp_impact[cpi_idx, ] < 0),
  unemp_positive  = mean(mp_impact[unemp_idx, ] > 0)
)



c(
  unique_policy = length(unique(mp_impact[policy_idx, ])),
  unique_gdp    = length(unique(mp_impact[gdp_idx, ])),
  unique_cpi    = length(unique(mp_impact[cpi_idx, ])),
  unique_unemp  = length(unique(mp_impact[unemp_idx, ]))

  
)


table(round(mp_impact[policy_idx, ], 8))[1:10]

mp_impact <- irf_habitual[, mp_shock, 1, ]



c(
  unique_B = length(unique(as.vector(post$B))),
  unique_Q = length(unique(as.vector(post$Q))),
  unique_Theta0 = length(unique(as.vector(post$Theta0)))
)



unique_counts_Q <- matrix(
  NA_integer_,
  nrow = N_sys,
  ncol = N_sys
)

for (i in 1:N_sys) {
  for (j in 1:N_sys) {
    unique_counts_Q[i, j] <- length(unique(post$Q[i, j, ]))
  }
}

unique_counts_Q



unique_counts_B <- matrix(
  NA_integer_,
  nrow = N_sys,
  ncol = N_sys
)

for (i in 1:N_sys) {
  for (j in 1:N_sys) {
    unique_counts_B[i, j] <- length(unique(post$B[i, j, ]))
  }
}

summary(as.vector(unique_counts_Q))
summary(as.vector(unique_counts_B))



unique(post$Q[1, 1, ])
unique(post$Q[policy_idx, mp_shock, ])


length(unique(post$Q[1, 1, ]))
length(unique(post$Q[policy_idx, mp_shock, ]))









Q <- post$Q

Q_signature <- apply(
  matrix(Q, nrow = N_sys * N_sys),
  2,
  paste0,
  collapse = "|"
)

length(unique(Q_signature))



Q_id <- match(Q_signature, unique(Q_signature))

table(Q_id)

sort(table(Q_id), decreasing = TRUE)



rotation_policy_impact <- tapply(
  mp_impact[policy_idx, ],
  Q_id,
  unique
)

rotation_policy_impact



rotation_frequency <- sort(table(Q_id), decreasing = TRUE)

data.frame(
  rotation = as.integer(names(rotation_frequency)),
  draws = as.integer(rotation_frequency),
  share = as.integer(rotation_frequency) / length(Q_id)
)









for (v in names(household_vars)) {
  
  i <- household_vars[[v]]
  
  vals <- tapply(
    mp_irf[i, 1, ],
    Q_id,
    unique
  )
  
  cat("\n", v, "\n")
  print(round(unlist(vals), 6))
}
###############################################################################
########################### 2. MODELS DIAGNOSTIC  #############################
###############################################################################


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


dir.create("4_Output_Analysis/Model_Baseline/Durable", recursive = TRUE, showWarnings = FALSE)
dir.create("4_Output_Analysis/Model_Baseline/Habitual", recursive = TRUE, showWarnings = FALSE)
dir.create("4_Output_Analysis/Model_Foreign/Durable", recursive = TRUE, showWarnings = FALSE)
dir.create("4_Output_Analysis/Model_Foreign/Habitual", recursive = TRUE, showWarnings = FALSE)
dir.create("4_Output_Analysis/Model_Narrative/Durable", recursive = TRUE, showWarnings = FALSE)
dir.create("4_Output_Analysis/Model_Narrative/Habitual", recursive = TRUE, showWarnings = FALSE)


#=============================================================================#
#                          [1]  MODEL BASELINE DURABLE

variables_durables <- c("GDP-D", "Unemployment", "CPIF", "Interest", "Durables", "DTA", "DSR", "LAI", "Savings")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Baseline/Durable/baseline_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Baseline/Durable",
    filename = paste0("IRF_Baseline_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Baseline Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Baseline/Durable",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                          [2]  MODEL BASELINE HABITUAL

variables_habituals <- c("GDP-D", "Unemployment", "CPIF", "Interest", "Habituals", "DTA", "DSR", "LAI", "Savings")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Baseline/Habitual/baseline_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habituals,
    output_dir = "4_Output_Analysis/Model_Baseline/Habitual",
    filename = paste0("IRF_Baseline_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Baseline Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habituals,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Baseline/Habitual",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}







#=============================================================================#
#                          [3]  MODEL FOREIGN DURABLE




variables_durables <- c("FED Rate", "KIX GDP", "KIX CPI", "GDP-D", "Unemployment", "CPIF", "Interest", "Durables", "DTA", "DSR", "LAI", "Savings", "Exchange")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Foreign/Durable/foreign_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Foreign/Durable",
    filename = paste0("IRF_Foreign_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(7, 4),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Baseline Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Foreign/Durable",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}








spec_narrative_one_durable <- specify_bsvarSIGN$new(
  data         = domestic_data,          
  p            = 1,                     
  sign_irf     = sign_irf,               
  stationary   = is_stationary, 
  sign_narrative = narrative_list,          
  hyper_lambda = TRUE,
  hyper_mu     = TRUE,                  
  hyper_delta  = TRUE,
  hyper_psi    = FALSE,
  hyper_covid  = raw_covid_idx -1,   # Lenza & Primiceri scaling
  mc.cores     = n_cores
)






###############################################################################
################################## 5. IRF'S PLOT ##############################
###############################################################################

dirs <- c(outer(
  c("Model_Baseline", "Model_Foreign", "Model_Narrative", "Model_Housing"),
  c("Durable", "Habitual"),
  function(m, type) file.path("4_Output_Analysis", m, type)
))

sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)


#=============================================================================#
#                          [1]  MODEL HOUSING (DURABLE)

variables_durables <- c("Unemployment (pp)", "Interest (pp)", "Real House Price (Log)", 
                       "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "Durables (Log)")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Housing/Durable/housing_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Housing/Durable",
    filename = paste0("IRF_Housing_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(2, 1, 3),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    ),
    title = paste0(
      "Housing Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Durable Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table_test(
    irf_object = irf,
    variable_names = variables_durables,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Housing/Durable",
      paste0("IRF_Housing_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}



#=============================================================================#
#                          [2]  MODEL HOUSING (HABITUAL)

variables_habitual  <- c("Unemployment (pp)", "Interest (pp)", "Real House Price (Log)", 
                       "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "Habituals (Log)")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Housing/Habitual/housing_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habitual,
    output_dir = "4_Output_Analysis/Model_Housing/Habitual",
    filename = paste0("IRF_Housing_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(2, 1, 3),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    ),
    title = paste0(
      "Housing Identification: Impulse Response Functions (p = ", p, ")"
    ),
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habitual,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock",
      "Asset Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Housing/Habitual",
      paste0("IRF_Housing_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                          [3]  MODEL MACRO (DURABLE)

variables_durables <- c("GDP-D (log)", "Unemployment (pp)", "CPIF (log)", "Interest (pp)", "Durables (log)",
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")


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
    shock_indices = c(4, 1),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Macro Identification: Impulse Response Functions (p = ", p, ")"
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
#                          [4]  MODEL MACRO (HABITUAL)

variables_habituals  <- c("GDP-D (log)", "Unemployment (pp)", "CPIF (log)", "Interest (pp)", "Habituals (log)",
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")



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
    shock_indices = c(4, 1),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Macro Identification: Impulse Response Functions (p = ", p, ")"
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
#                       [5]  MODEL MACRO DURABLE (NARRATIVE)


variables_durables <- c("GDP-D (log)", "Unemployment (pp)", "CPIF (log)", "Interest (pp)", "Durables (log)",
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")


for (p in 1:2) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Narrative/Durable/narrative_durable_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_durables,
    output_dir = "4_Output_Analysis/Model_Narrative/Durable",
    filename = paste0("IRF_Narrative_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2), # Note: Model was estimated on a different indices
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Narrative Identification: Impulse Response Functions (p = ", p, ")"
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
      "4_Output_Analysis/Model_Narrative/Durable",
      paste0("IRF_Narrative_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                       [6]  MODEL MACRO DURABLE (NARRATIVE)

variables_habituals  <- c("GDP-D (log)", "Unemployment (pp)", "CPIF (log)", "Interest (pp)", "Habituals (log)",
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")



for (p in 1:2) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Narrative/Habitual/narrative_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habituals,
    output_dir = "4_Output_Analysis/Model_Narrative/Habitual",
    filename = paste0("IRF_Narrative_Lag_", p, ".png"),
    horizon = 30,
    shock_indices = c(1, 2),
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    ),
    title = paste0(
      "Narrative Identification: Impulse Response Functions (p = ", p, ")"
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
      "4_Output_Analysis/Model_Narrative/Habitual",
      paste0("IRF_Narrative_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}


#=============================================================================#
#                       [7]  MODEL MACRO SMALL OPEN ECONOMY (DURABLE)
variables_durables <- c("FED Rate (pp)", "KIX GDP (Log)", "KIX CPI (Log)",
 "GDP-D (Log)", "Unemployment (pp)", "CPIF (Log)", "Interest (pp)", "Durables (log)", 
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")


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
    shock_indices = c(7, 4), # Very Important to change or you'll plot the wrong shocks!
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


#=============================================================================#
#                       [8]  MODEL MACRO SMALL OPEN ECONOMY (HABITUAL)


variables_habitual_foreign <- <- c("FED Rate (pp)", "KIX GDP (Log)", "KIX CPI (Log)",
 "GDP-D (Log)", "Unemployment (pp)", "CPIF (Log)", "Interest (pp)", "Habituals (log)", 
 "DTA (pp)", "DSR (pp)", "LAI (pp)", "Savings (pp)", "REER (log)")


for (p in 1:5) {

  model <- readRDS(
    paste0(
      "3_Model_Output/Model_Foreign/Habitual/foreign_habitual_p",
      p, ".rds"
    )
  )

  irf <- plot_irf_two_shocks(
    model = model,
    variable_names = variables_habitual_foreign,
    output_dir = "4_Output_Analysis/Model_Foreign/Habitual",
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
    system_label = "Habitual Goods System"
  )

  print(irf$plot)

  summary <- make_irf_summary_table(
    irf_object = irf,
    variable_names = variables_habitual_foreign,
    shock_labels = c(
      "Monetary Policy Shock",
      "Adverse Macro Shock"
    )
  )

  write.csv(
    summary,
    file.path(
      "4_Output_Analysis/Model_Foreign/Habitual",
      paste0("IRF_Baseline_Lag_", p, "_Summary.csv")
    ),
    row.names = FALSE
  )
}

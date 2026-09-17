


#============================================================================#
#                              [6] IRF
#============================================================================#

irf_updated <- compute_impulse_responses(
  run_bsvar_sign_model_01,
  horizon = H_total,
  standardise = TRUE # See Function about FEVD.
)


# Export IRF Results to CSV
irf_df <- export_bvar_results_to_csv(
  bvar_array = irf_updated,
  var_names  = target_cols,
  file_name  = "Analysis/BVAR_Sign/Model_01/IRF_Summary_Results.csv"
)


# Run the plotting function
irf_plot <- plot_bvar_irf(
  csv_path = "Analysis/BVAR_Sign/Model_01/IRF_Summary_Results.csv",
  shock_name = "policy_rate",
  target_cols = target_cols
)
ggsave("Analysis/BVAR_Sign/Model_01/IRF/IRF_Policy_Shock.png", irf_plot, width = 10, height = 7)



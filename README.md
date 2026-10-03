###############################################################################
############################# Description #####################################
###############################################################################

0a - 0b: Setup (Scripts and Functions)
0c - 0d: Raw data extraction (API and XLSX files)
1a: Dataset processing for KIX weighting (Minor importance)
1b: Dataset processing for household financials (Important)
1c: Dataset processing for raw data pre-inspection
1d: Pre-inspection data
2a: Dataset inspection (Outliers and seasonal adjustment)
2b: Seasonally adjusted data (Data used for estimations)
3a - 3c: Diagnostics (Stationarity, correlation, and PCA)
3c: Lag selection
4a: Housing specification
4b: Macro specification
4c: Macro specification - Foreign extension
4d: Macro specification - Narrative extension
5: Impulse response script
#=============================================================================#
                                Note on Replication
Below is a list of options in case you want to replicate the analysis. Note that
0a_Scripts.R and 0b_Functions.R must be run regardless of the chosen option.


#=============================================================================#
                               [Results Only]
Script 5.

Given that the RDS files are provided in a zip archive, all you need to do is replace 
the 3_Model_Output folder with the one from the zip if you want to replicate the 
IRF results using Script 5. However, the results have already been pushed 
to Git for your review!


#=============================================================================#
                             [Models and Results]
Scripts 4a-4d -> Script 5

Script 4 provides the code for modeling and can be run in any order.
I have added a loop function that clears the cache, because the package 
sometimes has trouble estimating models if the burn-in and estimation phases 
are not conducted sequentially.

Full re-estimation of all 40 models requires substantial 
computing capacity. Depending on your hardware, it may take 
several hours. I do not expect you to run the model again.


#=============================================================================#
                             [Data Processing]
1a-1b -> 1c -> 2a -> 3a* -> 3b* -> 3c* -> 4a-4d

This allows you to go from processing the raw data to full model 
estimation. Note that steps 3a-3c are optional and not strictly necessary for the model 
estimation (step 4) to run properly. *(Note: Asterisk meaning adjusted based on flow).*



#=============================================================================#
                     [API Extraction to Model Estimation]
0d -> 1a-1b -> 1c -> 2a -> 3a* -> 3b* -> 3c* -> 4a-4d

This gives you the full pipeline: from pulling data via APIs to model 
estimation. You can run 0c as well, but note that the weights are already provided, 
and extracting data directly from an online XLSX file may cause issues in the 
future. I wanted to make the program fully reliant on APIs, but unfortunately, 
Japan and Global Inflation data were not readily available. Only 2 of the 20 
data sources are static and not API-based (or 3 if you include the weights).
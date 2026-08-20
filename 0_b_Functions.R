#####################################0. FUNCTIONS #######################################
##       

#Functions in order of appearance with purpuose.
# (1) download_pxweb
# (2) get_riksbank_series
# (3) compute_fevd_from_irf
# (4)export_bvar_results_to_csv
# (5) get_irf_median_df

# ========================== 1a_Data_Set_Creation.R ====================================#

## (1) download_pxweb
# Purpuose: SCB PXWEB downloader, helps to streamline the download from SCB APIs using
# the pwxweb query.
download_pxweb <- function(url, query){

  pxq <- pxweb_query(query) # Converts list into a valid PX-Web query object.
  pxweb_get_data( # Executes the HTTP call to SCB and retrieves the data
    url = url,  # API URL
    query = pxq,
    column.name.type = "text",
    variable.value.type = "text"
  )
}


## (2) get_riksbank_series
# Purpouse: Helps to streamline the download from Riksbanken API
# originally I downloaded multiple items from Riksbanken (deposit_rate, lending_rate)
# so it was necessary then, but not strictly necessary now.

# Riksbank SWEA downloader
start_date <- "1995-01-01"
get_riksbank_series <- function(series_id,
                                from = "1995-01-01",  # Self-contained default
                                to = Sys.Date()) {

  url <- paste0(
    "https://api.riksbank.se/swea/v1/Observations/",
    series_id,
    "/",
    from,
    "/",
    to
  )

  response <- GET(url)

  stop_for_status(response)

  fromJSON(
    content(
      response,
      "text",
      encoding = "UTF-8"
    )
  ) |>
    as.data.frame()

}


# ========================== 2_BVARSIGN.R====================================#

#This is where function were necessary. Unfourtanately the BVARSIGN package
#encounters issues when dealing with large calculations and presents the 
#following message  "C:\Program Files\R\R-4.5.2\bin\R.exe '--no-save', 
#'--no-restore'" terminated with exit code: -1073741819. when using
# FEVD and historical decomposition.
# This is not surprising considering we are dealing with 4D objects that can
# reach in the millions of allocated values.
# E.G. 9 Rows * 9 Columns * 20 Time Periods * 30 000 Draws = ~19.5 Million.
# The IRF however still works, and the following allows to increase the draws
# while still being able to get the FEVD values.


## (3) compute_fevd_from_irf
# Used instead of compute_variance_decompositions() as it crashed.
# The purpouse is to compute FEVD manually by auto extracting the
# arrays. Due to multiple FEVD being necessar, having the whole
# script will clutter the script so I decided to create
# a function.

compute_fevd_from_irf <- function(irf_obj) {
  
  # 1. Automatically extract the 4D numeric array
  if (is.array(irf_obj)) {
    irf_array <- irf_obj
  } else if (is.list(irf_obj) && !is.null(irf_obj$posterior$irf)) {
    irf_array <- irf_obj$posterior$irf
  } else if (is.list(irf_obj) && !is.null(irf_obj$irf)) {
    irf_array <- irf_obj$irf
  } else if (inherits(irf_obj, "PosteriorIRF")) {
    # If returned as S3 class with array inside
    irf_array <- irf_obj[,,,]
  } else {
    stop("Could not extract 4D array from irf_obj. Run str(irf_updated) to check its class.")
  }

  dims <- dim(irf_array)
  N <- dims[1]
  H <- dims[3]
  S <- dims[4]
  
  message(paste("Processing FEVD for N =", N, "variables over H =", H, "horizons across S =", S, "draws..."))

  # 2. Square responses (variance per shock per draw)
  irf_sq <- irf_array^2
  
  # 3. Compute cumulative sum over horizons
  fevd <- array(0, dim = c(N, N, H, S))
  
  for (s in 1:S) {
    for (i in 1:N) {         # Variable
      for (j in 1:N) {       # Shock
        fevd[i, j, , s] <- cumsum(irf_sq[i, j, , s])
      }
      
      # 4. Normalize across all shocks at each horizon step
      for (h in 1:H) {
        total_var <- sum(fevd[i, , h, s])
        if (!is.na(total_var) && total_var > 0) {
          fevd[i, , h, s] <- fevd[i, , h, s] / total_var
        }
      }
    }
  }
  
  dimnames(fevd) <- dimnames(irf_array)
  return(fevd)
}



## (4) export_bvar_results_to_csv
# This allows to export the IRF & FEVD to CSV for analysis
# Note, I did not export the full posterior since it made
# a files in the GBs.

export_bvar_results_to_csv <- function(bvar_array, var_names, file_name, shock_names = var_names) {
  
  # 1. Automatically handle raw arrays or package list objects
  if (is.array(bvar_array)) {
    arr <- bvar_array
  } else if (!is.null(bvar_array$posterior$irf)) {
    arr <- bvar_array$posterior$irf
  } else if (!is.null(bvar_array$irf)) {
    arr <- bvar_array$irf
  } else {
    arr <- bvar_array[,,,]
  }
  
  dims <- dim(arr)
  N <- dims[1]
  H <- dims[3]
  
  summary_list <- list()
  idx <- 1
  
  # 2. Iterate over all variable-shock pairs and compute quantiles
  for (i in 1:N) {         # Response Variable
    for (j in 1:N) {       # Shock Source
      sub_mat <- arr[i, j, , ] # Matrix of size [Horizon x Draws]
      
      summary_list[[idx]] <- data.frame(
        Variable = var_names[i],
        Shock    = shock_names[j],
        Horizon  = 1:H,
        Mean     = apply(sub_mat, 1, mean, na.rm = TRUE),
        Median   = apply(sub_mat, 1, median, na.rm = TRUE),
        Lower_90 = apply(sub_mat, 1, quantile, probs = 0.05, na.rm = TRUE),
        Lower_68 = apply(sub_mat, 1, quantile, probs = 0.16, na.rm = TRUE),
        Upper_68 = apply(sub_mat, 1, quantile, probs = 0.84, na.rm = TRUE),
        Upper_90 = apply(sub_mat, 1, quantile, probs = 0.95, na.rm = TRUE)
      )
      idx <- idx + 1
    }
  }
  
  # 3. Combine list into a single Data Frame
  df_summary <- do.call(rbind, summary_list)
  
  # 4. Write to CSV file
  write.csv(df_summary, file = file_name, row.names = FALSE)
  message(paste("Successfully saved summary to:", file_name))
  
  return(df_summary)
}


## (5) get_irf_median_df
# This function allows to easily extract median IRF for a specific 
# response variable & shock. Unfourtanely the bvarsign nor the BVAR
# allow to export a single row from the IRF and instead plot all 9X9.

get_irf_median_df <- function(irf_obj, response_var, shock_var, label_name, target_cols) {
  arr <- if (is.array(irf_obj)) irf_obj else irf_obj$posterior$irf
  
  r_idx <- which(target_cols == response_var)
  s_idx <- which(target_cols == shock_var)
  
  if (length(r_idx) == 0 || length(s_idx) == 0) {
    stop("Variable or shock name not found in target_cols.")
  }
  
  sub_mat <- arr[r_idx, s_idx, , ]
  med <- apply(sub_mat, 1, median, na.rm = TRUE)
  
  data.frame(
    Horizon = 1:dim(arr)[3],
    Median = med,
    Specification = label_name
  )
}










#get_irf_median_df <- function(irf_obj, response_var, shock_var, label_name) {
#  arr <- if (is.array(irf_obj)) irf_obj else irf_obj$posterior$irf
#  r_idx <- which(target_cols == response_var)
#  s_idx <- which(target_cols == shock_var)
  
#  sub_mat <- arr[r_idx, s_idx, , ]
#  med <- apply(sub_mat, 1, median, na.rm = TRUE)
#  
#  data.frame(
#    Horizon = 1:dim(arr)[3],
#    Median = med,
#    Specification = label_name
#  )
#}

#' Add SPC rule flags (Western Electric style) to a CLABSI dataset
#'
#' @param df A data frame that already contains at least:
#'   - a numeric column `z` (z-scores)
#'   - a numeric column `clabsi` (counts)
#' @param z_col Name of the z-score column (default "z")
#' @param y_col Name of the response/measurement column (default "clabsi")
#' @return The same data frame with logical columns:
#'   rule_point_3sigma, rule_run_8_side, rule_trend_6, spc_any
#' @export
add_spc_rules <- function(df, z_col = "z", y_col = "clabsi") {
  
  # Auto-compute z if missing
  if (!z_col %in% names(df)) {
    if (!("expected" %in% names(df))) {
      stop("No z-scores and no 'expected' column available to compute them.")
    }

    df[[z_col]] <- (df[[y_col]] - df[["expected"]]) / sqrt(df[["expected"]])
  }
  
  
  
  
  # --- Pull out vectors for convenience ---
  z <- df[[z_col]]          # numeric vector of z-scores
  y <- df[[y_col]]          # numeric vector of CLABSI counts (or rates)
  n <- length(z)            # number of observations (months)
  
  ## --------------------------
  ## Rule 1: Point beyond 3 SD
  ## --------------------------
  # TRUE where |z| > 3, FALSE otherwise
  rule_point_3sigma <- abs(z) > 3
  
  ## --------------------------------------
  ## Rule 2: 8 consecutive points
  ##         on the same side of centerline
  ## --------------------------------------
  # We just need z > 0 (above centerline) or z < 0 (below).
  # Encode:  1 = above centerline, -1 = below, 0 = exactly on centerline
  side <- ifelse(z > 0, 1L,
                 ifelse(z < 0, -1L, 0L))
  
  # Start with all FALSE
  rule_run_8_side <- rep(FALSE, n)
  
  # rle() = "run length encoding": it finds stretches of the same value
  r <- rle(side) #detects dup
  run_lengths <- r$lengths   # how long each run is
  run_values  <- r$values    # the side value for each run (1, -1, or 0)
  
  # Compute start and end indices of each run in the original vector
  run_end   <- cumsum(run_lengths)
  run_start <- run_end - run_lengths + 1
  
  # Loop over runs and mark runs >= 8 that are all above or all below
  for (k in seq_along(run_lengths)) {
    if (run_values[k] != 0L && run_lengths[k] >= 8L) {
      idx_seq <- run_start[k]:run_end[k]
      rule_run_8_side[idx_seq] <- TRUE
    }
  }
  
  ## --------------------------------------
  ## Rule 3: 6-point trend
  ##         (strictly increasing or decreasing)
  ## --------------------------------------
  rule_trend_6 <- rep(FALSE, n)
  
  # Only meaningful if we have at least 6 points
  if (n >= 6) {
    # Slide a window of size 6 along the series
    for (i in 1:(n - 5)) {
      # Take 6 consecutive y values
      segment <- y[i:(i + 5)]
      # Differences between consecutive points
      diffs <- diff(segment)
      
      # All diffs > 0  => strictly increasing trend
      if (all(diffs > 0)) {
        rule_trend_6[i:(i + 5)] <- TRUE
      }
      
      # All diffs < 0  => strictly decreasing trend
      if (all(diffs < 0)) {
        rule_trend_6[i:(i + 5)] <- TRUE
      }
    }
  }
  
  ## --------------------------
  ## Combine rules into one flag
  ## --------------------------
  spc_any <- rule_point_3sigma | rule_run_8_side | rule_trend_6
  
  # Attach these flags back to the data frame
  df$rule_point_3sigma <- rule_point_3sigma
  df$rule_run_8_side   <- rule_run_8_side
  df$rule_trend_6      <- rule_trend_6
  df$spc_any           <- spc_any
  
  # Create a categorical label for plotting
  df$spc_flag <- dplyr::case_when(
    rule_point_3sigma ~ "Rule 1: Point beyond 3 SD",
    rule_run_8_side ~ "Rule 2: 8 points on one side",
    rule_trend_6 ~ "Rule 3: 6-point trend",
    TRUE ~ "No Rules"
  )
  
  
  return(df)
}




#' Create a Poisson-based u-chart table for CLABSI surveillance
#'
#' @description
#' Computes expected CLABSI counts, Poisson control limits, and alerts.
#'
#' @param data Data frame with: month, clabsi, line_days, and optionally year
#' @return A clean u-chart table
#' @export
alert_poisson_clabsi <- function(data) {

  
  
  # --- Required columns ---
  required <- c("month", "clabsi", "line_days")
  if (!all(required %in% names(data))) {
    stop("Data frame must include: month, clabsi, line_days")
  }
  
  # --- Add year if missing ---
  if (!"year" %in% names(data)) {
    data$year <- 1L
  }
  
  # --- Month-year label ---
 data$month_year <- paste0(data$month, "-", data$year)
  
  # --- u = CLABSI per line-day ---
  u <- data$clabsi / data$line_days
  data$u_baseline <-u
  
  # --- Overall mean rate ---
  u_bar <- mean(u, na.rm = TRUE)
  
  # --- Expected count per month (raw units) ---
  expected <- u_bar * data$line_days
  data$expected <- expected
  
  # centerline in raw units---
  centerline <- mean(expected, na.rm = TRUE)
  data$centerline<-centerline
  
  
  # --- Poisson u-chart limits (rate form) ---
  
  UCL_rate <- u_bar + 3 * sqrt(u_bar / data$line_days)
  LCL_rate <- pmax(0, u_bar - 3 * sqrt(u_bar / data$line_days))
  
  
  # --- Convert limits to raw CLABSI counts ---
  upper <- UCL_rate * data$line_days
  lower <- LCL_rate * data$line_days
  
  # --- Round everything ---
  expected <- round(expected, 2)
  upper    <- round(upper, 2)
  lower    <- round(lower, 2)
 
  
  # --- Alerts (rate rule) ---
  alert <- u > UCL_rate
  
  # --- Final clean output table ---
  output <- data.frame(
    year       = data$year,
    month      = data$month,
    month_year = data$month_year,
    clabsi     = data$clabsi,
    line_days  = data$line_days,
    expected  = expected,
    UCL      = upper,
    LCL      = lower,
    alert      = alert
  )
  
  return(output)
}

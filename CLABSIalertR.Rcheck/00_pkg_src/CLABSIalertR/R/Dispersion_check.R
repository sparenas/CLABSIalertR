#' Check dispersion of CLABSI counts
#'
#' @description
#' Calculates the variance-to-mean ratio for CLABSI counts to assess
#' whether the data follow a Poisson distribution. 
#' Ratios near 1 suggest Poisson behavior;
#' smaller than 1 indicates underdispersion (stable process);
#' bigger than 1 suggests overdispersion (extra variability).
#'
#' @param data Data frame with a numeric column `clabsi`
#' @return Numeric value of the variance-to-mean ratio
#' @examples
#' df <- data.frame(clabsi = c(1,0,0,2,1,3,0,0,1,0,2,4))
#' check_dispersion(df)
#' @importFrom stats var
#' @export
check_dispersion <- function(data) {
  if (!"clabsi" %in% names(data)) {
    stop("Data frame must include a 'clabsi' column.")
  }
  
  ratio <- var(data$clabsi, na.rm = TRUE) / mean(data$clabsi, na.rm = TRUE)
  
  if (ratio < 0.8) {
    message(paste("Dispersion ratio =", round(ratio, 2), 
                  "- Underdispersed (stable process)."))
  } else if (ratio > 1.2) {
    message(paste("Dispersion ratio =", round(ratio, 2), 
                  "- Overdispersed (variable process)."))
  } else {
    message(paste("Dispersion ratio =", round(ratio, 2), 
                  "- Consistent with Poisson expectation."))
  }
  
  return(ratio)
}

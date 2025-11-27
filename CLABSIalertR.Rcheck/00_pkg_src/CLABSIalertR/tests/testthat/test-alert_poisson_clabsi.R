test_that("alert_poisson_clabsi prints and returns correctly", {
  
  df <- data.frame(
    month = 1:3,
    clabsi = c(1, 4, 2),
    line_days = c(900, 1000, 950)
  )
  
  # Run function
  result <- alert_poisson_clabsi(df)
  
  # Correct columns should exist
  expect_true(all(c("UCL", "LCL", "alert") %in% names(result)))
  
  # Output should be same number of rows
  expect_equal(nrow(result), nrow(df))
  
  # Alerts should be logical
  expect_type(result$alert, "logical")
})

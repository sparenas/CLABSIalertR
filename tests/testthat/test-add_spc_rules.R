test_that("add_spc_rules() adds expected columns", {
  
  df <- data.frame(
    clabsi = c(1,2,3,4,5,6),
    z      = c(0, 1, -2, 0.5, 3.5, 0)
  )
  
  out <- add_spc_rules(df)
  
  expect_true(all(c("rule_point_3sigma",
                    "rule_run_8_side",
                    "rule_trend_6",
                    "spc_any") %in% names(out)))
})



test_that("Rule 1: Detects points beyond 3 sigma", {
  
  df <- data.frame(
    clabsi = c(1, 3, 2, 4, 3, 2),
    z      = c(0, 1, 2, 3.2, -3.5, 0)
  )
  
  out <- add_spc_rules(df)
  
  expect_equal(out$rule_point_3sigma,
               c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})



test_that("Rule 2: Detects 8 consecutive points on same side", {
  
  df <- data.frame(
    clabsi = 1:10,
    z      = c(0.5, 0.2, 0.3, 0.4, 1.1, 0.7, 0.9, 0.8, -0.5, 0)
  )
  
  out <- add_spc_rules(df)
  
  expect_equal(out$rule_run_8_side,
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})



test_that("Rule 3: Detects 6-point increasing trend", {
  
  df <- data.frame(
    clabsi = c(1,2,3,4,5,6,3,2),
    z      = rep(0, 8)
  )
  
  out <- add_spc_rules(df)
  
  expect_equal(out$rule_trend_6,
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})



test_that("Rule 3: Detects 6-point decreasing trend", {
  
  df <- data.frame(
    clabsi = c(12,11,10,9,8,7,15),
    z      = rep(0, 7)
  )
  
  out <- add_spc_rules(df)
  
  expect_equal(out$rule_trend_6,
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
})



test_that("spc_any is TRUE when any rule is triggered", {
  
  df <- data.frame(
    clabsi = c(1, 3, 2, 4, 3, 2),
    z      = c(0, 1, 2, 3.5, 0, 0)
  )
  
  out <- add_spc_rules(df)
  
  expect_equal(out$spc_any,
               c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
})

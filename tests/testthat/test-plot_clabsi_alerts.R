
  test_that("plot_clabsi_alerts returns a valid ggplot object", {
    # single-year test here…
  
  
  
  df <- data.frame(
    month = 1:6,
    clabsi = c(8, 1, 3, 9, 4, 2),
    line_days = c(900, 950, 1000, 970, 980, 960)
  )
  
  # Run alert function first to get alerts
  result <- alert_poisson_clabsi(df)
  
  # Run plotting function
  p<- plot_clabsi_alerts(result)
  
  # Check that plot1 is a ggplot object
 expect_s3_class(p, "ggplot")
  
  # Ensure no errors occur during plot rendering
expect_error(print(p), NA)
 

# --- TEST 3: key layers exist ---
layer_geoms <- sapply(p$layers, function(l) class(l$geom)[1])

# Points for clabsi
expect_true("GeomPoint" %in% layer_geoms)

# Line for clabsi over time
expect_true("GeomLine" %in% layer_geoms)

# Step or line for UCL/LCL
expect_true(any(layer_geoms %in% c("GeomStep", "GeomPath")))


})
  
  
  test_that("plot_clabsi_alerts handles multiple years", {
  df <- data.frame(
    year = rep(2023:2024, each = 3),
    month = rep(1:3, times = 2),
    clabsi = c(0,1,3, 2,1,0),
    line_days = c(1000,950,900, 970,980,960)
  )

  result <- alert_poisson_clabsi(df)
  p <- plot_clabsi_alerts(result)

  expect_s3_class(p, "ggplot")
  expect_error(print(p), NA)
 # expect_true("FacetWrap" %in% class(p$facet))
})
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  

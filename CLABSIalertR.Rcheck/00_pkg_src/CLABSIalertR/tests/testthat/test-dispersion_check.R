test_that("dispersion_check computes variance divided by mean of CLABSIs correctly",{
#create sample data set

df <- data.frame(
  month = 1:9,
  clabsi = c(6,3,4,5,3,6,2,3,1),
  line_days = c(3578,3673,3942,4074,3910,3766,3787,3670,3707)
)
  
#expected result(manual calculation)
expected_ratio <- var(df$clabsi)/mean(df$clabsi)

#run function package function
result<- check_dispersion(df)

# Test that the numeric ratio matches expected value (within rounding)
expect_equal(result, expected_ratio, tolerance = 1e-8)
})
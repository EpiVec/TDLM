# Inputs -----------------------------------------------------------------------
data(county)

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  res <- extract_spatial_information(county, id = "ID", show_progress = FALSE)

  alpha <- calib_param(av_surf = mean(res$surface), law = "NGravExp")

  expect_gt(alpha, 0.085)

  alpha <- calib_param(av_surf = mean(res$surface), law = "NGravPow")

  expect_gt(alpha, 3.2)

  alpha <- calib_param(av_surf = mean(res$surface), law = "RadExt")

  expect_gt(alpha, 1.29)
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  res <- extract_spatial_information(county, 
                                     id = "ID", 
                                     show_progress = FALSE)

  expect_error(
    calib_param(av_surf = mean(res$surface), law = "test"),
    "One or several laws chosen are not available.
Please choose from the following:
NGravExp, NGravPow, Schneider, or RadExt."
  )
  
})

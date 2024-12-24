# Inputs -----------------------------------------------------------------------
data(county)

res <- extract_spatial_information(county, 
                                   id = "ID", 
                                   show_progress = FALSE)

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  alpha <- calib_param(av_surf = mean(res$surface), 
                       law = "NGravExp")
  expect_gt(alpha, 0.085)

  alpha <- calib_param(av_surf = mean(res$surface), 
                       law = "NGravPow")
  expect_gt(alpha, 3.2)

  alpha <- calib_param(av_surf = mean(res$surface), 
                       law = "Schneider")
  expect_gt(alpha, 0.0000024)
  
  alpha <- calib_param(av_surf = mean(res$surface), 
                       law = "RadExt")
  expect_gt(alpha, 1.29)
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {

  # av_surf
  expect_error(
    calib_param(av_surf = c(0.1, 0.1), 
                law = "NGravExp"),
    "av_surf must be of length 1.",
    fixed = TRUE)

  
  expect_error(
    calib_param(av_surf = "a", 
                law = "NGravExp"),
    "av_surf must be numeric.",
    fixed = TRUE)
  
  expect_error(
    calib_param(av_surf = -1, 
                law = "NGravExp"),
    "av_surf must be strictly higher than 0.",
    fixed = TRUE) 
  
  expect_error(
    calib_param(av_surf = 0, 
                law = "NGravExp"),
    "av_surf must be strictly higher than 0.",
    fixed = TRUE) 
  
  # law
  expect_error(
    calib_param(av_surf = mean(res$surface), 
                law = c("a","b")),
    "law must be of length 1.",
    fixed = TRUE) 
  
  expect_error(
    calib_param(av_surf = mean(res$surface), 
                law =  NA),
    "law must be a character.",
    fixed = TRUE) 
  
  expect_error(
    calib_param(av_surf = mean(res$surface), 
                law = 1),
    "law must be a character.",
    fixed = TRUE) 
  
  expect_error(
    calib_param(av_surf = mean(res$surface), 
                law = "test"),
    "One or several laws chosen are not available.
Please choose from the following:
NGravExp, NGravPow, Schneider, or RadExt.",
    fixed = TRUE) 

})

# Inputs -----------------------------------------------------------------------
data(county)

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  res <- extract_spatial_information(county, id = "ID", show_progress = FALSE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)

  res <- extract_spatial_information(county, id = NULL, show_progress = FALSE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)

  res <- extract_spatial_information(county, id = NULL, show_progress = TRUE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    extract_spatial_information(NULL, id = NULL, show_progress = TRUE),
    "It seems that the geometry used is not an sf object."
  )
  
})
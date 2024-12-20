# Inputs -----------------------------------------------------------------------
data(distance)
data(coords)
data(coords_xy)

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  d1 <- extract_distances(coords = coords, 
                          method = "Haversine", 
                          id = rownames(coords),
                          show_progress = TRUE)
  
  d2 <- extract_distances(coords = coords, 
                          method = "Haversine", 
                          id = NULL,
                          show_progress = FALSE)

  expect_identical(class(d1)[1], "matrix")
  expect_identical(as.numeric(sum(d1==distance)), 11025)
  expect_identical(as.numeric(sum(d1==d1)), 11025)
  expect_identical(as.numeric(sum(rownames(d1)==rownames(distance))), 105)
  expect_identical(as.numeric(sum(colnames(d1)==colnames(distance))), 105)
  
})


# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    extract_distances(coords, method = 1),
    "method must be a character.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(coords, method = "enenut"),
    "Please choose method from the following:
Haversine or Euclidean.",
    fixed = TRUE)
  
})






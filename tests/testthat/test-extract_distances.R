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
  
  d3 <- extract_distances(coords = coords_xy, 
                          method = "Euclidean", 
                          id = NULL,
                          show_progress = TRUE)
  
  d4 <- extract_distances(coords = coords_xy, 
                          method = "Euclidean", 
                          id = NULL,
                          show_progress = FALSE)

  expect_identical(class(d1)[1], "matrix")
  expect_identical(as.numeric(sum(d1==distance)), 11025)
  expect_identical(as.numeric(sum(d1==d2)), 11025)
  expect_identical(as.numeric(sum(rownames(d1)==rownames(distance))), 105)
  expect_identical(as.numeric(sum(colnames(d1)==colnames(distance))), 105)
  expect_identical(as.numeric(sum(rownames(d3)==rownames(distance))), 105)
  expect_identical(as.numeric(sum(colnames(d3)==colnames(distance))), 105)
  expect_identical(as.numeric(sum(rownames(d4)==rownames(distance))), 105)
  expect_identical(as.numeric(sum(colnames(d4)==colnames(distance))), 105)
  
})


# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    extract_distances(1, 
                      method = 1),
    "coords must be a matrix or a data.frame.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(data.frame(test = c(1,2)), 
                      method = 1),
    "coords must be a data.frame with two columns.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(data.frame(test1 = c(1,NA), test2 = c(1,2)), 
                      method = 1),
    "^NA")
  
  expect_error(
    extract_distances(data.frame(test1 = c("a","b"), test2 = c(1,2)), 
                      method = 1),
    "coords must have only numeric values.",
    fixed = TRUE) 
  
  expect_error(
    extract_distances(coords, 
                      method = 1),
    "method must be a character.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(coords, 
                      method = "enenut"),
    "Please choose method from the following:
Haversine or Euclidean.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(coords, 
                      method = "Haversine",
                      id = TRUE),
    "id should be numeric or character.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(coords, 
                      method = "Haversine",
                      id = "a"),
    "id must have a length equal to the number of locations.",
    fixed = TRUE)
  
  expect_error(
    extract_distances(coords, 
                      method = "Haversine",
                      id = rep("a", dim(coords)[1])),
    "Duplicated names associated with id.",
    fixed = TRUE)
  
})






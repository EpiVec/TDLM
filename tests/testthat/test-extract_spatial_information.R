# Inputs -----------------------------------------------------------------------
library(sf)
data(county)

county2 <- county
st_crs(county2) <- NA

county3 <- county
colnames(county3)[2] <- "ID" 

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  res <- extract_spatial_information(county, 
                                     id = "ID", 
                                     great_circle = TRUE,
                                     show_progress = FALSE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)
  
  res <- extract_spatial_information(county, 
                                     id = "ID", 
                                     great_circle = FALSE,
                                     show_progress = FALSE)
  
  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)

  res <- extract_spatial_information(county, 
                                     id = NULL, 
                                     show_progress = FALSE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)

  res <- extract_spatial_information(county, 
                                     id = NULL, 
                                     show_progress = TRUE)

  expect_identical(class(res), "list")
  expect_identical(as.numeric(dim(res$distance)), c(105, 105))
  expect_identical(as.numeric(length(res$surface)), 105)
  expect_identical(trunc(mean(res$surface)), 2028)
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    extract_spatial_information(NULL, 
                                id = NULL, 
                                show_progress = TRUE),
    "It seems that the geometry used is not an sf object."
  )
  
  expect_error(
    extract_spatial_information(county2, 
                                id = NULL, 
                                show_progress = TRUE),
    "Not possible to identify the coordinate reference system of geometry."
  )
  
  expect_error(
    extract_spatial_information(county, 
                                id = "John", 
                                show_progress = TRUE),
    "If id is a character, it should be a column name of geometry."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = "ID", 
                                show_progress = TRUE),
    "Two or more columns with name id."
  )
  
  expect_error(
    extract_spatial_information(county, 
                                id = factor("John"), 
                                show_progress = TRUE),
    "If id is a character, it should be a column name of geometry."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = factor("ID"), 
                                show_progress = TRUE),
    "Two or more columns with name id."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = 0.1, 
                                show_progress = TRUE),
    "If id is numeric, it should be an integer."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = 0, 
                                show_progress = TRUE),
    "id should be strictly positive."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = 14, 
                                show_progress = TRUE),
    "id should be lower or equal to 5."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = TRUE, 
                                show_progress = TRUE),
    "id should be numeric or character."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = matrix(2,2,2), 
                                show_progress = TRUE),
    "^id should be a vector"
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = c(TRUE, TRUE), 
                                show_progress = TRUE),
    "id should be numeric or character."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = c("a", "b"), 
                                show_progress = TRUE),
    "id must have a length equal to the number of locations."
  )
  
  expect_error(
    extract_spatial_information(county3, 
                                id = rep("a", dim(county3)[1]), 
                                show_progress = TRUE),
    "Duplicated names associated with id."
  )
  
})
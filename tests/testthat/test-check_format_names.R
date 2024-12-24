# Inputs -----------------------------------------------------------------------
data(mass)
data(distance)
data(od)

mi <- as.numeric(mass[, 1])
mi2 <- mi
names(mi) <- rownames(distance)

mj <- as.numeric(mass[, 1])
names(mj) <- rownames(distance)

Oi <- as.numeric(mass[, 2]) + 0.001
names(Oi) <- rownames(distance)

Dj <- as.numeric(mass[, 3])
names(Dj) <- rownames(distance)

dist <- distance
dist2 <- dist
rownames(dist2) <- NULL

odobs <- od

vectors <- list(mi = mi, mj = mj, Oi = Oi, Dj = Dj)
vectors1 <- list(mi, mj)
vectors2 <- list(mi = mi2, mj = mj, Oi = Oi, Dj = Dj)

matrices <- list(Dij = dist, OD = odobs)
matrices1 <- list(dist)
matrices2 <- list(Dij = dist2, OD = odobs)

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  expect_message(
    check_format_names(vectors = vectors, 
                       matrices = matrices, 
                       check = "format_and_names"),
    "The inputs passed the format_and_names checks successfully!"
  )

  expect_message(
    check_format_names(vectors = vectors, 
                       matrices = matrices, check = "format"),
    "The inputs passed the format checks successfully!"
  )
  
  expect_message(
    check_format_names(vectors = vectors1, 
                       matrices = NULL, 
                       check = "format"),
    paste0(
      "No names identified in the vectors list.\n",
      "Names have been automatically assigned."
    )
  )
  
  expect_message(
    check_format_names(vectors = NULL, 
                       matrices = matrices1, 
                       check = "format"),
    paste0(
      "No names identified in the matrices list.\n",
      "Names have been automatically assigned."
    )
  )
  
  expect_error(
    check_format_names(vectors = vectors2, 
                       matrices = NULL, 
                       check = "format_and_names"),
    "^The inputs must contain the same number of names id!"
  )
  
  expect_error(
    check_format_names(vectors = NULL, 
                       matrices = matrices2, 
                       check = "format_and_names"),
    "^The inputs must contain the same number of names id!"
  )

})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    check_format_names(vectors = NULL, 
                       matrices = NULL, 
                       check = "format"),
    "At least one of the vectors or matrices argument should be non-null."
  )
  
  expect_error(
    check_format_names(vectors = vectors, 
                       matrices = NULL, 
                       check = "test"),
    "Please choose check from the following:
format or format_and_names."
  )

}) 
  
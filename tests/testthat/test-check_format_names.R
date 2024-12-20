# Inputs -----------------------------------------------------------------------
data(mass)
data(distance)
data(od)

mi <- as.numeric(mass[, 1])
names(mi) <- rownames(distance)

mj <- as.numeric(mass[, 1])
names(mj) <- rownames(distance)

Oi <- as.numeric(mass[, 2]) + 0.001
names(Oi) <- rownames(distance)

Dj <- as.numeric(mass[, 3])
names(Dj) <- rownames(distance)

dist <- distance

odobs <- od

vectors <- list(mi = mi, mj = mj, Oi = Oi, Dj = Dj)
matrices <- list(Dij = dist, OD = odobs)


# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  expect_message(
    check_format_names(vectors = vectors, matrices = matrices, 
                       check = "format_and_names"),
    "The inputs passed the format_and_names checks successfully!"
  )

  expect_message(
    check_format_names(vectors = vectors, 
                       matrices = matrices, check = "format"),
    "The inputs passed the format checks successfully!"
  )

  expect_error(
    check_format_names(vectors = NULL, matrices = NULL, check = "format"),
    "At least one of the vectors or matrices argument should be non-null."
  )

  expect_error(
    check_format_names(vectors = vectors, matrices = NULL, check = "test"),
    "Please choose check from the following:
format or format_and_names."
  )
  
})
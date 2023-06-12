# Preamble code ----------------------------------------------------------------
data(mass)
data(distance)

mi <- as.numeric(mass[, 1])
names(mi) <- rownames(distance)

dist <- distance

# Tests for valid outputs ------------------------------------------------------
test_that("class matrix and dimensions", {
  sij <- extract_opportunities(opportunity = mi, distance = dist, 
                               check_names = TRUE)

  expect_identical(class(sij)[1], "matrix")
})
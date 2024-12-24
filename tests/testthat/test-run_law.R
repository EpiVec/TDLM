# Inputs -----------------------------------------------------------------------
data(mass)
data(distance)
data(od)

mi <- as.numeric(mass[, 1])
names(mi) <- rownames(distance)

sij <- extract_opportunities(opportunity = mi, distance = distance, 
                             check_names = TRUE)

mj <- as.numeric(mass[, 1]) + 0.1
names(mj) <- rownames(distance)

dist <- distance

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  res <- run_law(law = "Unif", 
                 mass_origin = mi, 
                 distance = NULL, 
                 opportunity = NULL, 
                 param = 0.1,
                 check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law(law = "NGravExp", 
                 mass_origin = mi, 
                 distance = distance, 
                 opportunity = NULL, 
                 param = 0.1,
                 check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")

  res <- run_law(law = "Rad", 
                 mass_origin = mi, 
                 distance = NULL, 
                 opportunity = sij, 
                 param = 0.1,
                 check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law(law = "NGravExp", 
                 mass_origin = mi, 
                 distance = distance, 
                 opportunity = sij, 
                 param = c(0.1,0.1),
                 check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    run_law(law = "Uni", 
            mass_origin = mi, 
            mass_destination = NULL, 
            distance = NULL, 
            opportunity = NULL, 
            param = 0.1,
            check_names = TRUE),
    "^Please choose law from the following:")
  
})
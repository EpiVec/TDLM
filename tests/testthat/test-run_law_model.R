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

Oi <- as.numeric(mass[, 2]) #+0.11
names(Oi) <- rownames(distance)

Dj <- as.numeric(mass[, 3]) #+0.11
names(Dj) <- rownames(distance)

dist <- distance

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = dist, 
                       opportunity = sij,
                       param = 0.01,
                       model = "DCM", 
                       nb_trips = NULL, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = TRUE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")

  res <- run_law_model(law = "Rad",
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = NULL, 
                       opportunity = sij, 
                       param = NULL,
                       model = "DCM", 
                       nb_trips = NULL, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                      write_proba = TRUE,
                      check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")

  res <- run_law_model(law = "Rad", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = NULL, 
                       opportunity = sij, 
                       param = NULL,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = NULL, 
                       average = FALSE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = NULL, 
                       opportunity = sij, 
                       param = NULL,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = NULL, 
                       average = FALSE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = NULL, 
                       opportunity = sij, 
                       param = NULL,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = NULL, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = NULL, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")  
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = NULL, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "PCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = NULL, 
                       average = FALSE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "PCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = NULL, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "ACM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = Dj, 
                       average = FALSE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = FALSE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "ACM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "PCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "Unif", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "DCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")  
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "PCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM") 
  
  res <- run_law_model(law = "NGravExp", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "ACM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM") 
  
  res <- run_law_model(law = "RadExt", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "UM", 
                       nb_trips = 1000, 
                       out_trips = NULL, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM")  
  
  res <- run_law_model(law = "RadExt", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "PCM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM") 
  
  res <- run_law_model(law = "RadExt", 
                       mass_origin = mi, 
                       mass_destination = mj, 
                       distance = distance, 
                       opportunity = sij, 
                       param = 0.1,
                       model = "ACM", 
                       nb_trips = 1000, 
                       out_trips = Oi, 
                       in_trips = Dj, 
                       average = TRUE, 
                       nbrep = 3, 
                       maxiter = 50000, 
                       mindiff = 0.00001,
                       write_proba = FALSE,
                       check_names = TRUE)
  expect_identical(class(res)[1], "TDLM") 
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  expect_error(
    run_law_model(law = "test", 
                  mass_origin = mi, 
                  mass_destination = mj, 
                  distance = NULL, 
                  opportunity = sij, 
                  param = NULL,
                  model = "UM", 
                  nb_trips = 1000, 
                  out_trips = NULL, 
                  in_trips = NULL, 
                  average = FALSE, 
                  nbrep = 3, 
                  maxiter = 50000, 
                  mindiff = 0.00001,
                  write_proba = FALSE,
                  check_names = FALSE),
    "^Please choose law from the following:")
  
  expect_error(
    run_law_model(law = "Rad", 
                  mass_origin = mi, 
                  mass_destination = mj, 
                  distance = NULL, 
                  opportunity = sij, 
                  param = NULL,
                  model = "test", 
                  nb_trips = 1000, 
                  out_trips = NULL, 
                  in_trips = NULL, 
                  average = FALSE, 
                  nbrep = 3, 
                  maxiter = 50000, 
                  mindiff = 0.00001,
                  write_proba = FALSE,
                  check_names = FALSE),
    "^Please choose model from the following:")
  
  expect_error(
    run_law_model(law = "Rad", 
                  mass_origin = mi, 
                  mass_destination = mj, 
                  distance = NULL, 
                  opportunity = sij, 
                  param = NULL,
                  model = "DCM", 
                  nb_trips = 1000, 
                  out_trips = Oi-1, 
                  in_trips = Dj, 
                  average = FALSE, 
                  nbrep = 3, 
                  maxiter = 50000, 
                  mindiff = 0.00001,
                  write_proba = FALSE,
                  check_names = FALSE),
    "Total number of out-going and in-coming trips must be equal.")
  

})
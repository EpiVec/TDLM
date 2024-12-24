# Inputs -----------------------------------------------------------------------
data(mass)
data(distance)
data(od)

proba <- matrix(runif(105 * 105), 105, 105)
rownames(proba) <- rownames(od)
colnames(proba) <- colnames(od)

mi <- as.numeric(mass[, 1])
names(mi) <- rownames(distance)

mj <- as.numeric(mass[, 1])
names(mj) <- rownames(distance)

Oi <- as.numeric(mass[, 2])
names(Oi) <- rownames(distance)

Dj <- as.numeric(mass[, 3])
names(Dj) <- rownames(distance)

res <- run_law_model(law = "GravExp", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = distance, 
                     opportunity = NULL, 
                     param = 0.01,
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj, 
                     average = FALSE, 
                     nbrep = 1, 
                     maxiter = 50000, 
                     mindiff = 0.00001,
                     write_proba = TRUE,
                     check_names = TRUE)

res2 <- run_law_model(law = "GravExp", 
                     mass_origin = mi, 
                     mass_destination = mj, 
                     distance = distance, 
                     opportunity = NULL, 
                     param = c(0.01, 0.1),
                     model = "DCM", 
                     nb_trips = NULL, 
                     out_trips = Oi, 
                     in_trips = Dj, 
                     average = FALSE, 
                     nbrep = 1, 
                     maxiter = 50000, 
                     mindiff = 0.00001,
                     write_proba = TRUE,
                     check_names = TRUE)

res3 <- res2[[2]]$replication_1

res4 <- list(res3, res3)

dist <- distance

odobs <- od

m <- c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")

# Tests for valid outputs ------------------------------------------------------
test_that("valid output", {
  
  m <- c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")
  val <- gof(sim = res, 
             obs = odobs, 
             measures = m, 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(class(val), "data.frame")
  expect_gt(val$CPC, 0.4)

  m <- c("CPC")
  val <- gof(sim = res, 
             obs = odobs, 
             measures = m, 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(class(val), "data.frame")
  expect_identical(as.numeric(dim(val)[2]), 2)

  m <- c("CPC", "NRMSE")
  val <- gof(sim = res, 
             obs = odobs, 
             measures = m, 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(class(val), "data.frame")
  expect_identical(as.numeric(dim(val)[2]), 3)
  
  m <- c("CPC", "NRMSE")
  val <- gof(sim = res2, 
             obs = odobs, 
             measures = m, 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(class(val), "data.frame")
  expect_identical(as.numeric(dim(val)[2]), 5)
  
  val <- gof(sim = res, 
             obs = odobs, 
             measures = c("all", "CPC"), 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(colnames(val)[-1], 
                   c("CPC", 
                     "NRMSE", 
                     "KL", 
                     "CPL", 
                     "CPC_d", 
                     "KS_stat", 
                     "KS_pval"))
  
  val <- gof(sim = res2, 
             obs = odobs, 
             measures = c("all", "CPC"), 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(dim(val)[1] == 2, TRUE)
  
  val <- gof(sim = res3, 
             obs = odobs, 
             measures = c("all", "CPC"), 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(dim(val)[1] == 1, TRUE)
  
  val <- gof(sim = res4, 
             obs = odobs, 
             measures = c("all", "CPC"), 
             distance = dist, 
             bin_size = 2,
             use_proba = FALSE,
             check_names = TRUE)
  expect_identical(dim(val)[1] == 2, TRUE)
  
})

# Tests for invalid inputs -----------------------------------------------------
test_that("invalid inputs", {
  
  res <- run_law_model(
    law = "GravExp", mass_origin = mi, mass_destination = mj, 
    distance = distance, opportunity = NULL, param = 0.01,
    model = "DCM", nb_trips = NULL, out_trips = Oi, in_trips = Dj, 
    average = FALSE, nbrep = 1, maxiter = 50000, mindiff = 0.00001,
    write_proba = FALSE,
    check_names = TRUE
  )

  m <- c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")
  expect_error(
    gof(sim = res, 
        obs = odobs, 
        measures = m, 
        distance = dist, bin_size = 2,
        use_proba = TRUE,
        check_names = TRUE),
    "use_proba cannot be set to TRUE if there is no proba in sim.",
    fixed = TRUE)
  
  expect_error(
    gof(sim = res, 
        obs = odobs, 
        measures = "test1212", 
        distance = dist, 
        bin_size = 2,
        use_proba = TRUE,
        check_names = TRUE),
    "One or several goodness-of-fit measure(s) chosen are not available.
Please choose from the following:
CPC, CPL, NRMSE, KL, CPC_d, or KS.",
    fixed = TRUE)
  
  expect_error(
    gof(sim = 1, 
        obs = odobs, 
        measures = "CPC", 
        distance = dist, 
        bin_size = 2,
        use_proba = TRUE,
        check_names = TRUE),
    "If sim is not a TDLM object it must be a list or a matrix.",
    fixed = TRUE)
  
  expect_message(
    gof(sim = res4, 
        obs = odobs, 
        measures = "CPC", 
        distance = dist, 
        bin_size = 2,
        use_proba = TRUE,
        check_names = TRUE),
    "^No names identified in the list of matrices.")
  
})
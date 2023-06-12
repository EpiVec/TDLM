# Preamble code ----------------------------------------------------------------
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

dist <- distance

odobs <- od

m <- c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")

# Tests for valid outputs ------------------------------------------------------
test_that("class data.frame and values", {
  res <- run_law_model(
    law = "GravExp", mass_origin = mi, mass_destination = mj, 
    distance = distance, opportunity = NULL, param = 0.01,
    model = "DCM", nb_trips = NULL, out_trips = Oi, in_trips = Dj, 
    average = FALSE, nbrep = 1, maxiter = 50000, mindiff = 0.00001,
    write_proba = TRUE,
    check_names = TRUE
  )

  m <- c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")
  val <- gof(
    sim = res, obs = odobs, measures = m, distance = dist, bin_size = 2,
    use_proba = FALSE,
    check_names = TRUE
  )

  expect_identical(class(val), "data.frame")
  expect_gt(val$CPC, 0.457)

  m <- c("CPC")
  val <- gof(
    sim = res, obs = odobs, measures = m, distance = dist, bin_size = 2,
    use_proba = FALSE,
    check_names = TRUE
  )

  expect_identical(class(val), "data.frame")
  expect_identical(as.numeric(dim(val)[2]), 2)

  m <- c("CPC", "NRMSE")
  val <- gof(
    sim = res, obs = odobs, measures = m, distance = dist, bin_size = 2,
    use_proba = FALSE,
    check_names = TRUE
  )

  expect_identical(class(val), "data.frame")
  expect_identical(as.numeric(dim(val)[2]), 3)
})

# Check errors -----------------------------------------------------
test_that("check errors", {
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
    gof(
      sim = res, obs = odobs, measures = m, distance = dist, bin_size = 2,
      use_proba = TRUE,
      check_names = TRUE
    ),
    "use_proba cannot be set to TRUE if there is no proba in sim."
  )
})
# Preamble code ----------------------------------------------------------------
data(mass)
data(od)

# Prepare inputs
proba = matrix(runif(105*105),105,105)
rownames(proba)=rownames(od)
colnames(proba)=colnames(od)

Oi=as.numeric(mass[,2]) #+0.11
names(Oi)=rownames(od)

Dj=as.numeric(mass[,3]) #+0.11
names(Dj)=rownames(od)

# Tests for valid outputs ------------------------------------------------------
test_that("class TDLM", {
  
  res <- run_model(proba = proba,
                   model = "DCM", nb_trips = NULL, out_trips = Oi, in_trips = Dj, average = TRUE, nbrep = 3, maxiter = 50, mindiff = 0.01,
                   check_names = TRUE)
  
  expect_identical(class(res)[1], "TDLM")
  
})
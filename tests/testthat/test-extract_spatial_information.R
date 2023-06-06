test_that("multiplication works", {
  # Load data 
  data(mass)
  data(distance)
  
  # Prepare inputs
  mi=as.numeric(mass[,1])

  names(mi)=rownames(distance)
  
  mi=mi+0.0011
  
  dist=distance

  sij=extract_opportunities(opportunity=mi, distance=dist, check_names=TRUE)
  
  expect_identical(class(sij)[1], "matrix")
  
})
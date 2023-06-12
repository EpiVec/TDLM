# Check controls ---------------------------------------------------------------
test_that("check controls vectors_positive", {
  
  vec = list()
  
  vec[[1]]=c(1,1)
  #vec$vec = c(1.2,NA)
  #expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive"), 
  #             "NA(s) detected in vec.")
  
  vec$vec = c(1.2,"2")
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive"), 
               "vec must be a numeric vector.")
  
  vec$vec = c(1.2)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive"), 
               "vec must contain at least two locations.")
  
  vec$vec = c(0,0)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive"), 
               "vec must contain at least one strictly positive 
        value.")
  
  vec$vec = c(1.2,-1)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive"), 
               "vec must contain only positive values.")
})

test_that("check controls vectors_positive", {
  
  vec = list()
  
  vec[[1]]=c(1,1)
  
  vec$vec = c(1,"2")
  
  #vec$vec = c(1,NA)
  #expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
  #             "NA(s) detected in vec.")
  
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
               "vec must be a numeric vector.")
  
  vec$vec = c(1.2,1)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
               "vec must be a vector of integers.")
  
  vec$vec = c(-1,0)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
               "vec must contain only positive values.")
  
  vec$vec = c(1)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
               "vec must contain at least two locations.")
  
  vec$vec = c(0,0)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_positive_integer"), 
               "vec must contain at least one strictly positive 
        value.")
  
})

test_that("check controls matrices_positive", {
  
  mat = list()
  
  mat[[1]] = matrix(1,5,5)
  
  mat$mat = matrix(1,5,5)
  #mat$mat[1,1]=NA
  #expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
  #             "NA(s) detected in mat.")
  
  mat$mat = 1
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
               "mat must be a matrix.")
  
  mat$mat = matrix(1,5,6)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
               "mat must be squared.")
  
  mat$mat = matrix(1,1,1)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
               "mat must contain at least two locations.")
  
  mat$mat = matrix(0,5,5)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
               "mat must contain at least one strictly positive 
        value.")
  
  mat$mat = matrix(-1,5,5)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_positive"), 
               "mat must contain only positive values.")  
  
})

test_that("check controls vectors_vectors", {
  
  vec = list()
  
  vec[[1]]=c(1,1)
  
  vec$vec = c(1,1,3)
  
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_vectors"), 
               "The inputs must contain the same number of locations!\n") 

})

test_that("check controls matrices_matrices", {
  
  mat = list()
  
  mat[[1]] = matrix(0,5,5)
  
  mat$mat = matrix(0,6,6)
  
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_matrices"), 
               "The inputs must contain the same number of locations!\n") 
  
})

test_that("check controls vectors_matrices", {

  vec = list()
  vec[[1]] = c(1,1,1)
  vec$vec = c(1,1,3)
  
  mat = list()
  mat[[1]] = matrix(0,5,5)
  mat$mat = matrix(0,5,5)
  
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices"), 
               "The inputs must contain the same number of locations!\n") 
  
})

test_that("check controls vectors_checknames", {
  
  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=1:3
  vec$vec = c(1,1,3)
  
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_checknames"), 
               "The inputs must contain the same number of names id!\n") 
  
  vec[[1]] = c(1,1,1)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_checknames"), 
               "The number of names is lower than the number of locations!")
  
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=c(1,2,3)
  vec$vec = c(1,1,3)
  names(vec$vec)=c(3,2,1)
  expect_error(controls(args = NULL, vectors = vec, matrices = NULL, type = "vectors_checknames"), 
               "Different names in vectors:\n")
  
})

test_that("check controls matrices_checknames", {
  
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,2,3)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_checknames"), 
               "The inputs must contain the same number of names id!\n")
  
  mat[[1]] = matrix(1,3,3)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_checknames"), 
               "The number of names is lower than the number of locations!")
  
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,3,2)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,2,3)
  colnames(mat$mat)=c(1,2,3)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_checknames"), 
               "Different rownames and colnames in:\n")
  
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,3,2)
  colnames(mat[[1]])=c(1,3,2)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,2,3)
  colnames(mat$mat)=c(1,2,3)
  expect_error(controls(args = NULL, vectors = NULL, matrices = mat, type = "matrices_checknames"), 
               "Different names in matrices:\n")
  
})

test_that("check controls vectors_matrices_checknames", {
  
  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=1:3
  vec$vec = c(1,1,3)
  
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,2,3)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "The inputs must contain the same number of names id!\n")
  
  vec[[1]] = c(1,1,1)
  mat[[1]] = matrix(1,3,3)
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "The number of names is lower than the number of locations!")
  
  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=c(1,2,3)
  vec$vec = c(1,1,3)
  names(vec$vec)=c(1,2,3) 
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,3,2)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,2,3)
  colnames(mat$mat)=c(1,2,3)
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "Different rownames and colnames in:\n")
  
  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=c(1,2,3)
  vec$vec = c(1,1,3)
  names(vec$vec)=c(1,2,3) 
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,2,3)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,3,2)
  colnames(mat$mat)=c(1,3,2)
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "Different names in matrices:\n")

  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=c(1,2,3)
  vec$vec = c(1,1,3)
  names(vec$vec)=c(1,3,2) 
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,2,3)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,2,3)
  colnames(mat$mat)=c(1,2,3)
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "Different names in vectors:\n")  
  
  vec = list()
  vec[[1]] = c(1,1,1)
  names(vec[[1]])=c(1,3,2)
  vec$vec = c(1,1,3)
  names(vec$vec)=c(1,3,2) 
  mat = list()
  mat[[1]] = matrix(1,3,3)
  rownames(mat[[1]])=c(1,2,3)
  colnames(mat[[1]])=c(1,2,3)
  mat$mat = matrix(1,3,3)
  rownames(mat$mat)=c(1,2,3)
  colnames(mat$mat)=c(1,2,3)
  expect_error(controls(args = NULL, vectors = vec, matrices = mat, type = "vectors_matrices_checknames"), 
               "Different names in vectors and matrices:\n")  
  
})


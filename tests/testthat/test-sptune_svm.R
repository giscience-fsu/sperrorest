context("sptune_svm")

pacman::p_load(testthat, sperrorest, kernlab, e1071, gmum.r)

# kernlab Sun May 28 12:42:51 2017 ------------------------------

test_that("sp_tune_svm works with kernlab package", {
  kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot", 
               "laplacedot", "besseldot", "anovadot", "splinedot",
               "stringdot")
  data <- ecuador 
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  out <- list()
  for (i in kernels) {
    out[i] <- sptune_svm(fo, ecuador, accelerate = 16, nfold = 5, 
                         partition.fun = "partition.kmeans", kernel = i, 
                         type = "C-svc") 
  }
})

# e1071 Sun May 28 12:43:01 2017 ------------------------------

test_that("sp_tune_svm works with e1071 package", {
  kernels <- c("radial basis", "polynomial", "sigmoid", "linear")
  data <- ecuador 
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  out <- list()
  for (i in kernels) {
    out[i] <- sptune_svm(fo, ecuador, accelerate = 16, nfold = 5, 
                         partition.fun = "partition.kmeans", kernel = i, 
                         type = "C-classification") 
  }
})

# gmum.r Sun May 28 12:43:01 2017 ------------------------------

test_that("sp_tune_svm works with e1071 package", {
  kernels <- c("rbf", "poly", "sigmoid", "linear")
  data <- ecuador 
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  out <- list()
  for (i in kernels) {
    out[i] <- sptune_svm(fo, ecuador, accelerate = 16, nfold = 5, 
                         partition.fun = "partition.kmeans", kernel = i, 
                         type = "C-classification") 
  }
})

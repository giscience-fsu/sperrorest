context("sptune_svm")

pacman::p_load(testthat, sperrorest, kernlab, e1071, gmum.r, purrr)

# kernlab Sun May 28 12:42:51 2017 ------------------------------

test_that("sp_tune_svm works with kernlab package", {
  kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot",
               "laplacedot", "besseldot", "anovadot", "splinedot",
               "stringdot")
  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- map(kernels, function(x)
    sptune_svm(fo, ecuador, accelerate = 8, nfold = 5, svm_fun = "ksvm",
               partition_fun = "partition_kmeans", kernel = x,
               type = "C-svc"))

  out <- sptune_svm(fo, ecuador, accelerate = 2, nfold = 5, svm_fun = "ksvm",
             partition_fun = "partition_kmeans", kernel = "laplacedot",
             type = "C-svc")

})

# e1071 Sun May 28 12:43:01 2017 ------------------------------

test_that("sp_tune_svm works with e1071 package", {
  kernels <- c("radial", "polynomial", "sigmoid", "linear")
  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- map(kernels, function(x)
    sptune_svm(fo, ecuador, accelerate = 8, nfold = 5, svm_fun = "svm",
                         partition_fun = "partition_kmeans", kernel = x,
                         type = "C-classification"))
})

# gmum.r Sun May 28 12:43:01 2017 ------------------------------

test_that("sp_tune_svm works with gmum.r package", {
  kernels <- c("rbf", "poly", "sigmoid", "linear")
  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- map(kernels, function(x)
    sptune_svm(fo, ecuador, accelerate = 8, nfold = 5, svm_fun = "SVM",
                         partition_fun = "partition_kmeans", kernel = x,
                         type = "C-classification"))
})

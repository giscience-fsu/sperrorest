context("sptune_svm")

pacman::p_load(testthat, sperrorest, kernlab, e1071, gmum.r, purrr)

# kernlab Sun May 28 12:42:51 2017 ------------------------------

test_that("sp_tune_svm works with kernlab package", {

   # ---
   ## multiclass classification
   # ---
   fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
         ndvi02 + ndvi03 + ndvi04
   data(maipo)
   out <- sptune_svm(fo, maipo, accelerate = 8, nfold = 5,
                     coords = c("utmx", "utmy"),
                     partition_fun = "partition_kmeans",
                     svm_fun = "ksvm", type = "C-svc", kernel = "rbfdot")

   expect_length(out, 2)
})

# e1071 Sun May 28 12:43:01 2017 ------------------------------

test_that("sp_tune_svm works with e1071 package", {

  # ---
  ## binary classification
  # ---
   data(ecuador) # Muenchow et al. (2012), see ?ecuador
   fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

   out <- sptune_svm(fo, ecuador, accelerate = 8, nfold = 5,
                     partition_fun = "partition_kmeans", svm_fun = "svm",
                     kernel = "sigmoid", type = "C-classification")

  expect_length(out, 2)
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

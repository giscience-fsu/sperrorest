context("sptune_rf")

pacman::p_load(testthat, sperrorest, randomForest, randomForestSRC, purrr)

# randomForest Mon May 29 16:50:09 2017 ------------------------------

test_that("sp_tune_rf works with randomForest package", {

  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, accelerate = 4, nfold = 5,
                   rf_fun = "rfsrc", partition_fun = "partition_kmeans")
})

# randomForestSRC Mon May 29 16:50:19 2017 ------------------------------

test_that("sp_tune_rf works with randomForestSRC package", {

  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, accelerate = 5, nfold = 5,
                   rf_fun = "rfsrc", partition_fun = "partition_kmeans")
})

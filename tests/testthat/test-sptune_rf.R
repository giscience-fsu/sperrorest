context("sptune_rf")

Sys.setenv(R_TESTS = "")

pacman::p_load(testthat, sperrorest, randomForest, randomForestSRC, purrr)

# randomForest Mon May 29 16:50:09 2017 ------------------------------

test_that("sp_tune_rf works with randomForest package", {

  skip_on_travis()
  skip_on_appveyor()

  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, step_factor = 1, nfold = 5,
                   rf_fun = "randomForest", partition_fun = "partition_cv")

  expect_length(out, 2)
})

# randomForestSRC Mon May 29 16:50:19 2017 ------------------------------

test_that("sp_tune_rf works with randomForestSRC package", {

  skip_on_travis()
  skip_on_appveyor()

  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, step_factor = 5, nfold = 5,
                   rf_fun = "rfsrc", partition_fun = "partition_kmeans")

  expect_length(out, 2)
})

test_that("sp_tune_rf works with custom hyperparam range", {

  skip_on_travis()
  skip_on_appveyor()

  data <- ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, nfold = 5,
                   ntree = seq(1, 5), mtry = seq(1, 5),
                   rf_fun = "rfsrc", partition_fun = "partition_kmeans")

  expect_length(out, 2)
})

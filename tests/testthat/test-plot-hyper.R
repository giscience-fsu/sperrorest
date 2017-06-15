context("plot_hyper")

Sys.setenv(R_TESTS = "")

pacman::p_load(sperrorest)

# plot_hyper_rf Fri Jun  9 11:55:51 2017 ------------------------------

test_that("plot_hyper_rf works correctly with 'mtry' < 12", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
                   partition_fun = "partition_kmeans", rf_fun = "randomForest")

  out_plot <- plot_hyper_rf(out)

  expect_length(out_plot, 10)
})

test_that("plot_hyper_rf works correctly with 'mtry' > 12", {
  fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + b25 + b44 + b47
  data(maipo)
  out <- sptune_rf(fo, maipo, accelerate = 32, nfold = 5,
                   coords = c("utmx", "utmy"),
                   partition_fun = "partition_kmeans",
                   rf_fun = "randomForest")

  expect_message(plot_hyper_rf(out))
})

# plot_hyper_svm Mon Jun 12 12:11:12 2017 ------------------------------

test_that("plot_hyper_svm works correctly (multiclass)", {

  # ---
  ## multiclass classification
  # ---

  fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04
  data(maipo)
  out <- sptune_svm(fo, maipo, accelerate = 1, nfold = 5,
                    coords = c("utmx", "utmy"),
                    partition_fun = "partition_kmeans",
                    svm_fun = "ksvm", type = "C-svc", kernel = "rbfdot")

  expect_message(plot_hyper_svm(out))
})

test_that("plot_hyper_svm works correctly (binary)", {

  # ---
  ## binary classification
  # ---

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- sptune_svm(fo, ecuador, accelerate = 1, nfold = 5,
                    partition_fun = "partition_kmeans", svm_fun = "svm",
                    kernel = "sigmoid", type = "C-classification")

  expect_message(plot_hyper_svm(out))

})

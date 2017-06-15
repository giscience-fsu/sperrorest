context("sptune_svm")

Sys.setenv(R_TESTS = "")

pacman::p_load(testthat, sperrorest, kernlab, e1071, gmum.r, purrr,
               magrittr)

# kernlab Sun May 28 12:42:51 2017 ------------------------------

test_that("sp_tune_svm works with kernlab package", {

  # ---
  ## multiclass classification
  # ---
  fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04
  data(maipo)
  out <- sptune_svm(fo, maipo, accelerate = 4, nfold = 5,
                    coords = c("utmx", "utmy"),
                    partition_fun = "partition_kmeans",
                    svm_fun = "ksvm", type = "C-svc", kernel = "rbfdot")

  expect_length(out, 2)
})

test_that("sp_tune_svm works with non-converging
          hyperparameter combinations", {

            readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE/",
                           "mod/survey_data/data-clean.rda")) %>%
              as.data.frame() -> df
            fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope +
              hail_new + age + ph + lithology + soil
            out <- sptune_svm(fo, df, accelerate = 1, nfold = 5,
                              svm_fun = "ksvm",
                              partition_fun = "partition_kmeans",
                              type = "C-svc",
                              kernel = "besseldot")

            expect_lt(length(out$all_gammas), 110)
          })

test_that("sp_tune_svm works with custom hyperparam range", {

  # ---
  ## multiclass classification
  # ---
  fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04
  data(maipo)
  out <- sptune_svm(fo, maipo, accelerate = 1, nfold = 5,
                    coords = c("utmx", "utmy"), cost = seq(1, 5),
                    gamma = seq(1, 5),
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



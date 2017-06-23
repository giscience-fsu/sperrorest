context("processing.R")

pacman::p_load(sperrorest, rpart, MASS, tidyverse, maxnet)

# runfolds Sun May 21 22:58:39 2017 ------------------------------

test_that("runfolds works on missing factor levels in
          test data example", {

            skip("internal use")

            readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE",
                           "/mod/survey_data/data-clean.rda")) %>%
              as_tibble() %>%
              as.data.frame() -> df
            fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail +
              age + ph + lithology + soil

            current_sample <- partition_kmeans(df, nfold = 4)[[1]]
            current_res <- current_sample

            runfolds(j = 1, data = df, current_sample = current_sample,
                     formula = fo, par_mode = "sequential",
                     model_args = list(family = "binomial"),
                     model_fun = glm,
                     importance = TRUE,
                     imp_permutations = 2,
                     imp_variables = c("lithology"),
                     current_res = current_res,
                     pred_args = list(type = "response"),
                     response = "diplo01", par_cl = 2,
                     coords = c("x", "y"), progress = 1,
                     pooled_obs_train = c(),
                     pooled_obs_test = c(),
                     err_fun = err_default) -> runfolds_single
            expect_equal(length(runfolds_single), 6)
          })

test_that("runfolds works on glm example", {

  skip_on_cran()

  j <- 1 # running the first repetition of 'current_sample', normally we are
  # calling an apply call to seq_along nFolds of repetition
  # see also 'runreps()'
  data <- ecuador
  current_sample <- partition_cv(ecuador, nfold = 4)[[1]]
  current_res <- current_sample
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  runfolds(j = 1, data = ecuador, current_sample = current_sample,
           formula = fo,
           model_args = list(family = "binomial"),
           model_fun = glm,
           imp_permutations = 2,
           imp_variables = c("dem", "slope", "hcurv", "vcurv", "log.carea",
                             "cslope"),
           importance = TRUE, current_res = current_res,
           pred_args = list(type = "response"), response = "slides", par_cl = 2,
           coords = c("x", "y"), progress = 1, pooled_obs_train = c(),
           pooled_obs_test = c(), err_fun = err_default) -> runfolds_single
  expect_equal(length(runfolds_single), 6)
})

test_that("runfolds works on LDA example", {

  skip_on_cran()

  data <- ecuador
  current_sample <- partition_cv(maipo, nfold = 4)[[1]]
  current_res <- current_sample

  lda_predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority_filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[ fac == lev ] <- majority(x[ fac == lev ])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[,fac])
    return(pred)
  }

  data("maipo", package = "sperrorest")
  predictors <- colnames(maipo)[5:ncol(maipo)]
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))

  runfolds_single <- runfolds(j = 1, data = maipo,
                              current_sample = current_sample,
                              formula = fo, par_mode = "foreach",
                              model_fun = lda,
                              pred_fun = lda_predfun,
                              pred_args = list(fac = "field"),
                              importance = FALSE,
                              current_res = current_sample,
                              response = "croptype", par_cl = 2,
                              coords = c("x", "y"), progress = 1,
                              pooled_obs_train = c(),
                              pooled_obs_test = c(), err_fun = err_default)
  expect_equal(length(runfolds_single), 6)
})

test_that("runfolds works on rpart example", {

  skip_on_cran()

  data <- ecuador
  current_sample <- partition_cv(ecuador, nfold = 4)[[1]]
  current_res <- current_sample

  mypred_rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred_rpart <- function(object, newdata) predict(object, newdata)[,2]

  runfolds_single <- runfolds(j = 1, data = ecuador,
                              current_sample = current_sample,
                              formula = slides ~ dem + slope + hcurv +
                                vcurv + log.carea + cslope,
                              model_fun = rpart,
                              imp_permutations = 2, pred_fun = mypred_rpart,
                              model_args = list(control = ctrl),
                              imp_variables = c("dem", "slope", "hcurv",
                                                "vcurv", "log.carea", "cslope"),
                              importance = TRUE,
                              current_res = current_res,
                              response = "slides", par_cl = 2,
                              coords = c("x", "y"), progress = 1,
                              pooled_obs_train = c(),
                              pooled_obs_test = c(), err_fun = err_default)
  expect_equal(length(runfolds_single), 6)
})

test_that("runfolds works on maxnet example", {

  skip("internal use")

  readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE",
                 "/mod/survey_data/data-clean.rda")) %>%
    as_tibble() %>%
    as.data.frame() -> df
  fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail_new +
    age + ph + lithology + soil

  maxnet_modfun <- function(data = data, formula = formula) {

    data %>%
      dplyr::select(-id, -fus01, -x, -y, -hail, -diplo01, -geometry, -year) %>%
      as.data.frame() -> maxent_pred

    data %>%
      dplyr::select(diplo01) %>%
      .$diplo01 -> maxent_response

    maxent_pred %>%
      dplyr::mutate(lithology = forcats::fct_recode(lithology,
                                                    "1" = "Depsitos superficiales",
                                                    "2" = "Detrticos alternantes",
                                                    "3" = "Margas descarbonatadas",
                                                    "4" = "Calizas impuras y calcarenitas",
                                                    "5" = "Rocas volcnicas piroclsticas",
                                                    "6" = "Rocas volcnicas en coladas",
                                                    "7" = "Arcillas con yesos y otras sales",
                                                    "8" = "Alternancia de margocalizas margas calizas y calcarenitas",
                                                    "9" = "Granitos de grano grueso",
                                                    "10" = "Areniscas",
                                                    "11" = "Limolitas",
                                                    "12" = "Lutitas",
                                                    "13" = "Margas",
                                                    "14" = "Calizas",
                                                    "15" = "Ofitas",
                                                    "16" = "Pizarras",
                                                    "17" = "Granodioritas")) %>%
      dplyr::mutate(soil = forcats::fct_recode(soil,
                                               "1" = "Cambisols",
                                               "2" = "Chernozems",
                                               "3" = "Cryosols",
                                               "4" = "Durisols",
                                               "5" = "Ferralsols",
                                               "6" = "Fluvisols",
                                               "7" = "Gleysols",
                                               "8" = "Gypsisols",
                                               "9" = "Histosols",
                                               "10" = "Kastanozems",
                                               "11" = "Leptosols",
                                               "12" = "Lixisols",
                                               "13" = "Luvisols")) -> maxent_pred

    # maxnet needs a numeric response
    maxnet_response <- as.numeric(levels(maxent_response))[maxent_response]

    maxnet_out <- maxnet(data = maxent_pred,
                         p = maxnet_response)

    return(maxnet_out)
  }

  maxnet_predfun <- function(object = NULL, newdata = NULL, type = NULL) {
    pred <- predict(object = object, newdata = newdata, type = type)
    pred <- pred[, 1]
    return(pred)
  }

  current_sample <- partition_kmeans(df, nfold = 5, seed1 = 12345)[[1]]
  current_res <- current_sample

  runfolds_single <- runfolds(j = 1, data = df,
                              current_sample = current_sample,
                              formula = fo,
                              model_fun = maxnet_modfun,
                              pred_fun = maxnet_predfun,
                              pred_args = list(type = "logistic"),
                              importance = TRUE,
                              imp_variables = c("elevation", "lithology"),
                              imp_permutations = 2,
                              par_mode = "sequential",
                              current_res = current_res,
                              response = "diplo01", par_cl = 2,
                              coords = c("x", "y"), progress = 1,
                              pooled_obs_train = c(),
                              pooled_obs_test = c(), err_fun = err_default)
})

# runreps Sun May 21 23:07:03 2017 ------------------------------

test_that("runreps works on lda example", {

  skip_on_cran()

  j <- 1 # running the first repetition of 'current_sample', normally we are
  # calling an apply call to seq_along nFolds of repetition
  # see also 'runreps()'
  #### 2 repetitions, 4 folds
  current_sample <- partition_cv(maipo, nfold = 4)
  current_sample[[2]] <- partition_cv(maipo, nfold = 4)[[1]]
  current_res <- current_sample

  lda_predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority_filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[fac == lev] <- majority(x[fac == lev])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[,fac])
    return(pred)
  }

  predictors <- colnames(maipo)[5:ncol(maipo)]
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))

  runreps_res <- lapply(current_sample, function(x)
    runreps(current_sample = x, data = maipo,
            formula = fo, par_mode = "apply", pred_fun = lda_predfun,
            model_fun = lda,
            do_gc = 1,
            importance = FALSE, current_res = current_res,
            pred_args = list(fac = "field"), response = "croptype", par_cl = 2,
            coords = c("x", "y"), progress = 1, pooled_obs_train = c(),
            pooled_obs_test = c(), err_fun = err_default))
  expect_equal(length(runreps_res), 2)

})

test_that("runreps works on glm example", {

  skip("internal use")

  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  current_sample <- readRDS("inst/test-objects/resamp.rda")
  current_res <- readRDS("inst/test-objects/current_res.rda")

  runreps_res <- lapply(current_sample, function(x)
    runreps(current_sample = X, data = ecuador,
            formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope,
            model_args = list(family = "binomial"),
            model_fun = glm,
            imp_permutations = 2,
            do_gc = 1,
            imp_variables = c("dem", "slope", "hcurv", "vcurv",
                              "log.carea", "cslope"),
            importance = TRUE, current_res = current_res,
            pred_args = list(type = "response"), response = "slides",
            par_cl = 2,
            coords = c("x", "y"), progress = 1, pooled_obs_train = c(),
            pooled_obs_test = c(), err_fun = err_default))
})

test_that("runreps works on rpart example", {

  skip("internal use")

  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  current_sample <- readRDS("inst/test-objects/resamp.rda")
  current_res <- readRDS("inst/test-objects/current_res.rda")

  runreps_res <- lapply(current_sample, function(x)
    runreps(current_sample = X, data = ecuador,
            formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope,
            model_fun = rpart,
            imp_permutations = 2,
            pred_fun = mypred_rpart,
            model_args = list(control = ctrl),
            imp_variables = c("dem", "slope", "hcurv", "vcurv",
                              "log.carea", "cslope"),
            importance = TRUE, current_res = current_res,
            response = "slides", par_cl = 2,
            coords = c("x", "y"), progress = 1, pooled_obs_train = c(),
            pooled_obs_test = c(), err_fun = err_default))

})

test_that("runfolds works on missing factor levels in
          test data example", {

            skip("internal use")

            readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE",
                           "/mod/survey_data/data-clean.rda")) %>%
              as_tibble() %>%
              as.data.frame() -> df
            fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail +
              age + ph + lithology + soil

            current_sample <- partition_kmeans(df, nfold = 5, repetition = 1:5)
            current_res <- current_sample

            runreps_res <- lapply(current_sample, function(x)
              runreps(current_sample = x, do_gc = 1,
                      formula = fo, par_mode = "sequential", data = df,
                      model_args = list(family = "binomial"),
                      model_fun = glm,
                      importance = TRUE,
                      imp_variables = c("elevation", "lithology"),
                      imp_permutations = 2,
                      current_res = current_res,
                      pred_args = list(type = "response"), response = "diplo01",
                      par_cl = 2,
                      coords = c("x", "y"), progress = 1,
                      pooled_obs_train = c(),
                      pooled_obs_test = c(), err_fun = err_default))
            expect_equal(length(runfolds_single), 6)
          })

test_that("runfolds works on maxnet example", {

            skip("internal use")

            readRDS(paste0("/Users/pjs/Servers/GIServer/home/shares/data/LIFE",
                           "/mod/survey_data/data-clean.rda")) %>%
              as_tibble() %>%
              as.data.frame() -> df
            fo <- diplo01 ~ temp + p_sum + r_sum + elevation + slope + hail_new +
              age + ph + lithology + soil

            maxnet_modfun <- function(data = data, formula = formula) {

              data %>%
                dplyr::select(-id, -fus01, -x, -y, -hail, -diplo01,
                              -geometry, -year) %>%
                as.data.frame() -> maxent_pred

              data %>%
                dplyr::select(diplo01) %>%
                .$diplo01 -> maxent_response

              maxent_pred %>%
                dplyr::mutate(lithology = forcats::fct_recode(lithology,
                                                              "1" = "Depsitos superficiales",
                                                              "2" = "Detrticos alternantes",
                                                              "3" = "Margas descarbonatadas",
                                                              "4" = "Calizas impuras y calcarenitas",
                                                              "5" = "Rocas volcnicas piroclsticas",
                                                              "6" = "Rocas volcnicas en coladas",
                                                              "7" = "Arcillas con yesos y otras sales",
                                                              "8" = "Alternancia de margocalizas margas calizas y calcarenitas",
                                                              "9" = "Granitos de grano grueso",
                                                              "10" = "Areniscas",
                                                              "11" = "Limolitas",
                                                              "12" = "Lutitas",
                                                              "13" = "Margas",
                                                              "14" = "Calizas",
                                                              "15" = "Ofitas",
                                                              "16" = "Pizarras",
                                                              "17" = "Granodioritas")) %>%
                dplyr::mutate(soil = forcats::fct_recode(soil,
                                                         "1" = "Cambisols",
                                                         "2" = "Chernozems",
                                                         "3" = "Cryosols",
                                                         "4" = "Durisols",
                                                         "5" = "Ferralsols",
                                                         "6" = "Fluvisols",
                                                         "7" = "Gleysols",
                                                         "8" = "Gypsisols",
                                                         "9" = "Histosols",
                                                         "10" = "Kastanozems",
                                                         "11" = "Leptosols",
                                                         "12" = "Lixisols",
                                                         "13" = "Luvisols")) -> maxent_pred

              # maxnet needs a numeric response
              maxnet_response <- as.numeric(levels(maxent_response))[maxent_response]

              maxnet_out <- maxnet(data = maxent_pred,
                                   p = maxnet_response)

              return(maxnet_out)
            }

            maxnet_predfun <- function(object = NULL, newdata = NULL, type = NULL) {
              pred <- predict(object = object, newdata = newdata, type = type)
              pred <- pred[, 1]
              return(pred)
            }

            current_sample <- partition_kmeans(df, nfold = 5, repetition = 1:5,
                                               seed1 = 12345)
            current_res <- current_sample

            runreps_res <- lapply(current_sample, function(x)
              runreps(current_sample = x, do_gc = 1,
                      formula = fo, par_mode = "sequential", data = df,
                      model_fun = maxnet_modfun,
                      importance = TRUE,
                      imp_variables = c("elevation", "lithology"),
                      imp_permutations = 2,
                      current_res = current_res,
                      pred_fun = maxnet_predfun,
                      pred_args = list(type = "logistic"), response = "diplo01",
                      par_cl = 2,
                      coords = c("x", "y"), progress = 1,
                      pooled_obs_train = c(),
                      pooled_obs_test = c(), err_fun = err_default))
            expect_equal(length(runfolds_single), 6)
          })

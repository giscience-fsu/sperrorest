context("sperrorest.R")

test_that("output type (= list) for different logical combinations of
          error_rep and error_fold for par_mode = 'foreach' on LDA example", {
  library("MASS")

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
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
    return(pred)
  }

  fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
    b24 +
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
    ndwi08

  data(maipo)

  out <- sperrorest(fo,
    data = maipo, coords = c("utmx", "utmy"),
    model_fun = lda,
    pred_fun = lda_predfun,
    smp_fun = partition_cv,
    smp_args = list(repetition = 1:2, nfold = 4),
    benchmark = TRUE, progress = FALSE
  )

  expect_equal(typeof(out$error_rep), "list")
  expect_equal(typeof(out$error_fold), "list")
  # check that train_error is first
  expect_equal(names(out$error_rep)[[1]], "train_error")
})

test_that("output length of list is correct for par_mode = 'foreach' on rpart
          example", {

            library("rpart")
  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  mypred_rpart <- function(object, newdata) {
    predict(
      object,
      newdata
    )[, 2]
  }
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred_rpart <- function(object, newdata) {
    predict(
      object,
      newdata
    )[, 2]
  }

  out <- sperrorest(
    data = ecuador, formula = fo,
    model_fun = rpart,
    model_args = list(control = ctrl),
    pred_fun = mypred_rpart,
    progress = FALSE,
    smp_fun = partition_cv,
    smp_args = list(
      repetition = 1:2,
      nfold = 2
    )
  )

  expect_equal(length(out$error_fold[[1]]), 2)
})

# variable importance Wed Feb  8 21:59:03 2017

test_that("sperrorest() variable importance (foreach)", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm,
      model_args = list(family = "binomial"),
      pred_fun = predict,
      pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:1, nfold = 4),
      benchmark = TRUE,
      importance = TRUE, imp_permutations = 10,
      progress = FALSE
    )
  )
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})


# binary response Wed Feb  8 22:43:12 2017

test_that("sperrorest() produces correct output for binary response", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm, model_args = list(family = "binomial"),
      pred_fun = predict, pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 2),
      benchmark = TRUE,
      importance = FALSE,
      imp_permutations = 2,
      progress = FALSE
    )
  )

  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
})

test_that("sperrorest() when pred_fun = NULL", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm, model_args = list(family = "binomial"),
      pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4),
      benchmark = TRUE,
      importance = TRUE,
      imp_permutations = 10,
      progress = FALSE
    )
  )
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
  # check for importance object
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})

test_that("sperrorest correctly updates resampling object when using a
          sub-sample", {
  data(ecuador)

  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm,
      model_args = list(family = binomial),
      test_fun = resample_strat_uniform,
      test_param = list(strat = "slides", nstrat = Inf),
      train_fun = resample_strat_uniform,
      train_param = list(strat = "slides", nstrat = Inf),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4),
      importance = FALSE,
      progress = FALSE
    )
  )

  expect_lt(length(out[["represampling"]][["1"]][["1"]][["test"]]), 150)
})

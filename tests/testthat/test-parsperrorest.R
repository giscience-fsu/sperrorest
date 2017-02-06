context("sperrorest/parsperrorest.R")

Sys.unsetenv("R_TESTS")

pacman::p_load(sperrorest, testthat, rpart, MASS, doParallel, foreach)

# parsperrorest par.mode = 2 Mon Feb  6 23:24:11 2017 ------------------------------

test_that("output type (= list) for different logical combinations of 
          error.rep and error.fold for par.mode = 2 on LDA example", {
  testthat::skip_on_cran()

  lda.predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority.filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[ fac == lev ] <- majority(x[ fac == lev ])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac])
    return(pred)
  }

  fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + b24 +
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
    ndwi08

  # err.rep = TRUE, err.fold = TRUE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 2, par.units = 1),
                       error.rep = TRUE, error.fold = TRUE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "list")

  # err.rep = TRUE, err.fold = FALSE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 2, par.units = 2),
                       error.rep = TRUE, error.fold = FALSE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "NULL")

  # err.rep = FALSE, err.fold = TRUE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 2, par.units = 2),
                       error.rep = FALSE, error.fold = TRUE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "NULL")
  expect_equal(typeof(out$error.fold), "list")
})


test_that("output length of list is correct for error.rep = TRUE and error.fold  = TRUE 
          for par.mode = 2 on rpart example", {
            
  testthat::skip_on_cran()

  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
  par.nsp.res <- parsperrorest(data = ecuador, formula = fo,
                               model.fun = rpart, model.args = list(control = ctrl),
                               pred.fun = mypred.rpart,
                               progress = FALSE,
                               smp.fun = partition.cv,
                               smp.args = list(repetition = 1:2, nfold = 4),
                               par.args = list(par.mode = 2, par.units = 2),
                               error.rep = TRUE, error.fold = TRUE)

  expect_equal(length(par.nsp.res$error.fold[[1]]), 4)
})

# parsperrorest par.mode = 1 Mon Feb  6 23:25:08 2017 ------------------------------


test_that("output type (= list) for different logical combinations of 
          error.rep and error.fold for par.mode = 1 on LDA example", {

  testthat::skip_on_cran()

  lda.predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority.filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[ fac == lev ] <- majority(x[ fac == lev ])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac])
    return(pred)
  }

  fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + b24 +
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
    ndwi08

  # err.rep = TRUE, err.fold = TRUE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 1, par.units = 2),
                       error.rep = TRUE, error.fold = TRUE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "list")

  # err.rep = TRUE, err.fold = FALSE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 1, par.units = 2),
                       error.rep = TRUE, error.fold = FALSE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "NULL")

  # err.rep = FALSE, err.fold = TRUE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 1, par.units = 2),
                       error.rep = FALSE, error.fold = TRUE,
                       benchmark = TRUE, progress = FALSE)

  expect_equal(typeof(out$error.rep), "NULL")
  expect_equal(typeof(out$error.fold), "list")

})

test_that("output length of list is correct for error.rep = TRUE and error.fold  = TRUE 
          for par.mode = 1 on rpart example", {
  
  testthat::skip_on_cran()
            
  data(ecuador) 
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  # Example of a classification tree fitted to this data:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)
  
  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
  par.nsp.res <- parsperrorest(data = ecuador, formula = fo,
                               model.fun = rpart, model.args = list(control = ctrl),
                               pred.fun = mypred.rpart,
                               progress = FALSE, 
                               smp.fun = partition.cv,
                               smp.args = list(repetition = 1:2, nfold = 4),
                               par.args = list(par.mode = 1, par.units = 2),
                               error.rep = TRUE, error.fold = TRUE)
  
  expect_equal(length(par.nsp.res$error.fold[[1]]), 4)
})

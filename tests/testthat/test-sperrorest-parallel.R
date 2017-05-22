context("sperrorest.R")

Sys.setenv(R_TESTS = "")

pacman::p_load(sperrorest, testthat, rpart, MASS, doParallel, foreach, doFuture)

# par.mode = "foreach" Mon Feb  6 23:24:11 2017 --------------------------

test_that("output type (= list) for different logical combinations of 
          error.rep and error.fold for par.mode = 2 on LDA example", {
            
            lda.predfun <- function(object, newdata, fac = NULL) {
              library(nnet)
              majority <- function(x) {
                levels(x)[which.is.max(table(x))]
              }
              
              majority.filter <- function(x, fac) {
                for (lev in levels(fac)) {
                  x[fac == lev] <- majority(x[fac == lev])
                }
                x
              }
              
              pred <- predict(object, newdata = newdata)$class
              if (!is.null(fac)) pred <- majority.filter(pred, newdata[, fac])
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
            
            data(maipo)
            
            # err.rep = TRUE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 #par.args = list(par.mode = "sequential"),
                                 smp.args = list(repetition = 1:10, nfold = 4),
                                 error.rep = TRUE, error.fold = TRUE,
                                 benchmark = TRUE, progress = T)
            
            expect_equal(typeof(out$error.rep), "list")
            expect_equal(typeof(out$error.fold), "list")
            expect_equal(names(out$error.rep)[[1]], "train.error") # check that train.error is first
            
            # err.rep = TRUE, err.fold = FALSE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:6, nfold = 5),
                                 par.args = list(par.mode = "sequential"),
                                 error.rep = TRUE, error.fold = FALSE,
                                 benchmark = TRUE, progress = TRUE)
            
            expect_equal(typeof(out$error.rep), "list")
            expect_equal(typeof(out$error.fold), "NULL")
            
            # err.rep = FALSE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:2, nfold = 2),
                                 par.args = list(par.mode = "foreach", par.units = 2),
                                 error.rep = FALSE, error.fold = TRUE,
                                 benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error.rep), "NULL")
            expect_equal(typeof(out$error.fold), "list")
          })


test_that("output length of list is correct for error.rep = TRUE and error.fold  = TRUE 
          for par.mode = 2 on rpart example", {
            
            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)
            
            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                         model.fun = rpart, model.args = list(control = ctrl),
                                         pred.fun = mypred.rpart,
                                         progress = FALSE,
                                         smp.fun = partition.cv,
                                         smp.args = list(repetition = 1:2, nfold = 2),
                                         par.args = list(par.mode = "foreach", par.units = 2),
                                         error.rep = TRUE, error.fold = TRUE)
            
            expect_equal(length(par.nsp.res$error.fold[[1]]), 2)
          })

# variable importance Wed Feb  8 21:59:03 2017 

test_that("sperrorest() variable importance with error.rep = T and error.fold = T", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  nspres <- sperrorest(data = ecuador, formula = fo,
                          model.fun = glm, model.args = list(family = "binomial"),
                          pred.fun = predict, pred.args = list(type = "response"),
                          smp.fun = partition.cv,
                          smp.args = list(repetition = 1:2, nfold = 4),
                          par.args = list(par.mode = "foreach", par.units = 2),
                          benchmark = TRUE, 
                          importance = TRUE, imp.permutations = 10)
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
})

test_that("sperrorest() variable importance with error.rep = F and error.fold = T", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                          model.fun = glm, model.args = list(family = "binomial"),
                          pred.fun = predict, pred.args = list(type = "response"),
                          smp.fun = partition.cv,
                          smp.args = list(repetition = 1:2, nfold = 4),
                          par.args = list(par.mode = "sequential", par.units = 2),
                          benchmark = TRUE, error.rep = FALSE, 
                          importance = TRUE, imp.permutations = 10)
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
})

# binary response Wed Feb  8 22:43:12 2017 

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                          model.fun = glm, model.args = list(family = "binomial"),
                          pred.fun = predict, pred.args = list(type = "response"),
                          smp.fun = partition.cv,
                          smp.args = list(repetition = 1:2, nfold = 2),
                          par.args = list(par.mode = "sequential", par.units = 2),
                          benchmark = TRUE, 
                          importance = FALSE, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  expect_equal(names(nspres$error.rep)[[1]], "train.auroc") # check for train.auroc for binary response
})

test_that("sperrorest() when pred.fun = NULL", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                          model.fun = glm, model.args = list(family = "binomial"),
                          pred.args = list(type = "response"),
                          smp.fun = partition.cv,
                          smp.args = list(repetition = 1:2, nfold = 4),
                          par.args = list(par.mode = "foreach", par.units = 2),
                          benchmark = TRUE,
                          importance = TRUE, imp.permutations = 10)
  summary.rep <- summary(nspres$error.rep)
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  expect_equal(names(nspres$error.rep)[[1]], "train.auroc") # check for train.auroc for binary response
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame") # check for importance object
})

# par.mode = "future" Mon Feb  6 23:25:08 2017 ------------------------------


test_that("output type (= list) for different logical combinations of 
          error.rep and error.fold for par.mode = 'future' on LDA example", {
            
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
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:2, nfold = 2),
                                 par.args = list(par.mode = "future", par.units = 2),
                                 error.rep = TRUE, error.fold = TRUE,
                                 benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error.rep), "list")
            expect_equal(typeof(out$error.fold), "list")
            expect_equal(names(out$error.rep)[[1]], "train.error") # check for train.error existence
            
            # err.rep = TRUE, err.fold = FALSE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:2, nfold = 2),
                                 par.args = list(par.mode = "future", par.units = 2),
                                 error.rep = TRUE, error.fold = FALSE,
                                 benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error.rep), "list")
            expect_equal(typeof(out$error.fold), "NULL")
            
            # err.rep = FALSE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:2, nfold = 2),
                                 par.args = list(par.mode = "future", par.units = 2),
                                 error.rep = FALSE, error.fold = TRUE,
                                 benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error.rep), "NULL")
            expect_equal(typeof(out$error.fold), "list")
            
          })

test_that("do.try argument", {
  
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
  out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 2),
                       par.args = list(par.mode = "future", par.units = 2),
                       error.rep = TRUE, error.fold = TRUE,
                       benchmark = TRUE, progress = FALSE,
                       do.try = T)
  
  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "list")
  expect_equal(names(out$error.rep)[[1]], "train.error") # check for train.error existence
  
})

test_that("output length of list is correct for error.rep = TRUE and error.fold  = TRUE 
          for par.mode = 'apply' on rpart example", {
            
            data(ecuador) 
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            # Example of a classification tree fitted to this data:
            mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)
            
            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                         model.fun = rpart, model.args = list(control = ctrl),
                                         pred.fun = mypred.rpart,
                                         progress = FALSE, 
                                         smp.fun = partition.cv,
                                         smp.args = list(repetition = 1:2, nfold = 2),
                                         par.args = list(par.mode = "apply", par.units = 2),
                                         error.rep = TRUE, error.fold = TRUE)
            
            expect_equal(length(par.nsp.res$error.fold[[1]]), 2)
          })

# par.mode = "apply" variable importance Tue Feb 21 22:15:41 2017 ------------------------------

test_that("par.mode = 'apply' works with var.imp", {
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                          model.fun = glm, model.args = list(family = "binomial"),
                          pred.args = list(type = "response"),
                          smp.fun = partition.cv,
                          smp.args = list(repetition = 1:2, nfold = 4),
                          par.args = list(par.mode = "apply", par.units = 2),
                          benchmark = TRUE,
                          importance = TRUE, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  expect_equal(names(nspres$error.rep)[[1]], "train.auroc") # check for train.auroc for binary response
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame") # check for importance object
})

### manual

# notify argument Tue Feb  7 13:45:45 2017 

test_that("notify badge is working in sperrorest()", {

  testthat::skip_on_cran() # "because of 'notify=TRUE'"
  testthat::skip("notifier integration removed")

  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  # Example of a classification tree fitted to this data:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
  par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                               model.fun = rpart, model.args = list(control = ctrl),
                               pred.fun = mypred.rpart,
                               progress = FALSE,
                               smp.fun = partition.cv,
                               smp.args = list(repetition = 1:2, nfold = 2),
                               par.args = list(par.mode = "apply", par.units = 2),
                               error.rep = TRUE, error.fold = TRUE, notify = TRUE)

  expect_equal(length(par.nsp.res$error.fold[[1]]), 2)
})

test_that("notify without benchmark = TRUE", {
  
  testthat::skip("notifier integration removed")

  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  # Example of a classification tree fitted to this data:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
  par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                               model.fun = rpart, model.args = list(control = ctrl),
                               pred.fun = mypred.rpart,
                               progress = FALSE,
                               smp.fun = partition.cv, benchmark = FALSE,
                               smp.args = list(repetition = 1:2, nfold = 2),
                               par.args = list(par.mode = "apply", par.units = 2),
                               error.rep = TRUE, error.fold = TRUE, notify = TRUE)

  expect_equal(length(par.nsp.res$error.fold[[1]]), 2)
})

# sperrorest warnings Thu Feb  9 22:34:08 2017 

test_that("importance = T and err.fold = F", { 
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                            model.fun = glm,
                            pred.fun = predict,
                            smp.fun = partition.cv, 
                            smp.args = list(repetition = 1:2, nfold = 2),
                            par.args = list(par.mode = "apply", par.units = 2),
                            importance = TRUE, error.fold = FALSE))
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                            model.fun = glm,
                            pred.fun = predict,
                            smp.fun = partition.cv, 
                            smp.args = list(repetition = 1:2, nfold = 2),
                            par.args = list(par.mode = "apply", par.units = 2),
                            someargument = NULL))
})

# sperrorest depr. args Thu Feb  9 22:42:48 2017 

test_that("deprecated args", { 
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          predfun = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          silent = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          err.pooled = NULL))
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          err.unpooled = NULL))
})

# partition.factor.cv mit custom pred.fun Sun Feb 19 09:36:26 2017 

test_that("partition.factor.cv works (LDA)", {
  
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
  
  data("maipo", package = "sperrorest")
  
  predictors <- colnames(maipo)[5:ncol(maipo)]
  # Construct a formula:
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))
  
  res.lda.sp.par <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                  model.fun = lda,
                                  pred.fun = lda.predfun,
                                  pred.args = list(fac = "field"),
                                  smp.fun = partition.factor.cv,
                                  smp.args = list(fac = "field", repetition = 1:2, nfold = 2),
                                  par.args = list(par.units = 2, par.mode = "foreach"),
                                  error.rep = TRUE, error.fold = TRUE, progress = "TRUE",
                                  benchmark = TRUE)
  
})

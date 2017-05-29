context("sperrorest.R")

Sys.setenv(R_TESTS = "")

pacman::p_load(sperrorest, testthat, rpart, MASS, foreach, doFuture, future, 
               pbmcapply)

# par_mode = "foreach" Mon Feb  6 23:24:11 2017 --------------------------

test_that("output type (= list) for different logical combinations of 
          error_rep and error_fold for par_mode = 'foreach' on LDA example", {
            
            skip_on_cran()
            
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
            
            # err.rep = TRUE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              error_rep = TRUE, error_fold = TRUE,
                              benchmark = TRUE, progress = T)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            # check that train.error is first
            expect_equal(names(out$error_rep)[[1]], "train.error") 
            
            # err.rep = TRUE, err.fold = FALSE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 5),
                              par_args = list(par_mode = "foreach", 
                                              par_units = 2),
                              error_rep = TRUE, error_fold = FALSE,
                              benchmark = TRUE, progress = TRUE)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "NULL")
            
            # err.rep = FALSE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 2),
                              par_args = list(par_mode = "foreach", 
                                              par_units = 2),
                              error_rep = FALSE, error_fold = TRUE,
                              benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error_rep), "NULL")
            expect_equal(typeof(out$error_fold), "list")
          })


test_that("output length of list is correct for error_rep = TRUE and 
          error_fold  = TRUE for par_mode = 'foreach' on rpart example", {
            
            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            mypred.rpart <- function(object, newdata) predict(object, 
                                                              newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)
            
            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred.rpart <- function(object, newdata) predict(object, 
                                                              newdata)[,2]
            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                      model_fun = rpart, 
                                      model_args = list(control = ctrl),
                                      pred_fun = mypred.rpart,
                                      progress = FALSE,
                                      smp_fun = partition_cv,
                                      smp_args = list(repetition = 1:2, 
                                                      nfold = 2),
                                      par_args = list(par_mode = "foreach", 
                                                      par_units = 2),
                                      error_rep = TRUE, error_fold = TRUE)
            
            expect_equal(length(par.nsp.res$error_fold[[1]]), 2)
          })

# variable importance Wed Feb  8 21:59:03 2017 

test_that("sperrorest() variable importance with error_rep = T and 
          error_fold = T", {
            
            skip_on_cran()
            
            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm, 
                                 model_args = list(family = "binomial"),
                                 pred_fun = predict, 
                                 pred_args = list(type = "response"),
                                 smp_fun = partition_cv,
                                 smp_args = list(repetition = 1:2, nfold = 4),
                                 par_args = list(par_mode = "foreach", par_units = 2),
                                 benchmark = TRUE, 
                                 importance = TRUE, imp_permutations = 10)
            expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
          })

test_that("sperrorest() variable importance with error_rep = F and error_fold = T", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_fun = predict, pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 4),
                       par_args = list(par_mode = "foreach", par_units = 2),
                       benchmark = TRUE, error_rep = FALSE, 
                       importance = TRUE, imp_permutations = 10)
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
})

# binary response Wed Feb  8 22:43:12 2017 

test_that("sperrorest() produces correct output for binary response", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_fun = predict, pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 2),
                       par_args = list(par_mode = "foreach", par_units = 2),
                       benchmark = TRUE, 
                       importance = FALSE, imp_permutations = 2)
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  # check for train.auroc for binary response
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc") 
})

test_that("sperrorest() when pred_fun = NULL", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 4),
                       par_args = list(par_mode = "foreach", par_units = 2),
                       benchmark = TRUE,
                       importance = TRUE, imp_permutations = 10)
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  # check for train.auroc for binary response
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc") 
  # check for importance object
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame") 
})

# par_mode = "future" Mon Feb  6 23:25:08 2017 ------------------------------


test_that("output type (= list) for different logical combinations of 
          error_rep and error_fold for par_mode = 'future' on LDA example", {
            
            skip_on_cran()
            
            skip("par_mode = 'future' does not work on LDA example")
            
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
            
            fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + 
              b24 +
              b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
              b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
              b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
              b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
              ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
              ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
              ndwi08
            
            # err.rep = TRUE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:4, nfold = 2),
                              par_args = list(par_mode = "future", 
                                              par_units = 2),
                              error_rep = TRUE, error_fold = TRUE,
                              benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            expect_equal(names(out$error_rep)[[1]], "train.error") # check for train.error existence
            
            # err.rep = TRUE, err.fold = FALSE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 2),
                              par_args = list(par_mode = "future", par_units = 2),
                              error_rep = TRUE, error_fold = FALSE,
                              benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "NULL")
            
            # err.rep = FALSE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 2),
                              par_args = list(par_mode = "future", par_units = 2),
                              error_rep = FALSE, error_fold = TRUE,
                              benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error_rep), "NULL")
            expect_equal(typeof(out$error_fold), "list")
            
          })

test_that("do.try argument", {
  
  skip_on_cran()
  
  skip("par_mode = 'future' does not work on LDA example")
  
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
                    model_fun = lda,
                    pred_fun = lda.predfun,
                    smp_fun = partition_cv,
                    smp_args = list(repetition = 1:2, nfold = 2),
                    par_args = list(par_mode = "future", par_units = 2),
                    error_rep = TRUE, error_fold = TRUE,
                    benchmark = TRUE, progress = FALSE,
                    do.try = T)
  
  expect_equal(typeof(out$error_rep), "list")
  expect_equal(typeof(out$error_fold), "list")
  expect_equal(names(out$error_rep)[[1]], "train.error") # check for train.error existence
  
})

test_that("output length of list is correct for error_rep = TRUE and error_fold  = TRUE 
          for par_mode = 'future' on rpart example", {
            
            skip_on_cran()
            
            data(ecuador) 
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            # Example of a classification tree fitted to this data:
            mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)
            
            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                      model_fun = rpart, model_args = list(control = ctrl),
                                      pred_fun = mypred.rpart,
                                      progress = FALSE, 
                                      smp_fun = partition_cv,
                                      smp_args = list(repetition = 1:2, nfold = 2),
                                      par_args = list(par_mode = "future", par_units = 2),
                                      error_rep = TRUE, error_fold = TRUE)
            
            expect_equal(length(par.nsp.res$error_fold[[1]]), 2)
          })

# par_mode = "apply" variable importance Tue Feb 21 22:15:41 2017 ------------------------------

test_that("par_mode = 'apply' works with var.imp", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 4),
                       par_args = list(par_mode = "apply", par_units = 2),
                       benchmark = TRUE,
                       importance = TRUE, imp_permutations = 2)
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc") # check for train.auroc for binary response
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame") # check for importance object
})

# sperrorest warnings Thu Feb  9 22:34:08 2017 

test_that("importance = T and err.fold = F", { 
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                            model_fun = glm,
                            pred_fun = predict,
                            smp_fun = partition_cv, 
                            smp_args = list(repetition = 1:2, nfold = 2),
                            par_args = list(par_mode = "apply", par_units = 2),
                            importance = TRUE, error_fold = FALSE))
})

# sperrorest depr. args Thu Feb  9 22:42:48 2017 

test_that("deprecated args", { 
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv, 
                          smp_args = list(repetition = 1:2, nfold = 2),
                          predfun = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv, 
                          smp_args = list(repetition = 1:2, nfold = 2),
                          silent = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv, 
                          smp_args = list(repetition = 1:2, nfold = 2),
                          err.pooled = NULL))
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model_fun = glm,
                          pred_fun = predict,
                          smp_fun = partition_cv, 
                          smp_args = list(repetition = 1:2, nfold = 2),
                          err.unpooled = NULL))
})

# partition.factor.cv mit custom pred_fun Sun Feb 19 09:36:26 2017 

test_that("partition.factor.cv works (LDA)", {
  
  skip_on_cran()
  
  skip("par_mode = 'apply' does not work on LDA example") # only skip on macOS??
  
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
                               model_fun = lda,
                               pred_fun = lda.predfun,
                               pred_args = list(fac = "field"),
                               smp_fun = partition.factor.cv,
                               smp_args = list(fac = "field", repetition = 1:2, 
                                               nfold = 2),
                               par_args = list(par_units = 2, 
                                               par_mode = "apply"),
                               error_rep = TRUE, error_fold = TRUE, 
                               progress = "TRUE", benchmark = TRUE)
  
})

# par_mode = "sequential" Mon Feb  6 23:24:11 2017 --------------------------

test_that("output type (= list) for different logical combinations of error_rep 
          and error_fold for par_mode = 'sequential' on LDA example", {
            
            skip_on_cran()
            
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
            
            # err.rep = TRUE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 4),
                              error_rep = TRUE, error_fold = TRUE,
                              benchmark = TRUE, progress = T)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "list")
            # check that train.error is first
            expect_equal(names(out$error_rep)[[1]], "train.error") 
            
            # err.rep = TRUE, err.fold = FALSE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 5),
                              par_args = list(par_mode = "sequential"),
                              error_rep = TRUE, error_fold = FALSE,
                              benchmark = TRUE, progress = TRUE)
            
            expect_equal(typeof(out$error_rep), "list")
            expect_equal(typeof(out$error_fold), "NULL")
            
            # err.rep = FALSE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                              model_fun = lda,
                              pred_fun = lda.predfun,
                              smp_fun = partition_cv,
                              smp_args = list(repetition = 1:2, nfold = 2),
                              par_args = list(par_mode = "sequential"),
                              error_rep = FALSE, error_fold = TRUE,
                              benchmark = TRUE, progress = FALSE)
            
            expect_equal(typeof(out$error_rep), "NULL")
            expect_equal(typeof(out$error_fold), "list")
          })


test_that("output length of list is correct for error_rep = TRUE and 
          error_fold = TRUE for par_mode = 'sequential' on rpart example", {
            
            skip_on_cran()
            
            data(ecuador)
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            mypred.rpart <- function(object, newdata) predict(object, 
                                                              newdata)[, 2]
            ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
            fit <- rpart(fo, data = ecuador, control = ctrl)
            
            # Non-spatial 5-repeated 10-fold cross-validation:
            mypred.rpart <- function(object, newdata) predict(object, 
                                                              newdata)[,2]
            par.nsp.res <- sperrorest(data = ecuador, formula = fo,
                                      model_fun = rpart, 
                                      model_args = list(control = ctrl),
                                      pred_fun = mypred.rpart,
                                      progress = FALSE,
                                      smp_fun = partition_cv,
                                      smp_args = list(repetition = 1:2, 
                                                      nfold = 2),
                                      par_args = list(par_mode = "sequential"),
                                      error_rep = TRUE, error_fold = TRUE)
            
            expect_equal(length(par.nsp.res$error_fold[[1]]), 2)
          })

# variable importance Wed Feb  8 21:59:03 2017 

test_that("sperrorest() variable importance with error_rep = T and 
          error_fold = T", {
            
            skip_on_cran()
            
            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm, 
                                 model_args = list(family = "binomial"),
                                 pred_fun = predict, 
                                 pred_args = list(type = "response"),
                                 smp_fun = partition_cv,
                                 smp_args = list(repetition = 1:2, nfold = 4),
                                 par_args = list(par_mode = "sequential"),
                                 benchmark = TRUE, 
                                 importance = TRUE, imp_permutations = 10)
            expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
          })

test_that("sperrorest() variable importance with error_rep = F and 
          error_fold = T", {
            
            skip_on_cran()
            
            data(ecuador) # Muenchow et al. (2012), see ?ecuador
            fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
            
            nspres <- sperrorest(data = ecuador, formula = fo,
                                 model_fun = glm, 
                                 model_args = list(family = "binomial"),
                                 pred_fun = predict, 
                                 pred_args = list(type = "response"),
                                 smp_fun = partition_cv,
                                 smp_args = list(repetition = 1:2, nfold = 4),
                                 par_args = list(par_mode = "sequential"),
                                 benchmark = TRUE, error_rep = FALSE, 
                                 importance = TRUE, imp_permutations = 10)
            expect_equal(class(nspres$importance[[1]][[1]]), "data.frame")
          })

# binary response Wed Feb  8 22:43:12 2017 

test_that("sperrorest() produces correct output for binary response", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_fun = predict, pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 2),
                       par_args = list(par_mode = "sequential"),
                       benchmark = TRUE, 
                       importance = FALSE, imp_permutations = 2)
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  # check for train.auroc for binary response
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc") 
})

test_that("sperrorest() when pred_fun = NULL", {
  
  skip_on_cran()
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model_fun = glm, model_args = list(family = "binomial"),
                       pred_args = list(type = "response"),
                       smp_fun = partition_cv,
                       smp_args = list(repetition = 1:2, nfold = 4),
                       par_args = list(par_mode = "sequential"),
                       benchmark = TRUE,
                       importance = TRUE, imp_permutations = 10)
  summary.rep <- summary(nspres$error_rep)
  summary.fold <- summary(nspres$error_fold)
  summary.resampling <- summary(nspres$represampling)
  summary.impo <- summary(nspres$importance)
  # check for train.auroc for binary response
  expect_equal(names(nspres$error_rep)[[1]], "train.auroc") 
  # check for importance object
  expect_equal(class(nspres$importance[[1]][[1]]), "data.frame") 
})

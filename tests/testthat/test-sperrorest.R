context("sperrorest.R")

pacman::p_load(sperrorest, rpart, testthat, MASS)

# sperrorest() binary response Wed Feb  8 21:40:49 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm, model.args = list(family = "binomial"),
                       pred.fun = predict, pred.args = list(type = "response"),
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       benchmark = TRUE,
                       importance = TRUE, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)                    
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
  expect_equal(length(summary.resampling), 2) # resampling summary
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
  expect_equal(names(nspres$error.rep)[[1]], "train.auroc") # check for auroc existence
})


# sperorrest() continuous response Wed Feb  8 22:19:57 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       benchmark = TRUE, 
                       importance = F, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)                    
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
  expect_equal(length(summary.resampling), 2) # resampling summary
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
  expect_equal(names(nspres$error.rep)[[1]], "train.bias") # check for bias existence
})

# pred.fun = NULL response Wed Feb  8 22:19:57 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       importance = TRUE, imp.permutations = 2)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
})


# summary.sperroresterror() Thu Feb  9 22:10:15 2017 ------------------------------

test_that("summary.sperroresterror() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2))
  
  summary.rep1 <- summary(nspres$error.rep, pooled = FALSE)                    
  summary.fold1 <- summary(nspres$error.fold, pooled = FALSE)
  summary.rep <- summary(nspres$error.rep, pooled = TRUE)                    
  summary.fold <- summary(nspres$error.fold, pooled = TRUE)
  
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
})

# summary.sperrorestimportance() Thu Feb  9 22:17:15 2017 ------------------------------

test_that("summary.sperroresterror() with pooled = FALSE produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       importance = TRUE, imp.permutations = 2,
                       do.try = TRUE)
  
  summary.imp <- summary(nspres$importance)        
  
  expect_equal(length(summary.imp), 28)
})

# sperrorest warnings Thu Feb  9 22:34:08 2017 ------------------------------

test_that("importance = T and err.fold = F", { 
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                            model.fun = glm,
                            pred.fun = predict,
                            smp.fun = partition.cv, 
                            smp.args = list(repetition = 1:2, nfold = 2),
                            importance = TRUE, error.fold = FALSE))
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          someargument = NULL))
})

# sperrorest depr. args Thu Feb  9 22:42:48 2017 ------------------------------

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

# sperrorest() various non default arguments Fri Feb 10 19:09:01 2017 ------------------------------

# does not work with err.train = F and importance = T

test_that("sperrorest() produces correct output for binary response for non-default arguments", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm, model.args = list(family = "binomial"),
                       pred.fun = predict, pred.args = list(type = "response"),
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       benchmark = F,
                       importance = TRUE, imp.permutations = 2,
                       do.try = TRUE, err.train = T, do.gc = 2)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
})


# summary.sperrorest() Sun Feb 12 11:56:13 2017 ------------------------------

test_that("summary.sperrorest() works correctly", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  out <- sperrorest(data = ecuador, formula = fo,
                    model.fun = glm,
                    pred.fun = predict,
                    smp.fun = partition.cv, 
                    smp.args = list(repetition = 1:2, nfold = 2),
                    importance = T, imp.permutations = 2,
                    error.fold = T, 
                    benchmark = T)
  
  smry.out <- summary(out)
  
  expect_equal(length(smry.out), 6)
})

# sperrorest() error.rep = F & do.try = T Sun Feb 12 23:05:30 2017 ------------------------------

test_that("sperrorest() error.rep = F & do.try = T", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  out <- sperrorest(data = ecuador, formula = fo,
                    model.fun = glm,
                    pred.fun = predict,
                    smp.fun = partition.cv, 
                    smp.args = list(repetition = 1:2, nfold = 2),
                    error.rep = F, do.try = T)
  
  smry.out <- summary(out)
  
  expect_equal(length(smry.out), 6)
})



test_that("is.factor.prediction object for classification models", {
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
            
            data(maipo)
            
            # err.rep = TRUE, err.fold = TRUE
            out <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"),
                                 model.fun = lda,
                                 pred.fun = lda.predfun,
                                 smp.fun = partition.cv,
                                 smp.args = list(repetition = 1:2, nfold = 2),
                                 error.rep = TRUE, error.fold = TRUE,
                                 benchmark = FALSE, progress = FALSE)
            
            smry.out <- summary(out)
            
            expect_equal(length(smry.out), 6)
          })

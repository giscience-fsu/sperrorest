context("parsperrorest.R")

pacman::p_load(sperrorest, testthat, rpart, MASS) 

# runfolds Sun May 21 22:58:39 2017 ------------------------------

testthat::test_that("runfolds works on glm example", {
  
  j <- 1 # running the first repetition of 'currentSample', normally we are 
  # calling an apply call to seq_along nFolds of repetition
  # see also 'runreps()'
  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")[[1]]
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
  runfolds_single <- runfolds(j = 1, data = ecuador, currentSample = currentSample,
                              formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
                              model.args = list(family = "binomial"), do.try = FALSE, model.fun = glm,
                              error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, 
                              imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
                              err.train = TRUE, importance = TRUE, currentRes = currentRes, 
                              pred.args = list(type = "response"), response = "slides", par.cl = 2, 
                              coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                              pooled.obs.test = c(), err.fun = err.default)
  expect_equal(length(runfolds_single), 6)
})

testthat::test_that("runfolds works on LDA example", {
  
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")[[1]]
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
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
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))
  
  runfolds_single <- runfolds(j = 1, data = maipo, currentSample = currentSample,
                              formula = fo, 
                              do.try = FALSE, model.fun = lda, pred.fun = lda.predfun,
                              error.fold = TRUE, error.rep = TRUE, pred.args = list(fac = "field"),
                              err.train = TRUE, importance = FALSE, currentRes = currentSample, 
                              response = "croptype", par.cl = 2, 
                              coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                              pooled.obs.test = c(), err.fun = err.default)
  expect_equal(length(runfolds_single), 6)
})


testthat::test_that("runfolds works on rpart example", {
  
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")[[1]]
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  mypred.rpart <- function(object, newdata) predict(object, newdata)[, 2]
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  
  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred.rpart <- function(object, newdata) predict(object, newdata)[,2]
  
  runfolds_single <- runfolds(j = 1, data = ecuador, currentSample = currentSample,
                              formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
                              do.try = FALSE, model.fun = rpart,
                              error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, pred.fun = mypred.rpart, 
                              model.args = list(control = ctrl),
                              imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
                              err.train = TRUE, importance = TRUE, currentRes = currentRes, 
                              response = "slides", par.cl = 2, 
                              coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                              pooled.obs.test = c(), err.fun = err.default)
  expect_equal(length(runfolds_single), 6)
})

# runreps Sun May 21 23:07:03 2017 ------------------------------

testthat::test_that("runreps works on lda example", { 
  
  j <- 1 # running the first repetition of 'currentSample', normally we are 
  # calling an apply call to seq_along nFolds of repetition
  # see also 'runreps()'
  #### 2 repetitions, 4 folds
  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
  currentSample <- readRDS("inst/test-objects/resamp_maipo.rda")
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
  
  predictors <- colnames(maipo)[5:ncol(maipo)]
  fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))
  
  runreps_res <- lapply(currentSample, function(X) runreps(currentSample = X, data = maipo,
                                                           formula = fo, par.mode = 1, pred.fun = lda.predfun,
                                                           do.try = FALSE, model.fun = lda,
                                                           error.fold = TRUE, error.rep = TRUE, do.gc = 1,
                                                           err.train = TRUE, importance = FALSE, currentRes = currentRes, 
                                                           pred.args = list(fac = "field"), response = "croptype", par.cl = 2, 
                                                           coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                                                           pooled.obs.test = c(), err.fun = err.default))
})

testthat::test_that("runreps works on glm example", { 
  
  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
  runreps_res <- lapply(currentSample, function(X) runreps(currentSample = X, data = ecuador,
                                                           formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
                                                           model.args = list(family = "binomial"), do.try = FALSE, model.fun = glm,
                                                           error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, do.gc = 1,
                                                           imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
                                                           err.train = TRUE, importance = TRUE, currentRes = currentRes, 
                                                           pred.args = list(type = "response"), response = "slides", par.cl = 2, 
                                                           coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                                                           pooled.obs.test = c(), err.fun = err.default))
})

testthat::test_that("runreps works on rpart example", { 
  
  data <- ecuador
  imp.one.rep <- readRDS("inst/test-objects/imp.one.rep.rda")
  currentSample <- readRDS("inst/test-objects/resamp.rda")
  currentRes <- readRDS("inst/test-objects/currentRes.rda")
  
  runreps_res <- lapply(currentSample, function(X) runreps(currentSample = X, data = ecuador,
                                                           formula = slides ~ dem + slope + hcurv + vcurv + log.carea + cslope, 
                                                           do.try = FALSE, model.fun = rpart,
                                                           error.fold = TRUE, error.rep = TRUE, imp.permutations = 2, pred.fun = mypred.rpart, 
                                                           model.args = list(control = ctrl),
                                                           imp.variables = c("dem", "slope", "hcurv", "vcurv", "log.carea", "cslope"),
                                                           err.train = TRUE, importance = TRUE, currentRes = currentRes, 
                                                           response = "slides", par.cl = 2, 
                                                           coords = c("x", "y"), progress = 1, pooled.obs.train = c(), 
                                                           pooled.obs.test = c(), err.fun = err.default))
  
})

